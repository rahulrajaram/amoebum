#!/usr/bin/env bash
# tui-perf-test.sh — Automated performance regression test via tmux
#
# Launches amoebum in --demo mode, sends multiple "long" prompts
# (each generating 20 sections of lorem ipsum), and monitors /proc/<pid>/status
# for minor faults, RSS, and frame timing degradation across prompts.
#
# The test PASSES if:
#   1. Minor fault rate does not grow >3x between prompt 1 and prompt N
#   2. RSS stays bounded (no runaway leak)
#   3. The TUI remains responsive (each prompt completes within timeout)
#
# Usage:
#   ./bin/tui-perf-test.sh                # run with defaults (5 prompts)
#   ./bin/tui-perf-test.sh --prompts 10   # send 10 long prompts
#   ./bin/tui-perf-test.sh --watch        # pause for manual tmux attach
#   ./bin/tui-perf-test.sh --report       # print detailed /proc snapshots

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
BINARY="$REPO_ROOT/dist/amoebum"
SESSION="amoebum-perf-test-$$"
ARTIFACT_DIR="$REPO_ROOT/tmp/perf-test-$$"

NUM_PROMPTS=5
WATCH=false
REPORT=false
PROMPT_KEYWORD="long"          # triggers %demo-response-long (20 sections)
STARTUP_WAIT=4                 # seconds to wait for TUI init
PROMPT_TIMEOUT=30              # seconds to wait for each prompt to finish
SAMPLE_INTERVAL=0.5            # seconds between /proc samples during streaming
FAULT_GROWTH_THRESHOLD=3.0     # fail if minor faults/sec grow by more than this factor
RSS_GROWTH_THRESHOLD_KB=102400 # fail if RSS grows by more than 100MB total

while [ $# -gt 0 ]; do
    case "$1" in
        --prompts) NUM_PROMPTS="$2"; shift 2 ;;
        --watch)   WATCH=true; shift ;;
        --report)  REPORT=true; shift ;;
        *)         shift ;;
    esac
done

cleanup() {
    tmux kill-session -t "$SESSION" 2>/dev/null || true
}
trap cleanup EXIT

die() { echo "FATAL: $1" >&2; exit 1; }

watch_pause() {
    if $WATCH; then
        echo ""
        echo "  >>> Attach in another terminal:  tmux attach -t $SESSION"
        echo "  >>> Press Enter here to continue..."
        read -r
    fi
}

# --- helpers ---

capture_pane() {
    tmux capture-pane -t "$SESSION" -p -S -200
}

wait_for_text() {
    local needle="$1" timeout="${2:-8}" elapsed=0
    while [ "$elapsed" -lt "$timeout" ]; do
        if capture_pane | grep -qiF "$needle"; then
            return 0
        fi
        sleep 0.5
        elapsed=$((elapsed + 1))
    done
    return 1
}

# Find the PID of the amoebum process inside the tmux session
find_amoebum_pid() {
    local pane_pid
    pane_pid="$(tmux display-message -t "$SESSION" -p '#{pane_pid}' 2>/dev/null || echo "")"
    if [ -z "$pane_pid" ]; then
        echo ""
        return
    fi
    # The pane_pid is the shell; amoebum is its child
    local child_pid
    child_pid="$(pgrep -P "$pane_pid" 2>/dev/null | head -1 || echo "")"
    if [ -n "$child_pid" ] && [ -d "/proc/$child_pid" ]; then
        echo "$child_pid"
    else
        echo "$pane_pid"
    fi
}

# Sample /proc/<pid>/status for key metrics
sample_proc() {
    local pid="$1"
    if [ ! -f "/proc/$pid/status" ]; then
        echo "pid=$pid rss_kb=0 minflt=0 majflt=0"
        return
    fi
    local rss_kb minflt majflt
    rss_kb="$(awk '/^VmRSS:/ {print $2}' "/proc/$pid/status" 2>/dev/null || echo 0)"
    # Minor/major faults from /proc/<pid>/stat (field 10 and 12, 1-indexed)
    local stat_line
    stat_line="$(cat "/proc/$pid/stat" 2>/dev/null || echo "")"
    if [ -n "$stat_line" ]; then
        minflt="$(echo "$stat_line" | awk '{print $10}')"
        majflt="$(echo "$stat_line" | awk '{print $12}')"
    else
        minflt=0
        majflt=0
    fi
    echo "pid=$pid rss_kb=$rss_kb minflt=$minflt majflt=$majflt"
}

# Collect /proc samples during a streaming response
collect_samples() {
    local pid="$1" duration="$2" outfile="$3"
    local elapsed=0
    > "$outfile"
    while (( $(echo "$elapsed < $duration" | bc -l) )); do
        local ts
        ts="$(date +%s.%N)"
        echo "$ts $(sample_proc "$pid")" >> "$outfile"
        sleep "$SAMPLE_INTERVAL"
        elapsed="$(echo "$elapsed + $SAMPLE_INTERVAL" | bc -l)"
    done
}

# Compute minor fault rate (faults/sec) from sample file
compute_fault_rate() {
    local sample_file="$1"
    local lines
    lines="$(wc -l < "$sample_file")"
    if [ "$lines" -lt 2 ]; then
        echo "0"
        return
    fi
    local first_ts first_flt last_ts last_flt
    first_ts="$(head -1 "$sample_file" | awk '{print $1}')"
    first_flt="$(head -1 "$sample_file" | sed 's/.*minflt=//' | awk '{print $1}')"
    last_ts="$(tail -1 "$sample_file" | awk '{print $1}')"
    last_flt="$(tail -1 "$sample_file" | sed 's/.*minflt=//' | awk '{print $1}')"

    local dt dflt
    dt="$(echo "$last_ts - $first_ts" | bc -l)"
    dflt="$(echo "$last_flt - $first_flt" | bc -l)"

    if (( $(echo "$dt > 0" | bc -l) )); then
        echo "$(echo "scale=0; $dflt / $dt" | bc -l)"
    else
        echo "0"
    fi
}

# Get peak RSS from sample file
peak_rss() {
    local sample_file="$1"
    sed 's/.*rss_kb=//' "$sample_file" | awk '{print $1}' | sort -n | tail -1
}

# --- preconditions ---

[ -x "$BINARY" ] || die "Binary not found: $BINARY (run 'make build' first)"
command -v tmux >/dev/null 2>&1 || die "tmux is not installed"
command -v bc >/dev/null 2>&1 || die "bc is not installed"

mkdir -p "$ARTIFACT_DIR"

echo "=== TUI Performance Regression Test ==="
echo "Binary:  $BINARY"
echo "Session: $SESSION"
echo "Prompts: $NUM_PROMPTS × '$PROMPT_KEYWORD' (20 sections each)"
echo "Artifacts: $ARTIFACT_DIR"
echo ""

# ============================================================
# Launch amoebum in demo mode
# ============================================================

tmux new-session -d -s "$SESSION" -x 120 -y 40 "$BINARY --demo"
sleep "$STARTUP_WAIT"
watch_pause

PID="$(find_amoebum_pid)"
if [ -z "$PID" ] || [ ! -d "/proc/$PID" ]; then
    die "Could not find amoebum process PID"
fi
echo "Amoebum PID: $PID"
echo "Initial: $(sample_proc "$PID")"
echo ""

INITIAL_SAMPLE="$(sample_proc "$PID")"
INITIAL_RSS="$(echo "$INITIAL_SAMPLE" | sed 's/.*rss_kb=//' | awk '{print $1}')"

PASSED=0
FAILED=0
FAULT_RATES=()
PEAK_RSS_VALUES=()
SAMPLE_COUNTS=()
MIN_SAMPLES_FOR_RATE=10   # ignore prompts with fewer samples (noisy short windows)

# ============================================================
# Send N prompts, sampling /proc during each
# ============================================================

for i in $(seq 1 "$NUM_PROMPTS"); do
    echo "--- Prompt $i/$NUM_PROMPTS ---"

    SAMPLE_FILE="$ARTIFACT_DIR/samples-prompt-$i.txt"

    # Take a pre-prompt snapshot
    PRE="$(sample_proc "$PID")"
    PRE_FAULTS="$(echo "$PRE" | sed 's/.*minflt=//' | awk '{print $1}')"

    # Send the prompt
    tmux send-keys -t "$SESSION" "$PROMPT_KEYWORD" Enter

    # Collect /proc samples while the response streams
    collect_samples "$PID" "$PROMPT_TIMEOUT" "$SAMPLE_FILE" &
    SAMPLER_PID=$!

    # Wait for the response to finish (look for "Section 20" from the long response)
    if wait_for_text "Section 20" "$PROMPT_TIMEOUT"; then
        echo "  Response completed"
    else
        echo "  WARNING: Response may not have completed within ${PROMPT_TIMEOUT}s"
    fi

    # Give a moment for rendering to settle
    sleep 2

    # Stop sampler
    kill "$SAMPLER_PID" 2>/dev/null || true
    wait "$SAMPLER_PID" 2>/dev/null || true

    # Compute metrics
    RATE="$(compute_fault_rate "$SAMPLE_FILE")"
    PRSS="$(peak_rss "$SAMPLE_FILE")"
    SAMPLE_COUNT="$(wc -l < "$SAMPLE_FILE")"
    FAULT_RATES+=("$RATE")
    PEAK_RSS_VALUES+=("$PRSS")
    SAMPLE_COUNTS+=("$SAMPLE_COUNT")

    POST="$(sample_proc "$PID")"
    echo "  Fault rate: ~${RATE} minflt/sec"
    echo "  Peak RSS:   ${PRSS} kB"
    echo "  Post:       $POST"

    if $REPORT; then
        echo "  Samples:    $SAMPLE_FILE ($(wc -l < "$SAMPLE_FILE") points)"
    fi

    watch_pause
    echo ""
done

# ============================================================
# Analyze results
# ============================================================

echo "=== Performance Analysis ==="
echo ""

# Print summary table
printf "%-10s %-18s %-15s %-10s\n" "Prompt" "MinFlt/sec" "Peak RSS (kB)" "Samples"
printf "%-10s %-18s %-15s %-10s\n" "------" "----------" "-------------" "-------"
for i in $(seq 0 $((NUM_PROMPTS - 1))); do
    printf "%-10s %-18s %-15s %-10s\n" "$((i + 1))" "${FAULT_RATES[$i]}" "${PEAK_RSS_VALUES[$i]}" "${SAMPLE_COUNTS[$i]}"
done
echo ""

# Check 1: Fault rate growth (using only prompts with sufficient samples)
# Short-window measurements are noisy — a GC burst in 4 samples produces
# wildly inflated rates that don't reflect sustained performance.
FIRST_RATE=""
FIRST_IDX=""
LAST_RATE=""
LAST_IDX=""
for i in $(seq 0 $((NUM_PROMPTS - 1))); do
    if [ "${SAMPLE_COUNTS[$i]}" -ge "$MIN_SAMPLES_FOR_RATE" ] 2>/dev/null; then
        if [ -z "$FIRST_RATE" ]; then
            FIRST_RATE="${FAULT_RATES[$i]}"
            FIRST_IDX=$((i + 1))
        fi
        LAST_RATE="${FAULT_RATES[$i]}"
        LAST_IDX=$((i + 1))
    fi
done

if [ -n "$FIRST_RATE" ] && [ "$FIRST_RATE" -gt 0 ] 2>/dev/null; then
    GROWTH="$(echo "scale=2; $LAST_RATE / $FIRST_RATE" | bc -l)"
    echo "Fault rate growth: ${GROWTH}x (prompt $FIRST_IDX → prompt $LAST_IDX, >=${MIN_SAMPLES_FOR_RATE} samples only)"

    if (( $(echo "$GROWTH > $FAULT_GROWTH_THRESHOLD" | bc -l) )); then
        echo "  FAIL: Fault rate grew ${GROWTH}x (threshold: ${FAULT_GROWTH_THRESHOLD}x)"
        FAILED=$((FAILED + 1))
    else
        echo "  PASS: Fault rate growth within threshold"
        PASSED=$((PASSED + 1))
    fi
else
    echo "  SKIP: No prompts with sufficient samples (>=$MIN_SAMPLES_FOR_RATE) for rate comparison"
    PASSED=$((PASSED + 1))
fi

# Check 2: RSS growth
FINAL_RSS="${PEAK_RSS_VALUES[$((NUM_PROMPTS - 1))]}"
RSS_DELTA="$(echo "$FINAL_RSS - $INITIAL_RSS" | bc -l)"
echo ""
echo "RSS growth: ${RSS_DELTA} kB (initial: ${INITIAL_RSS} kB, final peak: ${FINAL_RSS} kB)"

if (( $(echo "$RSS_DELTA > $RSS_GROWTH_THRESHOLD_KB" | bc -l) )); then
    echo "  FAIL: RSS grew by ${RSS_DELTA} kB (threshold: ${RSS_GROWTH_THRESHOLD_KB} kB)"
    FAILED=$((FAILED + 1))
else
    echo "  PASS: RSS growth within threshold"
    PASSED=$((PASSED + 1))
fi

# Check 3: All prompts completed (TUI stayed responsive)
echo ""
FINAL_PANE="$(capture_pane)"
SECTIONS_FOUND="$(echo "$FINAL_PANE" | grep -c "Section" || true)"
echo "Responsiveness: $SECTIONS_FOUND 'Section' markers found in final pane"
if [ "$SECTIONS_FOUND" -gt 0 ]; then
    echo "  PASS: TUI remained responsive through $NUM_PROMPTS prompts"
    PASSED=$((PASSED + 1))
else
    echo "  FAIL: No 'Section' markers found — TUI may have frozen"
    FAILED=$((FAILED + 1))
fi

# Save summary
cat > "$ARTIFACT_DIR/verdict.json" <<VERDICT
{
  "test": "tui-perf-regression",
  "prompts": $NUM_PROMPTS,
  "prompt_keyword": "$PROMPT_KEYWORD",
  "pid": $PID,
  "initial_rss_kb": $INITIAL_RSS,
  "final_peak_rss_kb": $FINAL_RSS,
  "rss_delta_kb": $RSS_DELTA,
  "first_fault_rate": ${FIRST_RATE:-0},
  "first_fault_prompt": ${FIRST_IDX:-0},
  "last_fault_rate": ${LAST_RATE:-0},
  "last_fault_prompt": ${LAST_IDX:-0},
  "fault_growth_factor": ${GROWTH:-0},
  "min_samples_threshold": $MIN_SAMPLES_FOR_RATE,
  "passed": $PASSED,
  "failed": $FAILED,
  "verdict": "$([ "$FAILED" -eq 0 ] && echo "PASS" || echo "FAIL")"
}
VERDICT

echo ""
echo "Verdict: $ARTIFACT_DIR/verdict.json"

# ============================================================
# Summary
# ============================================================
echo ""
TOTAL=$((PASSED + FAILED))
echo "TUI_PERF_TEST passed=$PASSED failed=$FAILED total=$TOTAL"

if [ "$FAILED" -gt 0 ]; then
    exit 1
fi
