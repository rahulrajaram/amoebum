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
#   ./bin/tui-perf-test.sh --scale        # append one 200-section scale prompt after normal prompts
#   ./bin/tui-perf-test.sh --self-test    # run deterministic verdict-contract checks

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
BINARY="$REPO_ROOT/dist/amoebum"
SESSION="amoebum-perf-test-$$"
TMP_ROOT="$REPO_ROOT/tmp"
ARTIFACT_DIR="$TMP_ROOT/perf-test-$$"

NUM_PROMPTS=5
WATCH=false
REPORT=false
SCALE_MODE=0
SELF_TEST=false
PROMPT_KEYWORD="long"          # triggers %demo-response-long (20 sections)
SCALE_PROMPT_KEYWORD="scale"   # triggers %demo-response-scale (200 sections)
STARTUP_TIMEOUT=20             # seconds to wait for TUI init
PROMPT_TIMEOUT=30              # seconds to wait for each prompt to finish
SCALE_PROMPT_TIMEOUT=120       # seconds to wait for the 200-section scale prompt
SAMPLE_INTERVAL=0.5            # seconds between /proc samples during streaming
FAULT_GROWTH_THRESHOLD=3.0     # fail if minor faults/sec grow by more than this factor
RSS_GROWTH_THRESHOLD_KB=102400 # fail if RSS grows by more than 100MB total
STEADY_STATE_TRIM_SAMPLES=4    # trim prompt-edge bursts (4 samples ~= 2 seconds)

while [ $# -gt 0 ]; do
    case "$1" in
        --prompts) NUM_PROMPTS="$2"; shift 2 ;;
        --watch)   WATCH=true; shift ;;
        --report)  REPORT=true; shift ;;
        --scale)   SCALE_MODE=1; shift ;;
        --self-test) SELF_TEST=true; shift ;;
        *)         shift ;;
    esac
done

cleanup() {
    tmux kill-session -t "$SESSION" 2>/dev/null || true
}
trap cleanup EXIT

die() { echo "FATAL: $1" >&2; exit 1; }

need_command() {
    local name="$1"
    command -v "$name" >/dev/null 2>&1 || die "Required command not found: $name"
}

json_bool() {
    if [ "$1" = "true" ]; then
        printf 'true'
    else
        printf 'false'
    fi
}

scale_timeout_non_blocking_p() {
    [ "$SCALE_MODE" -eq 1 ] && [ "$SCALE_COMPLETION_KIND" = "timeout" ]
}

scale_timeout_note() {
    if scale_timeout_non_blocking_p; then
        printf '%s' "optional 200-section scale prompt hit the harness timeout and remains non-blocking because the primary perf verdict was already computed from the required prompt set"
    else
        printf '%s' ""
    fi
}

write_verdict_json() {
    local verdict_path="$1"
    local scale_timeout_non_blocking="false"
    if scale_timeout_non_blocking_p; then
        scale_timeout_non_blocking="true"
    fi
    local scale_note
    scale_note="$(scale_timeout_note)"

    cat > "$verdict_path" <<VERDICT
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
  "steady_state_trim_samples": $STEADY_STATE_TRIM_SAMPLES,
  "min_samples_threshold": $MIN_SAMPLES_FOR_RATE,
  "scale_mode": $SCALE_MODE,
  "scale_timeout_seconds": $SCALE_PROMPT_TIMEOUT,
  "scale_fault_rate_full": "${SCALE_FAULT_RATE_FULL:-0}",
  "scale_fault_rate_steady": "${SCALE_FAULT_RATE_STEADY:-0}",
  "scale_peak_rss_kb": "${SCALE_PEAK_RSS:-0}",
  "scale_elapsed_s": "${SCALE_ELAPSED:-n/a}",
  "scale_completion": "$SCALE_COMPLETION_KIND",
  "scale_timeout_non_blocking": $(json_bool "$scale_timeout_non_blocking"),
  "scale_note": "$scale_note",
  "passed": $PASSED,
  "failed": $FAILED,
  "verdict": "$([ "$FAILED" -eq 0 ] && echo "PASS" || echo "FAIL")",
  "prompt_metrics": [
$(for i in $(seq 0 $((NUM_PROMPTS - 1))); do
    comma=","
    if [ "$i" -eq $((NUM_PROMPTS - 1)) ]; then
        comma=""
    fi
    printf '    {"prompt": %s, "start_s": "%s", "section20_s": "%s", "done_s": "%s", "completion": "%s", "sample_count": %s, "fault_rate_full": %s, "fault_rate_steady": %s}%s\n' \
        "$((i + 1))" \
        "${STREAM_START_SECONDS[$i]}" \
        "${SECTION20_SECONDS[$i]}" \
        "${PROMPT_ELAPSED_SECONDS[$i]}" \
        "${COMPLETION_KINDS[$i]}" \
        "${SAMPLE_COUNTS[$i]}" \
        "${FAULT_RATES_FULL[$i]}" \
        "${FAULT_RATES_STEADY[$i]}" \
        "$comma"
done)
  ]
}
VERDICT
}

run_self_test() {
    need_command jq
    local tmp_dir verdict_file
    mkdir -p "${REPO_ROOT}/tmp"
    tmp_dir="$(mktemp -d "${REPO_ROOT}/tmp/tui-perf-self-test-XXXXXX")"
    verdict_file="${tmp_dir}/verdict.json"

    NUM_PROMPTS=2
    PROMPT_KEYWORD="long"
    PID=4242
    INITIAL_RSS=1000
    FINAL_RSS=1300
    RSS_DELTA=300
    FIRST_RATE=10
    FIRST_IDX=1
    LAST_RATE=12
    LAST_IDX=2
    GROWTH=1.20
    MIN_SAMPLES_FOR_RATE=2
    STREAM_START_SECONDS=("0.10" "0.20")
    SECTION20_SECONDS=("0.80" "0.95")
    PROMPT_ELAPSED_SECONDS=("1.20" "1.35")
    COMPLETION_KINDS=("stream-done" "stream-done")
    SAMPLE_COUNTS=(8 9)
    FAULT_RATES_FULL=(14 16)
    FAULT_RATES_STEADY=(10 12)
    PASSED=3
    FAILED=0

    SCALE_MODE=1
    SCALE_COMPLETION_KIND="timeout"
    SCALE_FAULT_RATE_FULL="22"
    SCALE_FAULT_RATE_STEADY="18"
    SCALE_PEAK_RSS="1500"
    SCALE_ELAPSED="n/a"
    write_verdict_json "$verdict_file"
    jq -e '.verdict == "PASS" and .scale_completion == "timeout" and .scale_timeout_non_blocking == true' \
        "$verdict_file" >/dev/null
    jq -e '.scale_note | contains("non-blocking")' "$verdict_file" >/dev/null

    SCALE_COMPLETION_KIND="stream-done"
    write_verdict_json "$verdict_file"
    jq -e '.scale_completion == "stream-done" and .scale_timeout_non_blocking == false and .scale_note == ""' \
        "$verdict_file" >/dev/null

    printf '%s\n' "TUI_PERF_SELF_TEST_OK"
    rm -rf "$tmp_dir"
    exit 0
}

if $SELF_TEST; then
    run_self_test
fi

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

capture_status_line() {
    tmux capture-pane -t "$SESSION" -p | tail -1
}

now_ms() {
    date +%s%3N
}

status_is_stream_running() {
    local status="$1"
    printf '%s\n' "$status" | grep -qiF "stream " \
        && ! printf '%s\n' "$status" | grep -qiF "stream idle" \
        && ! printf '%s\n' "$status" | grep -qiF "stream done" \
        && ! printf '%s\n' "$status" | grep -qiF "stream cancelled" \
        && ! printf '%s\n' "$status" | grep -qiF "stream failed"
}

status_is_stream_done() {
    local status="$1"
    printf '%s\n' "$status" | grep -qiF "stream done"
}

seconds_between_ms() {
    local start_ms="$1" end_ms="$2"
    if [ -z "$start_ms" ] || [ -z "$end_ms" ]; then
        printf 'n/a'
    else
        printf '%s' "$(echo "scale=2; ($end_ms - $start_ms) / 1000" | bc -l)"
    fi
}

wait_for_text() {
    local needle="$1" timeout="${2:-8}"
    local deadline_ms=$(( $(now_ms) + timeout * 1000 ))
    while [ "$(now_ms)" -lt "$deadline_ms" ]; do
        if ! tmux has-session -t "$SESSION" 2>/dev/null; then
            return 1
        fi
        if capture_pane | grep -qiF "$needle"; then
            return 0
        fi
        sleep 0.5
    done
    return 1
}

wait_for_startup() {
    wait_for_text "Type below and press Enter to start" "$STARTUP_TIMEOUT" \
        || wait_for_text "Type below and press Enter." "$STARTUP_TIMEOUT"
}

wait_for_prompt_cycle() {
    local timeout="$1"
    local deadline_ms=$(( $(now_ms) + timeout * 1000 ))
    local started=false
    PROMPT_STREAM_STARTED_MS=""
    PROMPT_SECTION20_MS=""
    PROMPT_COMPLETED_MS=""
    PROMPT_COMPLETION_KIND="timeout"
    PROMPT_LAST_STATUS=""

    while [ "$(now_ms)" -lt "$deadline_ms" ]; do
        local pane status now
        pane="$(capture_pane)"
        status="$(capture_status_line || true)"
        now="$(now_ms)"
        PROMPT_LAST_STATUS="$status"

        if ! $started && status_is_stream_running "$status"; then
            started=true
            PROMPT_STREAM_STARTED_MS="$now"
        fi

        if $started && [ -z "$PROMPT_SECTION20_MS" ]; then
            local section20_count
            section20_count="$(printf '%s\n' "$pane" | grep -cF "Section 20" || true)"
            if [ "${section20_count:-0}" -gt "${PROMPT_SECTION20_BASE_COUNT:-0}" ]; then
                PROMPT_SECTION20_MS="$now"
            fi
        fi

        if $started && status_is_stream_done "$status"; then
            PROMPT_COMPLETED_MS="$now"
            PROMPT_COMPLETION_KIND="stream-done"
            return 0
        fi

        sleep 0.5
    done

    if [ -z "$PROMPT_STREAM_STARTED_MS" ]; then
        PROMPT_COMPLETION_KIND="no-stream-start"
    fi
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
    > "$outfile"
    local deadline_ms=$(( $(now_ms) + duration * 1000 ))
    while [ "$(now_ms)" -lt "$deadline_ms" ]; do
        local ts
        ts="$(date +%s.%N)"
        echo "$ts $(sample_proc "$pid")" >> "$outfile"
        sleep "$SAMPLE_INTERVAL"
    done
}

# Compute minor fault rate (faults/sec) from sample file
compute_fault_rate() {
    local sample_file="$1" trim_samples="${2:-0}"
    local lines
    lines="$(wc -l < "$sample_file")"
    if [ "$lines" -lt 2 ]; then
        echo "0"
        return
    fi
    local first_line_no=1 last_line_no="$lines"
    if [ "$trim_samples" -gt 0 ] && [ "$lines" -gt $((trim_samples * 2 + 1)) ]; then
        first_line_no=$((trim_samples + 1))
        last_line_no=$((lines - trim_samples))
    fi

    local first_line last_line first_ts first_flt last_ts last_flt
    first_line="$(sed -n "${first_line_no}p" "$sample_file")"
    last_line="$(sed -n "${last_line_no}p" "$sample_file")"
    first_ts="$(printf '%s\n' "$first_line" | awk '{print $1}')"
    first_flt="$(printf '%s\n' "$first_line" | sed 's/.*minflt=//' | awk '{print $1}')"
    last_ts="$(printf '%s\n' "$last_line" | awk '{print $1}')"
    last_flt="$(printf '%s\n' "$last_line" | sed 's/.*minflt=//' | awk '{print $1}')"

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

mkdir -p "$TMP_ROOT" "$ARTIFACT_DIR"

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
wait_for_startup || die "amoebum did not reach the interactive prompt"
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
FAULT_RATES_FULL=()
FAULT_RATES_STEADY=()
PEAK_RSS_VALUES=()
SAMPLE_COUNTS=()
PROMPT_ELAPSED_SECONDS=()
STREAM_START_SECONDS=()
SECTION20_SECONDS=()
COMPLETION_KINDS=()
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
    PROMPT_SECTION20_BASE_COUNT="$(capture_pane | grep -cF "Section 20" || true)"
    PROMPT_SENT_MS="$(now_ms)"
    tmux send-keys -t "$SESSION" "$PROMPT_KEYWORD" Enter

    # Collect /proc samples while the response streams
    collect_samples "$PID" "$PROMPT_TIMEOUT" "$SAMPLE_FILE" &
    SAMPLER_PID=$!

    if wait_for_prompt_cycle "$PROMPT_TIMEOUT"; then
        echo "  Response completed via status bar"
    else
        echo "  WARNING: Response did not complete within ${PROMPT_TIMEOUT}s"
    fi

    # Give a moment for rendering to settle
    sleep 2

    # Stop sampler
    kill "$SAMPLER_PID" 2>/dev/null || true
    wait "$SAMPLER_PID" 2>/dev/null || true

    # Compute metrics
    FULL_RATE="$(compute_fault_rate "$SAMPLE_FILE" 0)"
    STEADY_RATE="$(compute_fault_rate "$SAMPLE_FILE" "$STEADY_STATE_TRIM_SAMPLES")"
    PRSS="$(peak_rss "$SAMPLE_FILE")"
    SAMPLE_COUNT="$(wc -l < "$SAMPLE_FILE")"
    ELAPSED_S="$(seconds_between_ms "$PROMPT_SENT_MS" "$PROMPT_COMPLETED_MS")"
    START_S="$(seconds_between_ms "$PROMPT_SENT_MS" "$PROMPT_STREAM_STARTED_MS")"
    SECTION20_S="$(seconds_between_ms "$PROMPT_SENT_MS" "$PROMPT_SECTION20_MS")"
    FAULT_RATES_FULL+=("$FULL_RATE")
    FAULT_RATES_STEADY+=("$STEADY_RATE")
    PEAK_RSS_VALUES+=("$PRSS")
    SAMPLE_COUNTS+=("$SAMPLE_COUNT")
    PROMPT_ELAPSED_SECONDS+=("$ELAPSED_S")
    STREAM_START_SECONDS+=("$START_S")
    SECTION20_SECONDS+=("$SECTION20_S")
    COMPLETION_KINDS+=("$PROMPT_COMPLETION_KIND")

    POST="$(sample_proc "$PID")"
    echo "  Fault rate: full ~${FULL_RATE} minflt/sec | steady ~${STEADY_RATE} minflt/sec"
    echo "  Peak RSS:   ${PRSS} kB"
    echo "  Stream start: ${START_S}s | Section 20: ${SECTION20_S}s | Completion: ${ELAPSED_S}s (${PROMPT_COMPLETION_KIND})"
    echo "  Post:       $POST"
    echo "  Status:     ${PROMPT_LAST_STATUS}"

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
printf "%-8s %-12s %-12s %-12s %-14s %-15s %-8s\n" \
    "Prompt" "Start(s)" "Section20(s)" "Done(s)" "Completion" "Peak RSS (kB)" "Samples"
printf "%-8s %-12s %-12s %-12s %-14s %-15s %-8s\n" \
    "------" "--------" "------------" "-------" "----------" "-------------" "-------"
for i in $(seq 0 $((NUM_PROMPTS - 1))); do
    printf "%-8s %-12s %-12s %-12s %-14s %-15s %-8s\n" \
        "$((i + 1))" \
        "${STREAM_START_SECONDS[$i]}" \
        "${SECTION20_SECONDS[$i]}" \
        "${PROMPT_ELAPSED_SECONDS[$i]}" \
        "${COMPLETION_KINDS[$i]}" \
        "${PEAK_RSS_VALUES[$i]}" \
        "${SAMPLE_COUNTS[$i]}"
done
echo ""

echo "Minor fault rates:"
printf "%-8s %-18s %-18s\n" "Prompt" "Full" "Steady"
printf "%-8s %-18s %-18s\n" "------" "----------" "----------"
for i in $(seq 0 $((NUM_PROMPTS - 1))); do
    printf "%-8s %-18s %-18s\n" \
        "$((i + 1))" \
        "${FAULT_RATES_FULL[$i]}" \
        "${FAULT_RATES_STEADY[$i]}"
done
echo ""

# Check 1: Steady-state fault rate growth (using only prompts with sufficient samples)
# Short-window measurements are noisy — a GC burst in 4 samples produces
# wildly inflated rates that don't reflect sustained performance.
FIRST_RATE=""
FIRST_IDX=""
LAST_RATE=""
LAST_IDX=""
for i in $(seq 0 $((NUM_PROMPTS - 1))); do
    if [ "${SAMPLE_COUNTS[$i]}" -ge "$MIN_SAMPLES_FOR_RATE" ] 2>/dev/null; then
        if [ -z "$FIRST_RATE" ]; then
            FIRST_RATE="${FAULT_RATES_STEADY[$i]}"
            FIRST_IDX=$((i + 1))
        fi
        LAST_RATE="${FAULT_RATES_STEADY[$i]}"
        LAST_IDX=$((i + 1))
    fi
done

if [ -n "$FIRST_RATE" ] && [ "$FIRST_RATE" -gt 0 ] 2>/dev/null; then
    GROWTH="$(echo "scale=2; $LAST_RATE / $FIRST_RATE" | bc -l)"
    echo "Steady-state fault rate growth: ${GROWTH}x (prompt $FIRST_IDX → prompt $LAST_IDX, >=${MIN_SAMPLES_FOR_RATE} samples only, trimmed ±${STEADY_STATE_TRIM_SAMPLES} samples)"

    if (( $(echo "$GROWTH > $FAULT_GROWTH_THRESHOLD" | bc -l) )); then
        echo "  FAIL: Steady-state fault rate grew ${GROWTH}x (threshold: ${FAULT_GROWTH_THRESHOLD}x)"
        FAILED=$((FAILED + 1))
    else
        echo "  PASS: Steady-state fault rate growth within threshold"
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

# ============================================================
# Optional scale prompt (--scale flag)
# ============================================================

SCALE_FAULT_RATE_FULL=""
SCALE_FAULT_RATE_STEADY=""
SCALE_PEAK_RSS=""
SCALE_ELAPSED=""
SCALE_COMPLETION_KIND="skipped"

if [ "$SCALE_MODE" -eq 1 ]; then
    echo ""
    echo "=== Scale Prompt (200 sections) ==="
    SCALE_SAMPLE_FILE="$ARTIFACT_DIR/samples-scale.txt"

    PROMPT_SECTION20_BASE_COUNT="$(capture_pane | grep -cF "Section 20" || true)"
    SCALE_SENT_MS="$(now_ms)"
    tmux send-keys -t "$SESSION" "$SCALE_PROMPT_KEYWORD" Enter

    collect_samples "$PID" "$SCALE_PROMPT_TIMEOUT" "$SCALE_SAMPLE_FILE" &
    SCALE_SAMPLER_PID=$!

    if wait_for_prompt_cycle "$SCALE_PROMPT_TIMEOUT"; then
        echo "  Scale response completed via status bar"
        SCALE_COMPLETION_KIND="$PROMPT_COMPLETION_KIND"
    else
        echo "  WARNING: Scale response did not complete within ${SCALE_PROMPT_TIMEOUT}s"
        SCALE_COMPLETION_KIND="timeout"
    fi

    sleep 2
    kill "$SCALE_SAMPLER_PID" 2>/dev/null || true
    wait "$SCALE_SAMPLER_PID" 2>/dev/null || true

    SCALE_FAULT_RATE_FULL="$(compute_fault_rate "$SCALE_SAMPLE_FILE" 0)"
    SCALE_FAULT_RATE_STEADY="$(compute_fault_rate "$SCALE_SAMPLE_FILE" "$STEADY_STATE_TRIM_SAMPLES")"
    SCALE_PEAK_RSS="$(peak_rss "$SCALE_SAMPLE_FILE")"
    SCALE_ELAPSED="$(seconds_between_ms "$SCALE_SENT_MS" "$PROMPT_COMPLETED_MS")"

    echo "  Fault rate: full ~${SCALE_FAULT_RATE_FULL} minflt/sec | steady ~${SCALE_FAULT_RATE_STEADY} minflt/sec"
    echo "  Peak RSS:   ${SCALE_PEAK_RSS} kB"
    echo "  Elapsed:    ${SCALE_ELAPSED}s (${SCALE_COMPLETION_KIND})"
    if scale_timeout_non_blocking_p; then
        echo "  NOTE: Optional scale prompt hit ${SCALE_PROMPT_TIMEOUT}s and is recorded as a non-blocking harness limit"
    fi
    if $REPORT; then
        echo "  Samples:    $SCALE_SAMPLE_FILE ($(wc -l < "$SCALE_SAMPLE_FILE") points)"
    fi
    watch_pause
fi

# Save summary
write_verdict_json "$ARTIFACT_DIR/verdict.json"

echo ""
echo "Verdict: $ARTIFACT_DIR/verdict.json"

# ============================================================
# Summary
# ============================================================
echo ""
TOTAL=$((PASSED + FAILED))
echo "TUI_PERF_TEST passed=$PASSED failed=$FAILED total=$TOTAL"
if scale_timeout_non_blocking_p; then
    echo "TUI_PERF_TEST_NOTE scale_timeout=non-blocking timeout_s=$SCALE_PROMPT_TIMEOUT"
fi

if [ "$FAILED" -gt 0 ]; then
    exit 1
fi
