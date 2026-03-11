# Trace Helper Scripts (I361)

This repository ships `bpftrace` helper scripts for the observability probes
implemented in `amoebum/src/usdt.lisp`.

## Prerequisites

1. Linux with `bpftrace` installed.
2. Privileges to attach tracing (`root` or `CAP_BPF`/`CAP_PERFMON` depending on distro/kernel policy).
3. A running `amoebum` process, or run `bpftrace -c ./dist/amoebum` to launch under tracing.

## Scripts

1. `scripts/trace/tool-latency.bt`
   - Purpose: histogram and counts for tool execution latency.
   - Probe inputs:
     - `amoebum:tool-enter(tool_name, request_id)`
     - `amoebum:tool-exit(tool_name, request_id, elapsed_ms, status)`
   - Output:
     - periodic call-count and total-latency maps,
     - final per-tool latency histogram in milliseconds.

2. `scripts/trace/stream-throughput.bt`
   - Purpose: per-second streaming completion throughput and latency for LLM streaming turns.
   - Probe inputs:
     - `amoebum:llm-request-start(model, base_url, mode, request_id)`
     - `amoebum:llm-request-end(model, base_url, mode, request_id, elapsed_ms, status)`
   - Output:
     - per-second completions and summed latency by model,
     - current inflight stream count,
     - final completion totals and latency histograms.

## Usage

Attach to an existing process:

```bash
sudo bpftrace scripts/trace/tool-latency.bt -p <amoebum-pid>
sudo bpftrace scripts/trace/stream-throughput.bt -p <amoebum-pid>
```

Launch amoebum directly under tracing:

```bash
sudo bpftrace scripts/trace/tool-latency.bt -c ./dist/amoebum
sudo bpftrace scripts/trace/stream-throughput.bt -c ./dist/amoebum
```

Stop tracing with `Ctrl-C`; each script prints final summary maps/histograms on exit.
