# Source ownership and analysis lifetime

The session consumer owns source authority, analysis attempts, available results,
and publication. Disk readers and analyzers run privately and report products;
only the consumer can accept them. Source identity, source revision, and attempt
identity are separate: a revision is a source lifetime token, and retrying the
same revision creates a new attempt.

## Interfaces and responsibilities

`source-controller.rkt` exposes commands for request threads:

- `source-controller-observe-evt` acquires current source knowledge. For a file
  without an open buffer, each command is an ordered read barrier: it cannot
  reuse a disk read started before the consumer accepted that command. Concurrent
  observations share the next read. Open buffers require no disk read.
- `source-controller-analysis-evt` starts or joins analysis for a snapshot and
  replies with `Analysis-Handle`. Its `reusable-summary` field contains optional
  reusable immutable facts; its `completion-evt` belongs to the fresh attempt.
  A completed current success reuses its original completion event. A ready
  outcome takes precedence over reusable facts, including failure and stop.
- `source-controller-current-view-evt` asks the consumer whether a captured view
  is still current. It does not observe the filesystem.

Constructing a command event submits work immediately. Cancelling a request's
wait does not cancel shared work. Request callers must check cancellation before
submission and validate the target after waiting, including when using available
facts. A source revision certifies the server's accepted observation, not an
atomic filesystem transaction.

Cross-file definition and hover observe the target once per request. They use
that snapshot's analysis and perform a final owner view check without rereading
disk. An observed target change or invalidated acquisition ends the request with
RequestFailed; the query does not reacquire or retry. A subsequent query observes
the target again. An external disk change that the server has not observed may
therefore leave an in-flight query answering from its original snapshot. Opening
a buffer during a pending disk read may complete acquisition with buffer text;
once acquired, that view must remain eligible through the query's final check.
Missing files, Check Syntax errors, and absent bindings supply no feature detail.
Read/worker failures end the request. Changing or closing the requesting document
cancels it; deadlines and shutdown end it without cancelling shared analysis.

Binding lookup preserves the full path, submodule list, phase+space, and identifier.
There is no query restriction on submodules or phase+space: matching Check Syntax
jump and definition-target callbacks suffice, including after upstream fixes.
Missing or inconsistent upstream binding information gives no detail; the query
does not fall back to a different submodule or phase. Target observation uses
`path->source`, which preserves the exact binding path as source identity and
constructs the URI separately; no URI round-trip check is needed.

The `owner` submodule exports operations for the session consumer only. Open
registers the buffer, restores matching retained analysis, and schedules fresh
analysis. Edit takes the registered buffer selected by the incoming URI, owns
the write lock, invalidates eligibility before the supplied
document mutation, and schedules the successor after releasing the lock. Its
mutation callback updates text and client version and must not acquire the same
buffer lock. Close handles retention, cancellation, and removal of publications.
`source-controller-refresh-analysis!` joins active work or starts another attempt
after completion; `source-controller-ensure-analysis!` also reuses a current
completed success. The command queue accepts named operations and worker
completions; it has no arbitrary callback command.

`source-controller.rkt` owns command submission, reply handling, and consumer
dispatch. `source-controller/source-owner.rkt` owns the private controller
record, construction, source transitions, acquisition policy, revision validation,
result routing, and shutdown ordering. The public interface calls semantic owner
operations and re-exports the supported interfaces; it cannot access record
fields or subsystem handles. The owner never imports the public interface.
Lifecycle and acceptance policy stay together in the owner so their revision,
locking, cancellation, and trace-transfer invariants remain local. Subsystems own
private records and collections; none imports or receives the controller:

- `workspace/source-store.rkt`: source authority, revisions, and change signals.
- `disk-reader.rkt`: `Disk-Reader`, ordered per-source disk reads and waiters.
- `analysis-scheduler.rkt`: `Analysis-Scheduler`, shared queued/running attempts,
  FIFO dispatch, capacity, cancellation, and attempt completion.
- `supervised-worker.rkt`: `Supervised-Worker`, resource ownership, timeout and
  premature-exit detection. It suppresses worker stdout and releases resources
  before reporting a result. Cancelling work does not kill its supervisor;
  a resulting late message is rejected by the reader or scheduler.
- `analysis-publisher.rkt`: `Analysis-Publisher`, document analysis installation,
  workspace contributions, diagnostics, and Resyntax publication. Buffer
  operations require the controller-held SafeDoc write lock.
- `closed-analysis-cache.rkt`: `Closed-Analysis-Cache`, bounded completed analysis
  owned while files are closed.
- `completion-event.rkt`: `Completion-Event`, a single-owner first-value
  broadcast event, including when that value is `#f`.

Worker messages are opaque envelopes. Readers and schedulers check attempt
identity before invoking a synchronous acceptance procedure on the consumer:

- Disk acceptance receives the started view and read result, commits source
  knowledge, cancels analysis when that view changes, and returns the observer
  response plus the successor disk view. The reader then answers current waiters
  and starts any queued read using that successor view.
- Analysis acceptance receives the snapshot, kind, completion, parent completion
  (for Resyntax), and result. The controller validates revision and executor
  products, installs or discards analysis, and translates acceptance failures to
  terminal outcomes. The scheduler removes the attempt and resolves its event
  only after acceptance returns. Successful open-buffer analysis may enqueue
  Resyntax during acceptance; dispatch occurs after the command finishes.

These procedures run inline, do not wait for workers, and do not dispatch another
controller command. They are internal acceptance boundaries, not request commands
or a callback registry. Source replacement cancels affected work. The scheduler
also checks queued snapshot eligibility immediately before launch. Availability
lookup and reuse of completed success belong to the controller, not the scheduler.

The controller receives its publication workspace through `#:workspace`.
The default remains the process's `current-workspace`, also used by existing
workspace requests. Injecting a different publication workspace alone does not
create an isolated LSP session; the request layer must use that same workspace.

## Available results and fresh completion

`SafeDoc-available` holds a completed product and the snapshot it analyzed.
`SafeDoc-analysis-completion` is the current fresh attempt's broadcast event.
Available analysis may answer queries while that event is pending. Restoring a
product never completes that event or starts Resyntax.

Only queued and running attempts occupy the scheduler's private attempt table. Every terminal
attempt is removed. An accepted available result retains its completion event,
so repeated acquisitions can reuse success without retaining the job. Resyntax
acceptance compares its parent completion with the installed result's completion;
same-revision retries cannot accept an old attempt's findings.

Exact editor queries require available analysis under the current source
revision; otherwise they wait for fresh completion. Fast local queries keep
their adjusted-trace fallbacks. An `errors` outcome publishes current
diagnostics and clears exact availability. A `failed` outcome or timeout also
clears availability. The old trace may still support fast fallbacks, but cannot
be retained as a completed success. Non-success attempts may be retried.

## Close, restore, and retention

Closing an eligible file detaches its trace from the old Doc, moves its completed
product into closed retention, and clears the SafeDoc projections. Close also
cancels pending jobs, signals the old revision, removes the active workspace
contribution, and publishes empty diagnostics. Edits clear eligibility before
adjusting the trace, so an unfinished edit cannot enter retained storage.

Opening creates a new source revision. Exact text equality permits transferring
a retained trace into the new buffer, installing its contribution, and publishing
its Check Syntax diagnostics to the incoming URI. The restored available result
has no fresh completion in that lifetime. Every open schedules fresh analysis;
old completion events and Resyntax findings are never restored.

Closed-target definition and hover requests observe saved text before acquisition.
They may use matching retained facts while fresh analysis runs, then perform final
view validation. Text equality does not establish dependency freshness:
retained dependency information can remain stale until fresh analysis replaces it.
Neither current revisions nor fresh completion track external dependency changes.

Only closed files occupy retention: at most 64 entries by default, configurable
with `#:closed-analysis-limit`. Results are keyed by source ID in a private hash. Insertion
and successful text-matching lookups mark an entry most recently used; misses and
text mismatches leave recency unchanged. Insertion beyond capacity evicts the
least recently used result without changing source authority or live documents.
Reopening removes the entry before checking its text and transferring its trace,
so retained storage cannot keep a trace that a live document starts modifying.
A zero limit disables retention; evicted sources must analyze again. Existing
waiters retain their outcomes.
Virtual sources never enter retention. Shutdown stops work and clears retention.

`tests/lsp/source-controller-test.rkt` covers ordered observation through the
production consumer. `analysis-controller-test.rkt` covers attempt identity,
bounds, acceptance, available facts, cancellation, and URI preservation.
`closed-analysis-test.rkt` covers restoration, dependency changes, and eviction.
`closed-analysis-cache-test.rkt`, `disk-reader-test.rkt`,
`analysis-scheduler-test.rkt`, and `supervised-worker-test.rkt` exercise subsystem
interfaces, ownership, and completion ordering without exposing mutable records.

Integration tests use the production consumer and its named commands or LSP
messages. Tests that must order individual transitions or replay delayed worker
messages own an isolated controller on the test thread and call the same owner
operations directly. Assertions run on the test thread; command failures are
values only where the test explicitly exercises that production contract.
