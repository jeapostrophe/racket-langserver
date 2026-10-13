# Available analysis and refresh completion

The session consumer in `source-controller.rkt` owns source revisions, retained
analysis, jobs, and publication. Workers return products; only the consumer can
install them. `SafeDoc-available` holds the completed product and the snapshot
whose text it analyzed. `SafeDoc-analysis` is the fresh job's broadcast completion
event. Available analysis is useful while that event is pending, but it never
completes the job or starts Resyntax.

Exact editor queries require available analysis under the current source
revision. Without it, they wait for the fresh job. Fast local queries keep their
existing adjusted-trace fallbacks. An unsuccessful refresh publishes its current
diagnostics and clears exact availability; execution failure or timeout also
clears availability. The old trace can still support existing fast fallbacks,
but cannot be retained as a completed success.

Closing an eligible file detaches its trace from the old Doc, moves the completed
product and analyzed snapshot into the closed-source cache, and clears the old
SafeDoc projections. It still cancels pending jobs, signals the old revision,
removes the active workspace contribution, and publishes empty diagnostics.
Edits clear eligibility before adjusting the trace, under the same write lock.
A trace adjusted by an unfinished edit cannot enter retained storage.

Opening creates a new source revision and SafeDoc. Exact text equality permits
moving a retained trace into that new lifetime, installing its contribution,
and publishing its Check Syntax diagnostics to the incoming document URI.
A different text discards the retained entry. Every open starts fresh analysis
through the normal bounded runner, even after restoration. Old completion events
and Resyntax results are never restored. Attempt identity and revision checks
still reject late results after edit, close, replacement, or shutdown.

Closed-target definition and hover requests first read saved text. Their owner
operation starts or joins the current revision's fresh job, and may return
matching retained immutable facts immediately while the job runs. Every shared
outcome still passes final disk validation and current-view validation. Repeated
queries for the same disk revision share one job. Dependency information may be
stale until fresh analysis replaces it; no dependency detection is involved.

Only closed sources occupy the session cache: at most 32 entries by default,
configurable with `make-source-controller`'s `#:retained-limit`. Insertion evicts
the oldest retained entry and its completed closed job, without invalidating
source authority or active documents. A zero limit disables retention. Shutdown
clears retained storage. Virtual documents retain their existing lifecycle and
never enter this cache.

`tests/lsp/closed-analysis-test.rkt` uses worker gates to cover real Typed Racket
restoration, a dependency type change, the `doc-check-syntax` definition jump,
eviction, and failure. Controller, definition, and hover tests cover repeated
reopen, URI spelling, cold waiting, request cancellation, and shared refreshes.
