# GenAI Workflow for wearableAnomaly

## GenAI Workflow for wearableAnomaly

### Introduction

This vignette documents how generative AI tools helped build the
**wearableAnomaly** package. It summarizes the prompts, workflows, and
guardrails used while turning the Biostat 615 project outline into a
reproducible R package.

### Tools used

- **ChatGPT**: Used to plan, summarize goals, write “first prompts”, and
  convert vague goals into small, testable tasks.
- **OpenAI Codex Agent in VS Code**: Used to make actual repo changes.
  Especially useful for editing multiple files consistently, adding
  tests, and cleaning up packaging details.
- **Copilot Chat**: Used similarly to Codex for smaller edits. The key
  was to constrain scope to one file or one task.
- **pkgdown / R CMD check**: final validation and documentation builds.

### Development workflow

This is the loop I followed from start to finish.

#### Phase 1. Plan (before coding)

1.  Draft specs in ChatGPT.
    - Define purpose, inputs, outputs, edge cases, and invariants.
    - Decide the return schema up front (column names, types, units).

#### Phase 2. Implement (scoped Codex changes)

2.  Convert the spec into a Codex prompt.
    - Constrain scope to 1 file or 1 feature.
    - List concrete acceptance criteria (what must pass, what output
      must look like).
3.  Implement and refactor in Codex.
    - Add or update R/Rcpp code.
    - Add or update tests alongside the code.

#### Phase 3. Validate (tight feedback loop)

4.  Run locally and collect failures.
    - [`devtools::test()`](https://devtools.r-lib.org/reference/test.html)
    - [`devtools::check()`](https://devtools.r-lib.org/reference/check.html)
      when changes are bigger
    - Bench scripts when performance is involved
5.  Re-prompt with evidence.
    - Paste the failing output, the minimal reproduction, and the
      expected behavior.
    - Ask for the smallest fix that preserves the current API.

#### Phase 4. Stabilize the interface (when things get messy)

6.  Use ChatGPT for API/schema redesign when needed.
    - If outputs or internal schemas become hard to reason about, pause
      and redesign.
    - Then re-run Phase 2 to implement the new interface cleanly.

#### Phase 5. Package polish (after code is stable)

7.  Document and polish.
    - Update README examples so they run end-to-end.
    - Write vignettes to match the final presentation narrative.
    - Update pkgdown navigation so the story is easy to follow.

### Step-by-step examples

This section gives concrete examples of the prompts I used and what I
asked the agent to change. Each example follows the same pattern.

1.  Goal.
2.  Prompt.
3.  What I checked locally to confirm it worked.

#### Example 1. Create a “first prompt” so the agent understands project goals

**Goal.** Codex could not read my slides or PDFs directly, so I used
ChatGPT to produce a reusable “first prompt” that captures what the repo
should contain and what “done” means.

**Prompt (ChatGPT).**

![](figures/cant_read_files.png)

**What I validated.**

- The “first prompt” listed concrete repo requirements (installability,
  core functions, documentation, and GenAI tutorial).
- The prompt was structured so I could paste it into Codex unchanged.

#### Example 2. Targeted README edit (high leverage, low risk)

**Goal.** Make README match the real exported functions and include a
runnable quickstart example.

**Prompt (ChatGPT -\> Codex).**

![](figures/response_readme.png)

**What I validated.**

- README examples run from a clean session.
- The function list in README matches NAMESPACE exports.
- No unrelated files changed.

#### Example 2b. Capture the result as evidence (agent confirmation)

**Goal.** Save a clear “before moving on” checkpoint that the README
work actually landed.

**Evidence (Codex output).**

![](figures/check_output.png)

**What I validated.** - The change described in the output matches what
I see in `README.md`. - Running the README quickstart works locally.

#### Example 2c. Turn the checkpoint into the next targeted prompt

**Goal.** After the README was aligned, ask ChatGPT for the next prompt
that targets the next step in the cleanup plan.

**Prompt (ChatGPT).**

![](figures/next_step.png)

**What I validated.** - The suggested next prompt was scoped to one
task. - It moved the project forward without reopening the whole repo
plan.

#### Example 3. Read-only redundancy audit before cleanup

**Goal.** Identify redundant or confusing files that should be removed
or clearly quarantined, without making changes yet.

**Prompt (ChatGPT -\> Codex).**

![](figures/remove_redundancies.png)

**What I validated.**

- The output produced a concrete “keep vs drop” list.
- The recommendations were easy to convert into small cleanup tasks.

### Guardrails I used

- These rules helped keep changes safe and made it easier to debug.
- Keep scope small. One file or one feature per prompt.
- Require acceptance criteria. Always specify what success looks like.
- Always run tests after changes.
- Prefer minimal fixes over rewrites.
- When behavior is ambiguous, pause and redesign the interface before
  adding more code.

### What worked well

- **Scaffolding and consistency**. ChatGPT was good at turning a
  high-level outline into a coherent package plan (function names,
  responsibilities, and how pieces connect). This reduced early “design
  churn” and made later implementation more systematic.
- **Small, scoped edits**. Codex performed best when I constrained the
  task to one file or one feature and gave explicit acceptance criteria
  (expected columns, return types, edge cases, and what tests should
  pass).
- **Documentation acceleration**. Drafting README and vignette structure
  was fast, and it was easy to iterate on wording and organization
  before filling in details.
- **Boilerplate generation**. Rcpp skeletons, roxygen templates, and
  initial test scaffolds were high value because they are repetitive,
  but still require consistent formatting and naming.
- **Debugging with evidence**. When I supplied failing outputs and a
  minimal reproduction, Codex often found a precise fix quickly,
  especially for small off-by-one errors, missing imports/exports, and
  documentation mismatches.

### What didn’t work well

- **Ambiguity amplified quickly**. If I did not specify schema details
  (column names, units, and types), Codex frequently guessed
  incorrectly. The most common failures were unit mismatches (seconds vs
  minutes), inconsistent column naming, and small differences from my
  intended return structure.
- **Complex internal data structures**. Nested tibbles and list-columns
  were more error-prone for the agent. Even when the code “worked”, the
  outputs were sometimes awkward to consume, which forced an API
  redesign step.
- **Long prompts and broad scope**. Multi-file refactors in a single
  prompt often led to partial updates (tests not updated, docs drifting
  from exports, or helper functions invented but not wired in).
- **Methods logic still needed human control**. For changepoint details
  and statistical design decisions (e.g., penalty choices,
  energy-distance variants), GenAI was useful for implementation ideas
  and structure, but I still had to define the correct approach and
  verify the behavior carefully.

### Validation strategy

- **Unit tests**: ran `tests/testthat/` after each major change; many
  Codex-generated snippets failed initially and required re-prompts.
- **Comparator benchmarks**: compared wearableAnomaly PELT/E-divisive to
  [changepoint](https://github.com/rkillick/changepoint/) and `{ecp}`
  (when installed) using the bench scripts and `evaluate_methods`.
- **Visual inspection**: used the workflow vignette’s episode tables +
  overlay plot to verify detectors/changepoints.
- **pkgdown/site builds**: forced
  [`pkgdown::build_site()`](https://pkgdown.r-lib.org/reference/build_site.html)
  to ensure documentation matched the exported API.

### Lessons learned / advice

- **Start with a mini-spec, not code**. Write down purpose, inputs,
  outputs, invariants, and a concrete return schema first. Then prompt.
  This was the best way to prevent “almost-right” implementations.
- **Make prompts test-driven**. For any non-trivial change, I included
  acceptance criteria like “add tests that fail before the change and
  pass after” and “run
  [`devtools::test()`](https://devtools.r-lib.org/reference/test.html)
  after editing”. This kept the work grounded.
- **Keep scope intentionally small**. One file or one feature per prompt
  is dramatically easier to validate and debug than a broad refactor.
- **Always provide evidence when debugging**. The most effective
  debugging prompts included the failing test output, a minimal
  reproduction, and the exact expected behavior. This reliably produced
  smaller, safer fixes.
- **Use GenAI for speed, not authority**. It excelled at scaffolding,
  repetitive edits, and drafting documentation, but I treated all
  outputs as drafts until tests, benchmarks, and end-to-end examples
  confirmed the behavior.
- **Pause and redesign when the interface gets confusing**. If the
  output schema became hard to explain or use, it was worth stopping to
  simplify the API instead of piling on more patches.
