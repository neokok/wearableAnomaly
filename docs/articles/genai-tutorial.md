# GenAI Workflow for wearableAnomaly

## Introduction

This vignette documents how generative AI tools helped build the
**wearableAnomaly** package. It summarizes the prompts, workflows, and
guardrails used while turning the Biostat 615 project outline into a
reproducible R package.

## Tools used

- **ChatGPT**: high-level planning, function specs, and prompt
  refinement.
- **GitHub Copilot / Codex in VS Code**: implementation, refactors, and
  inline completions.
- **pkgdown / R CMD check**: final validation and documentation builds.

## Development workflow

1.  **Draft specs in ChatGPT**: describe the function purpose, inputs,
    outputs, and invariants before writing code.
2.  **Turn specs into Codex prompts**: feed the scoped requirements into
    VS Code’s Codex agent for implementation.
3.  **Code & refactor in Codex**: iterate on R/Rcpp functions, tests,
    and helper scripts, verifying each change locally.
4.  **ChatGPT for API redesign**: when internal schemas (nested tibbles,
    list columns) became confusing, use ChatGPT to reason about better
    interfaces.
5.  **Test → Re-prompt cycle**: run unit tests and benchmark scripts,
    note failures, then re-prompt ChatGPT/Codex with the failing
    scenario and desired fix.
6.  **Document & polish**: after code stabilized, use the same workflow
    to produce README, vignettes, and pkgdown navigation.

## Example prompts

> **Prompt (ChatGPT)**: “Outline a `detect_rate_change` function for CGM
> data. Inputs: wa_ts, window, threshold, scaling (MAD/SD/none).
> Outputs: tibble with id,start,end,type,strength.”

> **Prompt (Codex)**: “Given the spec above, implement the R function
> under R/artifacts.R, add roxygen comments, and plug into the existing
> merge pipeline.”

> **Prompt (ChatGPT)**: “Given this failing test output (paste
> `test_pelt` diff), explain how to reconcile Rcpp and R implementations
> and update the penalty docs.”

> **Prompt (Codex)**: “Refactor
> [`evaluate_methods()`](https://neokok.github.io/wearableAnomaly/reference/evaluate_methods.md)
> so it accepts a list of prediction objects, attaches runtime metadata,
> and returns a tibble with method/runtime/precision/recall/F1/IoU/MAE
> as used in the slides.”

## What worked well

- Rapid scaffolding of package structure and consistent naming.
- Generating Rcpp boilerplate (RcppExports, pelt_core.cpp, ed_core.cpp)
  with minimal hand-editing.
- Producing starter tests and roxygen templates that already matched
  standards.
- Quick iteration on README and vignette outlines before filling in real
  data.

## What didn’t work well

- Nested tibble/list schemas often confused Codex; manual fixes were
  required.
- Ambiguous prompts yielded incorrect column names or wrong units
  (minutes vs seconds).
- Complex statistical reasoning (penalties, energy distance variants)
  still needed hand-derived formulas.
- Without precise specs, Codex hallucinated helper functions or forgot
  to update tests.

## Validation strategy

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

## Lessons learned / advice

- Write precise mini-specs before prompting; ambiguity amplifies
  hallucinations.
- Treat AI output as a draft—tests and benchmarks are non-negotiable.
- Iterative prompt adjustments beat one long mega prompt.
- Let AI handle repetitive scaffolding, but keep humans in charge of
  statistical logic.
- Capture the workflow (like this vignette) so reviewers see the GenAI
  contribution.
