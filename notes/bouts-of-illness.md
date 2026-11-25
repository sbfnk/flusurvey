# Bouts of illness

This file states in prose the rules that `bouts_of_illness()` uses to turn weekly symptom reports into bouts of illness. It also describes the cleaning in `merge_data()` that decides which reports reach it. Changes to cleaning, case definitions, bout reconstruction, incidence or contact classification must update this file and the corresponding tests in the same pull request. The rules can then be reviewed as rules, and the code checked against them.

The baseline was checked against `main` at `54e14ba`; the date and incidence policies include #82, and the shared reconstruction and contact rules include #110. It records current behaviour, including known defects; it is not an endorsement of those defects. The agreed-direction section separates the agreed direction from functionality available on main. The historical baseline remains in the Git history (PR #89).

## Terms

A report is one weekly symptom survey. Reports are grouped by participant and season, where `participant_id` and `season` are set by `extract_data()`. One participant's reports in different seasons count as different participants.

A symptomatic report has `no.symptoms == "f"` and a symptom-free report has `no.symptoms == "t"`. The leading reports of a participant in a season are the symptomatic reports before their first symptom-free report. A participant with no symptom-free report in the season has no leading reports.

`same` is the answer to "Is this the same illness as in your last report?" ("yes", "no", "dont_know", "not_applicable" or missing). `symptoms.start.date` and `symptoms.end.date` are the reported dates of onset and recovery, and `date` is the date of the report.

## Reports reaching `bouts_of_illness()`

`extract_data()` cleans each season separately with `merge_data()`. With the default `clean`, the symptom survey goes through these steps in order. Here a participant is a `global_id` within one season.

1. Surveys are ordered background, contact, symptom. At this stage, every row of the last survey present (the driving survey) is retained, including multiple reports on the same day; the subsequent cleaning rules can still remove rows. Earlier surveys keep only the last input row per participant and date before joining. Thus symptom-only extraction preserves same-day symptom reports.
2. `limit.season`: each survey is restricted separately using its busiest report date, choosing the earliest date in a tie. The start is 1 November of that year for November–December, otherwise of the previous year. The end is 1 April of that year for January–April, otherwise of the next year. Both boundaries are included. For a busiest date in May–October this produces a 17-month window, not a single winter.
3. `aggregate_symptoms()` adds report-level case definitions, as described below. It keeps every report.
4. `nReports` counts distinct symptom-report dates per participant. `n.reports` drops participants with fewer than `min.reports` dates (default three). Multiple reports on one day count once. `min.date` and `max.date` are then calculated from all remaining report dates, before first-day removal; these are not symptom-free follow-up boundaries.
5. `remove.first`: all symptom reports on the participant's earliest reporting day are dropped. The eligibility count and date bounds include that day and are not recalculated afterwards.
6. `remove.bad.symptom.dates`: first, when the end precedes the start, both dates become missing. Then a start after the report date becomes missing, as does an end after the report date. Dates before entry or more than four weeks earlier are not removed by this rule.
7. `remove.bad.health.scores`: scores outside 0 to 100 become missing; an absent health-score column is skipped.
8. `only.symptoms`: participants with no symptom-free report among the remaining reports are dropped. A participant with any missing `no.symptoms` answer is also dropped because the count of symptom-free reports is missing.
9. The surveys are joined by a rolling join on participant and report date. Each driving row receives the latest earlier-survey row on or before its date. Missing or empty participant identifiers are discarded. Joining contact and symptom surveys gives a warning: this join does not classify illness on the day described by the contact survey. `unsuccessful.join` removes rows missing an identifier from any included survey. Consequently, removing an early background report in step 2 can remove subsequent symptom reports at this step. `remove.postcodes` removes postcode columns. Finally, character columns containing both "t" and "f" become factors; a character column containing only one of those values stays character.

`clean = NULL` or `character(0)` skips the named cleaning options. It still deduplicates earlier surveys, calculates case definitions, `nReports`, `min.date` and `max.date`, transforms the other surveys, joins them, discards unusable participant identifiers and converts the qualifying columns to factors.

These steps change what the bout rules see:

- A participant who reports symptoms in every report of a season is dropped in step 8. This includes anyone who stops reporting before recovering from an illness they had at their first report. A participant can still reach `bouts_of_illness()` with only symptomatic reports if step 9 removes all of their symptom-free reports. Under default cleaning, this is how rule S3 below can become relevant; custom cleaning can also retain participants with only symptomatic reports.
- After step 5 the first report kept often continues an illness from the removed report, with `same` set to "yes". Rows dropped by `unsuccessful.join` have the same effect.
- Step 6 runs before the bout rules. A missing date in a bout is often a date that was reported and then removed as implausible.

## Report-level case definitions

[`aggregate_symptoms()`](../R/aggregate_symptoms.r) retains an `onset` value: `sudden`, `not.sudden`, `dont_know` (explicit uncertainty), `not.answered`, or `no.question` (no onset-question column). These are distinct evidence states. A "yes" to a fever-onset question can establish sudden onset; a "no" resolves an otherwise uncertain or unanswered onset as not sudden. Explicit symptom-onset answers are used first, with fever "yes" able to override "no".

The default `ili.sudden.unknown = NA` leaves unresolved onset missing in `suddenly`. Setting it to 1 or 0 interprets unresolved onset as sudden or not sudden respectively, without changing the recorded evidence. A report failing another ILI criterion is negative even when onset is uncertain.

Where the required symptom columns exist:

- Fever is a positive `fever` or `fever.symptom` answer; if `fever` is absent, a temperature-range code greater than zero supplies it.
- Systemic symptoms are tiredness, weakness or headache. Respiratory symptoms are sore throat, cough or shortness of breath.
- `ili.symptoms` requires fever or a systemic symptom, together with a respiratory symptom. `ili` additionally requires sudden onset. `ili.fever` additionally requires fever; its creation also requires a systemic-symptom column in the input.
- `ili.self` is a self-assessment of ILI; missing self-assessment is currently negative.
- `gi` requires vomiting or diarrhoea.

These symptom-group checks recognise positive values `1` or "t"; missing symptom answers do not themselves propagate uncertainty. Derived definitions are initially calculated on each report. The shared bout reconstruction then combines symptom evidence within each bout and reapplies its case definitions, as described below.

## Start of a bout

The reports are sorted by participant, season and report date. Same-day ties retain their input order; the rules do not reconcile conflicting answers on that day. A symptomatic report starts a new bout if any of the following holds. These rules are shared by `bouts_of_illness()`, `bout_evidence()` and contact classification:

- S1. `same` is "no".
- S2. It is the first symptomatic report observed for the participant in that season. This retains an illness already under way, even when `same` is "yes", "dont_know", "not_applicable" or missing, or onset is unknown. Revising the onset in a later report does not alone split the leading illness.
- S3. The old special case for participants with no symptom-free report is subsumed by S2.
- S4. The immediately preceding report was symptom-free. Character and factor answers are normalised to the same representation.
- S5. Its start date is after the end date of the previous report of the participant in the season.

An absent `same` column is treated as unanswered. Missing `same` is not a start under S1, and missing boundary dates do not trigger S5. A report with missing `no.symptoms` belongs to no bout and does not itself separate two symptomatic reports into different bouts. Every explicitly symptomatic report belongs to a bout. Reports with unknown symptom status remain available to the contact classifier as unresolved observations.

## Dates of a bout

Three arguments of `bouts_of_illness()` set the dates of a bout. `get_incidence()` passes them on. The defaults keep the start-date behaviour of the code before the end-date fix. The end date differs from that code whatever the arguments.

`start.date` chooses among the start dates reported in the bout:

- `"last"` (the default): the start date given in the last report of the bout. If the last report gives none, the last start date given in any report of the bout.
- `"earliest"`: the earliest start date given in any report of the bout.

`missing.start` sets the start date of a bout in which no report gives one:

- `"first.report"` (the default): the date of the first report of the bout. This is the date the participant first reported symptoms, so it is not an observed onset.
- `"missing"`: the start date stays missing.

The end date of a bout is the last end date given in any of its reports, and every report of the bout gets it. If no report gives one, it is the date of the last symptomatic report of the bout. The first symptom-free report that follows is not used, although its date is the earliest by which the participant reported being well. A date taken from the last symptomatic report is a fallback that the participant never reported as a recovery date. The earlier rule kept the end date of the first report when the last report gave one, which could be missing or a different date. It used the last reported date only when the last report gave none. The rule applies without any argument and changes the output of `bouts_of_illness()` for bouts whose reports give different end dates.

`inverted.dates` sets what happens when the end date of a bout is before its start date:

- `"keep"` (the default): both dates stay.
- `"clear"`: both dates become missing.
- `"reset.end"`: the end date becomes the date of the last report of the bout.

A bout with a missing start or end date is never inverted. These selected dates are summary policies; contact classification uses the separate report evidence below, so a fallback date does not establish an observed onset or recovery.

### Examples

The reports of a participant with `same` set to "no" then "yes", with these dates:

| Report | Start date | End date |
|---|---|---|
| 10 January | 5 January | 8 January |
| 17 January | 6 January | 12 January |

The bout starts on 6 January with the default and on 5 January with `start.date = "earliest"`. It ends on 12 January, the last reported end date.

A participant whose reports give no start date, and whose end date is reported only in the last report (16 January), has a bout that starts on the date of its first report (10 January) by default and has no start date with `missing.start = "missing"`. It ends on 16 January.

A participant whose bout starts on 9 January and whose only reported end date is 8 January has an inverted bout. The default keeps both dates, `"clear"` sets both to missing, and `"reset.end"` moves the end to the date of the last report.

The tests in `tests/testthat/test-bouts.R` run these examples, and cases with several distinct reported end dates, with the end date reported only in the first or only in the last report, and with no end date reported. They also run participants who report no onset in any report.

## One row per bout and combined case definitions

`bouts_of_illness()` retains the first report of each bout, with selected dates and combined evidence. `symptomatic.only = TRUE` returns only those rows; `FALSE` also retains reports outside bouts with their original answers. Construction no longer depends on whether `symptom.id` exists or its column position. `min.part.symptom.id` and `max.part.symptom.id` locate the first and last supporting report in the participant-season sequence. Health-score summaries remain the minimum within the bout and median of the participant's symptom-free reports.

Factor columns containing levels "t" and "f" combine within actual bouts by `any`, except names ending `.no` or `.none`, which use `all`; missing answers remain unresolved when they prevent a decision. Reports outside bouts are not pooled into an artificial bout.

Case definitions are recomputed separately. Raw fever, systemic, respiratory and gastrointestinal answers are combined across the reports of the **same** bout: any positive makes that component positive; otherwise a missing answer makes it unknown; otherwise it is negative. When raw components are unavailable, their derived report-level components are combined with the same precedence. The ILI formula then uses the combined components and onset evidence. For example, fever in one report and cough in another report of the same illness can jointly meet the symptom criteria. No single questionnaire or day need contain both. Self-assessed ILI remains separate. If only a final definition flag is available, without its components, the fallback combines that supplied flag rather than inventing component evidence.

The bout-level onset rule gives an explicit sudden answer precedence, followed by not sudden, explicit uncertainty, unanswered and no question. The `onset.conflict` flag in `bout_evidence()$bouts` records conflicting explicit answers. Original report-level onset states remain available in `bout_evidence()$reports`. An explicit sudden-onset answer anywhere in the same bout satisfies the sudden-onset criterion even if another report says onset was not sudden. Other symptoms may develop gradually; sudden onset need not describe every symptom or every report. The remaining ILI criteria must still be met by the combined evidence of that bout. The extraction option `ili.sudden.unknown` continues to control derived onset flags for uncertain answers.

The default `daily.flags = "none"` returns one row per bout. Optional `"bout"` applies its combined classification to each selected interval day, `"report"` uses report-day flags and `"carry.forward"` uses the last earlier report in the bout. These expansions skip missing or inverted selected intervals; they do not fabricate endpoints. They are an explicit summary-based expansion, not the evidence used by contact classification.

## Report evidence and established illness intervals

`bout_evidence()` returns:

- `bouts`: one row per shared bout, its case definitions and selected boundaries. Its default `missing.start = "missing"` preserves unknown onset. `onset.known` and `recovery.known` say whether a selected boundary matches a reported boundary. `onset.source` and `recovery.source` distinguish `reported`, `missing`, `first.report` and `last.report` as applicable; a reset end is a last-report fallback, not observed recovery.
- `reports`: original report start/end dates, report-to-bout membership, onset answers where available, and the supported interval `first.day`–`last.day`. `row_id` identifies the supporting row (supplied row identifiers are retained; otherwise it is the sorted input position). A missing onset can be recovered from the earliest onset supplied elsewhere in the same bout, without replacing the original report answer.
- `intervals`: the span from the first to last established illness day of each bout. Illness persists between established days belonging to that same bout. Touching or overlapping intervals belonging to different bouts do not merge their identities.
- `days`: an optional daily expansion of those evidence intervals. Sources distinguish reported intervals (`reported`), a still-ill observation with no usable onset (`report.date`) and inferred intervening days (`within.bout`). `expand.days = FALSE` avoids expansion and returns an empty days table; the contact classifier uses intervals directly.

A report with usable onset and recovery supports their inclusive interval, with recovery capped at the report date for day evidence. `still_ill` without a recovery supports illness through its own date. Without a usable onset anywhere in that bout, it establishes only its own day until other same-bout evidence supports a longer span. A report lacking both a usable interval and an explicit continuing-illness observation is unresolved. Original reported dates remain available even when a selected summary boundary is cleared or reset.

## Incidence

[`get_incidence()`](../R/get_incidence.r) calls `bouts_of_illness()` with its defaults and assigns positive case-definition flags to the bout start period. Within a season and any requested strata, multiple positive bouts with the same participant identifiers and onset period contribute one case. The identifiers are all columns ending in `_id`.

The default `active.members` denominator counts participants between their `min.date` and `max.date`, including periods without reports. Those bounds come from all symptom reports at cleaning step 4, not the first and last symptom-free reports. The alternative `reports` denominator counts distinct participants reporting in that period. Dates are grouped by day, week, month or season-year; non-daily bounds are grouped to the same periods. Only periods present in the denominator survive the join, missing case counts become zero, and `min.N` defaults to one. The `exclude` argument is empty by default. `"unknown.onset"` removes bouts whose selected start date is missing; use `missing.start = "missing"` to avoid replacing unknown onset with the first report. Dates cleared by `inverted.dates = "clear"` are also missing and are excluded by this option. `"ongoing.at.entry"` removes bouts whose selected start precedes `min.date`, the first symptom-report date before first-day removal. This is distinct from contact follow-up beginning at the first symptom-free report. These exclusions affect the numerator only and do not remove bouts from the shared evidence used for contacts. With yearly aggregation an unknown start can still be assigned to its known season unless explicitly excluded; at finer resolutions an unknown start has no denominator period to match.

The synthetic policy examples and exclusion tests in [`test-bouts.R`](../tests/testthat/test-bouts.R) cover day, week, month and year aggregation. The options change bout boundaries or incidence inclusion; they do not change start rules S1–S5 or turn summary fallback dates into observed contact evidence.

## Illness on the day described by a contact report

A contact report submitted on 16 January describes contacts on **15 January** (`contact.date`). It is the participant's illness on 15 January that matters. There is no separate concept of a bout of contacts.

The analysis uses [`contact_illness()`](../R/contact_illness.r), which consumes the shared `bout_evidence()` output. It does not reconstruct illness by independently joining date intervals.

1. Participants are grouped by `global_id`, and also by `season` when both input tables contain it. Missing grouping keys or symptom-report dates cannot support matching. Eligibility requires at least `min.reports` distinct reporting dates (default three) and at least one symptom-free report. Follow-up includes the first through last symptom-free report dates. Outside follow-up or without eligibility, illness status is unknown; `follow_up` distinguishes these cases.
2. Within follow-up, days in an established shared-bout interval are ill. Separate adjacent illnesses retain their own case definitions. If distinct bouts overlap on a date, the contact result combines their flags with positive, then unknown, then negative precedence; the underlying bout identities remain separate.
3. A report without a usable illness interval, unless symptom-free, leaves the days after the previous distinct reporting date through its own date unresolved. For a first report this is just its own date. For a still-ill report whose onset cannot be recovered, the preceding window ends the day before its known illness day. A still-ill report without a recovery also leaves days after its date and before the next report unresolved. Established bout evidence takes precedence over these unresolved windows. Other days within follow-up are healthy by default, including days without reports. The `healthy` option of `contact_illness()` changes this (see below).
4. The `bout_ili`, `bout_ili_fever`, `bout_ili_self` and `bout_ari` values use the single classification of the matching bout on every established illness day. Uncertainty about a report's dates does not turn a known negative definition into an unknown definition on only some days. Unknown symptom components can make the whole-bout classification unknown.
5. Daily `current_*` definition flags require a report on the date described by the contact questionnaire whose supported interval includes that date. Elsewhere within a bout they are missing even though illness is established. Multiple reports that day combine as positive if any is positive, otherwise unknown if any is unknown, otherwise negative. Known healthy days have negative definition flags. These daily flags are distinct from the bout-based exposure.

The `healthy` option sets the rule for a day within follow-up that lies outside any bout. With `healthy = "follow-up"` (the default) every such day is healthy, whether or not a report exists for that day. With `healthy = "report"` a day is healthy only if a later symptom-free report covers it, from the date of the previous report up to the day before its own. Days before a participant's first report, and days that no such report covers, are unknown rather than healthy. The option changes neither which days are ill or unresolved nor the bout flags.

For example, a participant's `still_ill` report on 15 January with no known onset establishes illness for a contact questionnaire submitted on 16 January, which describes the 15th. It does not establish recovery on the 15th, illness on the 16th, or health on the 16th. A later observation may resolve that uncertainty. Two still-ill reports assigned to the same bout on 15 and 20 January establish continuing illness between those dates.

`contact_illness()` has an opt-in `chronic = "duration"` option with `chronic.days`: a report with a valid interval longer than `chronic.days` days is chronic-only, supports no bout and leaves no day unknown, but still counts towards eligibility and follow-up. This is a heuristic and has not been validated: a long duration does not show that symptoms are chronic. The default is `chronic = "none"`. The option acts inside `contact_illness()` before the shared reconstruction. It does not yet affect incidence (#112).

[`analysis/contacts.R`](../analysis/contacts.R) extracts symptoms with `clean = c("unsuccessful.join", "remove.bad.symptom.dates")`, retaining first-day and same-day reports and avoiding the default report-count and symptom-free filters before classification. The classifier applies eligibility itself. Contact rows outside follow-up remain in the output with unknown illness status. The default applies no chronic-only rule.

The separate [`flag_ill_contacts()`](../R/flag_ill_contacts.r) accepts already-built bouts and tests inclusive start/end intervals by `global_id`. It does not reconstruct bouts, enforce eligibility or separate unknown from healthy. Unmatched rows receive `FALSE`, including missing contact dates and bouts with missing boundaries. It does not group by season explicitly, and the contact analysis does not use it as its classifier.

## Remaining choices and limitations

The date-policy defaults remain those documented above; choosing an earliest onset or excluding onset before entry is explicit. The contact follow-up window ends at the last symptom-free report; using end-of-illness as an alternative remains unresolved. Chronic-only symptoms should not count as acute illness, but an operational classifier remains to be specified. A chronic condition such as asthma, or onset more than four weeks ago, is not by itself an exclusion.

Default extraction can still remove an illness's supporting reports: `only.symptoms` excludes participants with any missing symptom-status answer, and season restriction combined with an unsuccessful background join can remove symptom reports. The contact analysis avoids those cleaning options for symptoms. Missing symptom answers are treated differently by the initial report-level aggregation and the uncertainty-preserving bout aggregation; they should not be interpreted as evidence of absence.

The leading-report retention cases from #45 are included in the shared reconstruction, while the optional date policies remain those of #82. Further splitting or entry policies in #47/#48 must not silently change these rules. Any follow-up that changes a rule must update this prose and its regression tests together. Chronic-only symptoms should not count as acute illness; we plan to move the opt-in duration heuristic into the shared reconstruction, to cover incidence as well as contacts (#112).

## Verification references

The source links above and [`bouts_of_illness()`](../R/bouts.r), [`merge_data()`](../R/merge_data.r) define the implementation checked for this baseline. Relevant regression tests are [`test-bouts.R`](../tests/testthat/test-bouts.R), [`test-shared-bouts.R`](../tests/testthat/test-shared-bouts.R), [`test-merge-data.R`](../tests/testthat/test-merge-data.R), [`test-aggregate-symptoms.R`](../tests/testthat/test-aggregate-symptoms.R), [`test-contact-illness.R`](../tests/testthat/test-contact-illness.R) and [`test-flag-ill-contacts.R`](../tests/testthat/test-flag-ill-contacts.R). They cover parts of these rules, not a complete proof of the prose. Examples in this document are synthetic; no participant data or data-derived results are included.
