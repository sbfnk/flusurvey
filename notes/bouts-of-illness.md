# Bouts of illness

This file states in prose the rules that `bouts_of_illness()` uses to turn weekly symptom reports into bouts of illness. It also describes the cleaning in `merge_data()` that decides which reports reach it. Changes to cleaning, case definitions, bout reconstruction, incidence or contact classification must update this file and the corresponding tests in the same pull request. The rules can then be reviewed as rules, and the code checked against them.

The baseline was checked against `main` at `54e14ba`; the date and incidence policies below include the changes in #82. It records current behaviour, including known defects; it is not an endorsement of those defects. The agreed-direction section separates the agreed direction from functionality available on main. The historical baseline remains in the Git history (PR #89).

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

These symptom-group checks recognise positive values `1` or "t"; missing symptom answers do not themselves propagate uncertainty. Derived definitions are calculated on each report before bout construction. Combining already-derived ILI flags across a bout does **not** reapply the definition to the union of its symptoms.

## Start of a bout

The reports are sorted by participant, season and report date. Same-day ties retain their input order; the rules do not reconcile conflicting answers on that day. A symptomatic report starts a new bout if any of the following holds:

- S1. `same` is "no".
- S2. It is a leading report, and its start date is the earliest start date the participant reports in the season, counting all their reports in the season. Where several leading reports give that date, only the first of them starts a bout through this rule.
- S3. It is the participant's first report, the participant has no symptom-free report in the season, and every report answers `no.symptoms`.
- S4. The report before it was symptom-free. S4 needs `no.symptoms` to be a factor: with a character column it never applies.
- S5. Its start date is after the end date of the previous report of the participant in the season.

S2 and S3 apply only to participants who answer `no.symptoms` in every report of the season. An absent `same` column is treated as unanswered. A missing `same` is not a start under S1, and a missing start or end date is not a start under S2 or S5. A report with a missing `no.symptoms` answer is neither symptomatic nor symptom-free. It belongs to no bout, and it does not count as the symptom-free report before a start under S4.

Each symptomatic report then belongs to the bout of the last start at or before it. A symptomatic report with no start at or before it belongs to no bout.

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

A bout with a missing start or end date is never inverted.

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

## One row per bout

When the input has a `symptom.id` column, each bout becomes one row, unless `symptom.id` is the first column and there is no `health.score` column (see below). This is the row of its first report, with these changes:

- Every factor column with levels "t" and "f" becomes "t" if any report of the bout has "t". For columns with names ending in `.no` or `.none`, all reports must have "t". Otherwise the column becomes "f", unless a missing answer leaves the result open, in which case it is missing.
- `min.health.score` is the lowest health score in the bout, and `baseline.health.score` is the median health score of the participant's symptom-free reports in the season.
- `min.part.symptom.id` and `max.part.symptom.id` give the positions of the first and last report of the bout among the participant's reports in the season.

The "t"/"f" columns of the rows outside any bout are combined in the same way, per participant and season. All these rows take "t" if any of them has "t" (for `.no` and `.none` columns, if all of them have "t"), with missing answers handled as in a bout. Their other columns stay as they are.

With `symptomatic.only = TRUE` (the default) only the bout rows are returned. Without a `symptom.id` column the function stops with an error. With `symptom.id` as the first column and no `health.score` column, no bout rows are built: the function stops if `symptomatic.only = TRUE`, and otherwise returns every report. A `health.score` column moves `participant_id` and `season` to the front, through the merge that adds `baseline.health.score`, and the bout rows are then built.

## Incidence

[`get_incidence()`](../R/get_incidence.r) calls `bouts_of_illness()` with its defaults and assigns positive case-definition flags to the bout start period. Within a season and any requested strata, multiple positive bouts with the same participant identifiers and onset period contribute one case. The identifiers are all columns ending in `_id`.

The default `active.members` denominator counts participants between their `min.date` and `max.date`, including periods without reports. Those bounds come from all symptom reports at cleaning step 4, not the first and last symptom-free reports. The alternative `reports` denominator counts distinct participants reporting in that period. Dates are grouped by day, week, month or season-year; non-daily bounds are grouped to the same periods. Only periods present in the denominator survive the join, missing case counts become zero, and `min.N` defaults to one. The `exclude` argument is empty by default. `"unknown.onset"` removes bouts whose selected start date is missing; use `missing.start = "missing"` to avoid replacing unknown onset with the first report. Dates cleared by `inverted.dates = "clear"` are also missing and are excluded by this option. `"ongoing.at.entry"` removes bouts whose selected start precedes `min.date`, the first symptom-report date before first-day removal. This is distinct from contact follow-up beginning at the first symptom-free report. These exclusions affect the numerator only. With yearly aggregation an unknown start can still be assigned to its known season unless explicitly excluded; at finer resolutions an unknown start has no denominator period to match.

The synthetic policy examples and exclusion tests in [`test-bouts.R`](../tests/testthat/test-bouts.R) cover day, week, month and year aggregation. The options change bout boundaries or incidence inclusion; they do not change start rules S1–S5 or the independent contact reconstruction below.

## Illness on the day described by a contact report

A contact report submitted on 16 January describes contacts on **15 January** (`contact.date`). It is the participant's illness on 15 January that matters. There is no separate concept of a bout of contacts.

The current analysis uses [`contact_illness()`](../R/contact_illness.r), which still reconstructs illness intervals independently of `bouts_of_illness()`. The following describes that implementation, not the agreed shared-bout replacement.

1. Participants are grouped by `global_id`, and also by `season` when both input tables contain it. Symptom rows with missing grouping keys or report dates are excluded. Eligibility requires at least `min.reports` distinct reporting dates (default three) and at least one symptom-free report. Follow-up includes the first through last symptom-free report dates. Illness status outside follow-up, or without eligibility, is unknown rather than healthy; `follow_up` distinguishes those cases.
2. A symptomatic report supplies an interval only when it has a known start and a usable end no earlier than that start. A reported end is capped at the report date; otherwise `still_ill` supplies the report date as the interval end. This requires a known start even for `still_ill`.
3. Overlapping or adjacent intervals for the same participant and season are joined. For example, 10–12 January and 13–15 January become 10–15 January, even if the reports say they are different illnesses. A symptom-free report between overlapping intervals does not split them. All reports can support intervals, including those outside follow-up; eligibility only controls classification of contact dates.
4. A report without a usable interval, unless symptom-free, leaves an unresolved window from the day after the previous distinct reporting date through its own date. For the first report the window is just its own date. Within follow-up, a valid illness interval takes precedence over an unresolved window. Other unresolved days have unknown illness status; all remaining days are healthy, including days without a symptom report.
5. The `current_ili`, `current_ili_fever`, `current_ili_self` and `current_ari` flags within an illness interval require a symptom report on the date described by the contact report, with an interval covering that date. On other illness days these daily definition flags are missing even though `current_symptoms` is "symptoms". Multiple same-day reports combine as positive if any is positive, otherwise unknown if any is unknown, otherwise negative.
6. The `bout_*` flags combine the corresponding report-level definitions across the merged interval with the same positive/unknown/negative precedence. Positive flags apply throughout the interval. Negative flags become unknown on dates overlapping an unresolved window. Thus current negative bout classification can vary by day; it is not yet a single invariant classification for the whole bout. Known healthy days have negative daily and bout flags. Missing flag columns give unknown flags within illness intervals.

For example, within follow-up, a report on 15 January saying `still_ill` but giving no onset date currently leaves 15 January unknown unless another valid interval covers it. The agreed replacement must retain the evidence that the participant was ill on that date without inventing an onset or recovery date.

[`analysis/contacts.R`](../analysis/contacts.R) extracts symptoms with `clean = c("unsuccessful.join", "remove.bad.symptom.dates")`, retaining first-day and same-day reports and not applying the default report-count or symptom-free filters before classification. The classifier then determines eligibility itself. Its output retains contact rows outside follow-up with unknown illness status. The generic bout function does not identify chronic-only symptoms. `contact_illness()` has an opt-in `chronic = "duration"` option with `chronic.days`: a report with a valid interval longer than `chronic.days` days is chronic-only, supports no bout and leaves no day unknown, but still counts towards eligibility and follow-up. This is a heuristic, not a validated classifier: a long duration does not show that symptoms are chronic. The default is `chronic = "none"`. The option is applied in `contact_illness()`'s own reconstruction, so it does not yet affect incidence (#112).

The separate [`flag_ill_contacts()`](../R/flag_ill_contacts.r) accepts already-built bouts and tests inclusive start/end intervals by `global_id`. It does not reconstruct bouts, enforce eligibility or distinguish unknown from healthy: unmatched rows receive `FALSE`, including missing contact dates and bouts with missing boundaries. It does not explicitly group by season. It is not the classifier used by the current contact analysis.

## Known problems

- Leading reports before the first start are dropped. S1, S2 or S5 can start a bout among them. A participant whose first kept report continues an illness, with no start date or a later one than another report, loses that report and the illness it records (#19).
- S2 can start the bout at a later leading report. The leading reports before it are then dropped.
- With the default `start.date = "last"`, the selected onset may differ from earlier reported onsets (#20). `start.date = "earliest"` chooses the earliest instead.
- The default `missing.start = "first.report"` can create an onset after a known end (#76, #77). `missing.start = "missing"` preserves the unknown onset; `inverted.dates` controls inconsistent selected boundaries.
- With `symptomatic.only = FALSE` the reports outside any bout share their "t"/"f" values. A dropped symptomatic report makes the symptom-free reports of the participant show its symptoms, and the dropped report itself is marked as symptom-free (#87).
- With `limit.season` and `unsuccessful.join`, a participant who filled in the background survey only before the season window loses all their symptom reports (#91).
- `only.symptoms` also drops participants with any missing `no.symptoms` answer (#88).

## Agreed direction, not yet implemented on main

The shared-bout work in [#110](https://github.com/sbfnk/flusurvey/pull/110) must update the current-behaviour sections above as they land. Their intended rules are:

- One generic reconstruction supplies bouts and report-to-bout membership for both incidence and contact classification. Contacts must not reconstruct a second set by merging intervals independently.
- Retain illness evidence when onset or recovery is unknown. `still_ill` on 15 January establishes illness on 15 January; it does not establish recovery on that date or illness on 16 January. Unknown boundaries must remain distinguishable from inferred ones.
- Combine symptom evidence across reports belonging to the **same bout**, then apply each case definition to that union. A single report need not contain all qualifying symptoms, and symptoms need not be recorded concurrently. Bout classification is constant across the bout; it does not assert that every symptom was present every day. Explicit uncertainty and unanswered questions remain distinct evidence.
- Eligible follow-up starts at the first symptom-free report and ends at the last symptom-free report. Using end-of-illness as an alternative end remains a decision to resolve. Participants are healthy between bouts except where unresolved evidence prevents classification.
- Chronic-only symptoms should not constitute acute illness, but an operational classifier remains to be specified. The opt-in duration heuristic in `contact_illness()` is to be rebuilt on the shared reconstruction so that it applies to incidence as well as contacts (#112). A chronic condition such as asthma, or onset more than four weeks ago, must not alone exclude an acute illness.

These principles do not authorise silently replacing current defaults with a new onset-date policy or treating missing boundaries as known. Each implementation PR must state its actual policy and keep this prose and its tests aligned.

## Verification references

The source links above and [`bouts_of_illness()`](../R/bouts.r), [`merge_data()`](../R/merge_data.r) define the implementation checked for this baseline. Relevant regression tests are [`test-bouts.R`](../tests/testthat/test-bouts.R), [`test-merge-data.R`](../tests/testthat/test-merge-data.R), [`test-aggregate-symptoms.R`](../tests/testthat/test-aggregate-symptoms.R), [`test-contact-illness.R`](../tests/testthat/test-contact-illness.R) and [`test-flag-ill-contacts.R`](../tests/testthat/test-flag-ill-contacts.R). They cover parts of these rules, not a complete proof of the prose. Examples in this document are synthetic; no participant data or data-derived results are included.
