# Bouts of illness

This file states in prose the rules that `bouts_of_illness()` uses to turn weekly symptom reports into bouts of illness. It also describes the cleaning in `merge_data()` that decides which reports reach it. A change to either should change this file in the same pull request. The rules can then be reviewed as rules, and the code checked against them.

This version describes the code at commit `af3f969` (12 November 2026), before the fixes of November 2026. It records what the code did, including behaviour that is clearly unintended. The section "Known problems" lists those cases with the issue that covers each.

## Terms

A report is one weekly symptom survey. Reports are grouped by participant and season, where `participant_id` and `season` are set by `extract_data()`. One participant's reports in different seasons count as different participants.

A symptomatic report has `no.symptoms == "f"` and a symptom-free report has `no.symptoms == "t"`. The leading reports of a participant in a season are the symptomatic reports before their first symptom-free report. A participant with no symptom-free report in the season has no leading reports.

`same` is the answer to "Is this the same illness as in your last report?" ("yes", "no", "dont_know" or missing). `symptoms.start.date` and `symptoms.end.date` are the reported dates of onset and recovery, and `date` is the date of the report.

## Reports reaching `bouts_of_illness()`

`extract_data()` cleans each season separately with `merge_data()`. With the default `clean`, the symptom survey goes through these steps in order. Here a participant is a `global_id` within one season.

1. Only the last report of a participant on any one day is kept.
2. `limit.season`: only reports between 1 November and 1 April around the date with most reports are kept.
3. `aggregate_symptoms()` adds the case definitions (`ili`, `ili.fever`, `gi` and others). It keeps every report and leaves the columns used by the bout rules unchanged.
4. `n.reports`: participants with fewer than three reports are dropped. The count includes the report removed in the next step.
5. `remove.first`: each participant's earliest report is dropped.
6. `remove.bad.symptom.dates`: a start date after the report date is set to missing, as is an end date after the report date. When the end date is before the start date, both are set to missing. The documentation also promises to remove start dates before the first report and end dates after the last; the code does neither (#21).
7. `remove.bad.health.score` was meant to set health scores outside 0 to 100 to missing. It never ran, because the option and the check spelt the name differently (#6).
8. `only.symptoms`: participants with no symptom-free report among the remaining reports are dropped. A participant with a missing `no.symptoms` answer in any report is dropped as well, because their count of symptom-free reports is missing.
9. The surveys are joined by a rolling join on the report date. The last survey in the order given drives the join. If the contact survey came after the symptom survey, the rows were contact reports, each with the latest symptom report on or before its date (#31). With `unsuccessful.join`, rows without a report on or before their date from each of the other surveys are dropped. Last, character columns with values including both "t" and "f" become factors, but only those of the last survey in the order given; when the symptom survey is not last, its character columns stay character (#15).

`clean = NULL` was meant to skip all of this. `match.arg()` turned it into `"remove.first"`, and the first report was still removed (#5).

These steps change what the bout rules see:

- A participant who reports symptoms in every report of a season is dropped in step 8. This includes anyone who stops reporting before recovering from an illness they had at their first report. A participant can still reach `bouts_of_illness()` with only symptomatic reports if step 9 removes all of their symptom-free reports. Only then does rule S3 below apply.
- After step 5 the first report kept often continues an illness from the removed report, with `same` set to "yes". Rows dropped by `unsuccessful.join` have the same effect.
- Step 6 runs before the bout rules. A missing date in a bout is often a date that was reported and then removed as implausible.

## Start of a bout

The reports of each participant and season are taken in the order of the input table. With a `health.score` column, the merge that adds `baseline.health.score` first sorts the table by participant and season, keeping the input order within each participant. A symptomatic report starts a new bout if any of the following holds:

- S1. `same` is "no".
- S2. It is a leading report, and its start date is the earliest start date the participant reports in the season, counting all their reports in the season. Where several leading reports give that date, only the first of them starts a bout through this rule.
- S3. It is the participant's first report, the participant has no symptom-free report in the season, and every report answers `no.symptoms`.
- S4. The report before it was symptom-free. Here "the report before it" is the previous row of the whole table, except at the first report of a participant. S4 needs `no.symptoms` to be a factor: with a character column it never applies.
- S5. Its start date is after the end date of the report before it. Here "the report before it" is the previous row of the whole table. For a participant's first report, that is the last report of the participant in the row above.

S2 and S3 apply only to participants who answer `no.symptoms` in every report of the season. A missing `same` is not a start under S1, and a missing start or end date is not a start under S2 or S5. A report with a missing `no.symptoms` answer is neither symptomatic nor symptom-free. It belongs to no bout, and it does not count as the symptom-free report before a start under S4.

Each symptomatic report then belongs to the bout of the last start at or before it. A symptomatic report with no start at or before it belongs to no bout.

## Dates of a bout

The start date of a bout is the start date given in its last report. If the last report gives none, it is the last start date given in any of its reports. If no report gives one, it is the date of the first report.

The end date of a bout depends on whether its last report gives one. If it does, the bout keeps the end date of its first report, which may be missing or different. If it does not, the end date is the last end date given in any of its reports, and if no report gives one, the date of the last report.

The start and end dates are not checked against each other. A bout can end before it starts.

## One row per bout

When the input has a `symptom.id` column, each bout becomes one row, unless `symptom.id` is the first column and there is no `health.score` column (see below). This is the row of its first report, with these changes:

- Every factor column with levels "t" and "f" becomes "t" if any report of the bout has "t". For columns with names ending in `.no` or `.none`, all reports must have "t". Otherwise the column becomes "f", unless a missing answer leaves the result open, in which case it is missing.
- `min.health.score` is the lowest health score in the bout, and `baseline.health.score` is the median health score of the participant's symptom-free reports in the season.
- `min.part.symptom.id` and `max.part.symptom.id` give the positions of the first and last report of the bout among the participant's reports in the season.

The "t"/"f" columns of the rows outside any bout are combined in the same way, per participant and season. All these rows take "t" if any of them has "t" (for `.no` and `.none` columns, if all of them have "t"), with missing answers handled as in a bout. Their other columns stay as they are.

With `symptomatic.only = TRUE` (the default) only the bout rows are returned. Without a `symptom.id` column the function stops with an error. With `symptom.id` as the first column and no `health.score` column, no bout rows are built: the function stops if `symptomatic.only = TRUE`, and otherwise returns every report. A `health.score` column moves `participant_id` and `season` to the front, through the merge that adds `baseline.health.score`, and the bout rows are then built.

## Known problems

- Leading reports before the first start are dropped. S1, S2 or S5 can start a bout among them, and so can S4 when another participant's symptom-free report sits between two of them in an input without `health.score`. A participant whose first kept report continues an illness, with no start date or a later one than another report, loses that report and the illness it records (#19).
- S2 can start the bout at a later leading report. The leading reports before it are then dropped.
- S5 compares a participant's first report with another participant's last report. Where that end date is before this participant's start date, the first report starts a bout by chance (#22).
- Without a `health.score` column, a table sorted by date makes S4 and S5 compare reports of different participants. The rules depend on the input order, and `bouts_of_illness()` sorts only by participant and season, and only when there is a `health.score` column.
- The end date of a bout is lost when only its last report gives one, and wrong when its first report gives a different one (#18).
- The start date of a bout comes from its last report without a check against the other reports (#20).
- A bout with no reported start date takes the date of its first report as its start. If the end date it keeps is earlier, the bout ends before it starts (#76, #77).
- With `symptomatic.only = FALSE` the reports outside any bout share their "t"/"f" values. A dropped symptomatic report makes the symptom-free reports of the participant show its symptoms, and the dropped report itself is marked as symptom-free (#87).
- When the symptom survey is not the last one joined, its character "t"/"f" columns stay character (#15). S4 then never applies. Those columns are not merged within a bout, nor across the reports outside any bout.
- `only.symptoms` also drops participants with any missing `no.symptoms` answer (#88).
