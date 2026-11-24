# Bouts of illness

This file states in prose the rules that `bouts_of_illness()` uses to turn weekly symptom reports into bouts of illness. It also describes the cleaning in `merge_data()` that decides which reports reach it. A change to either should change this file in the same pull request. The rules can then be reviewed as rules, and the code checked against them.

The sections on reports reaching `bouts_of_illness()` and on the start of a bout describe the code at commit `af3f969` (12 November 2026), which includes behaviour that is clearly unintended. The section "Dates of a bout" describes the current code, which has the options for the date rules and the corrected end date. The section "Known problems" lists the remaining cases with the issue that covers each.

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
6. `remove.bad.symptom.dates`: first, when the reported end date is before the reported start date, both are set to missing. Then a start date after the report date is set to missing, as is an end date after the report date. The documentation also promises to remove start dates before the first report and end dates after the last; the code does neither (#21).
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

### Incidence

`get_incidence()` counts bouts by the week, day, month or year of their start date. Its `exclude` argument leaves bouts out of the counts. It takes any of:

- `"unknown.onset"`: bouts with no reported start date. This needs `missing.start = "missing"`. With the default `"first.report"`, no bout has a missing start date and nothing is excluded.
- `"ongoing.at.entry"`: bouts whose start date is before the participant's first report.

By default `exclude` is empty and every bout is counted. A bout with an unknown onset and `missing.start = "missing"` is not matched to contacts by report date.

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
- With the default `start.date = "last"`, the start date of a bout comes from its last report without a check against the other reports (#20). `start.date = "earliest"` avoids this.
- With the default `missing.start = "first.report"`, a bout with no reported start date takes the date of its first report as its start. If its end date is earlier, the bout ends before it starts. `missing.start = "missing"` and `inverted.dates` avoid this (#76, #77).
- With `symptomatic.only = FALSE` the reports outside any bout share their "t"/"f" values. A dropped symptomatic report makes the symptom-free reports of the participant show its symptoms, and the dropped report itself is marked as symptom-free (#87).
- When the symptom survey is not the last one joined, its character "t"/"f" columns stay character (#15). S4 then never applies. Those columns are not merged within a bout, nor across the reports outside any bout.
- `only.symptoms` also drops participants with any missing `no.symptoms` answer (#88).
