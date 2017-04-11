header_replace <- function(x, old_questions, new_questions)
{
    old_pos <- which(names(x) %in% old_questions)
    x <- x[-old_pos]
    if (!missing(new_questions))
        x <- append(x, new_questions, after = min(old_pos) - 1)
    return(x)
}


questions <- list()

questions[["2010"]] <- list()

questions[["2010"]][["background"]] <-
    c(q1000 = "postcode",
      q1001 = "gender",
      q1002 = "birthyear",
      q1005 = "work.postcode",
      q2000 = "where.spend.time",
      q2000_1 = "where.spend.time.school",
      q2000_2 = "where.spend.time.work",
      q2000_3 = "where.spend.time.home",
      q2000_4 = "where.spend.time.other",
      q2001 = "transport",
      q2001_1 = "transport.bike.motor",
      q2001_2 = "transport.car",
      q2001_3 = "transport.walk",
      q2001_4 = "transport.public",
      q2002 = "howmany.colds",
      q2040 = "vaccine.last.year",
      q2004 = "riskgroup",
      q2005 = "smoke",
      q2008 = "exercise",
      q2009001 = "nb.household",
      q2009002 = "nb.household.0.4",
      q2009003 = "nb.household.5.18",
      q2009004 = "nb.household.19.64",
      q2009005 = "nb.household.65+",
      q2010 = "school.nursery",
      q2011 = "frequent.contact",
      q2004_1 = "chronic.heart.disease",
      q2004_2 = "diabetes",
      q2004_3 = "asthma",
      q2004_4 = "other.chronic.lung.disease",
      q2004_5 = "pregnant",
      q2004_6 = "immunocompromised",
      q2004_7 = "other.chronic",
      q2011_1 = "frequent.contact.children",
      q2011_2 = "frequent.contact.patients",
      q2011_3 = "frequent.contact.elderly",
      q2011_4 = "frequent.contact.people",
      q2041 = "why.vaccine",
      q2041_1 = "why.vaccine.doctor",
      q2041_2 = "why.vaccine.protected",
      q2041_3 = "why.vaccine.protect.others",
      q2041_4 = "why.vaccine.other",
      q2042 = "why.not.vaccine",
      q2042_1 = "why.not.vaccine.doctor",
      q2042_2 = "why.not.vaccine.noteffective",
      q2042_3 = "why.not.vaccine.gives.flu",
      q2042_4 = "why.not.vaccine.side.effects",
      q2042_5 = "why.not.vaccine.other",
      q2042_6 = "why.not.vaccine.norisk",
      q2060 = "how.find.out",
      q2060_1 = "how.find.out.TV",
      q2060_2 = "how.find.out.radio",
      q2060_3 = "how.find.out.newspaper.magazine",
      q2060_4 = "how.find.out.internet",
      q2060_5 = "how.find.out.meeting",
      q2060_6 = "how.find.out.friend",
      q2060_7 = "how.find.out.survey.team",
      q2060_8 = "how.find.out.other")


questions[["2010"]][["vaccine"]] <-
    c(q9001 = "offered.swineflu.vaccine",
      q9002 = "why.offered.swineflu.vaccine",
      q9003 = "swineflu.vaccine.this.year",
      q9004 = "intend.swineflue.vaccine",
      q9005 = "date.swineflu.vaccine",
      q9006 = "why.not.swineflu.vaccine",
      q9007 = "why.swineflu.vaccine",
      q9008 = "offered.seasonal.vaccine",
      q9009 = "why.offered.seasonal.vaccine",
      q9010 = "vaccine.this.year",
      q9011 = "intend.seasonal.vaccine",
      q9012 = "date.vaccine",
      q9013 = "why.not.seasonal.vaccine",
      q9014 = "why.seasonal.vaccine")

questions[["2010"]][["symptom"]] <-
    c(q3000 = "symptoms",
      q3001 = "symptoms.start.date",
      q3002 = "fever.temperature.range",
      q3003 = "fever.start",
      q3004 = "fever.suddenly",
      q3005 = "medical.service.phone",
      q30051 = "medical.service.visit",
      q30052 = "nights.hospital",
      q3006 = "diagnosis",
      q3007 = "alter.routine",
      q3008 = "howlong.altered",
      q3009 = "medication",
      q3010 = "medication.when",
      q3011 = "encountered.flu",
      q3000_1 = "blocked.runny.nose",
      q3000_2 = "cough",
      q3000_3 = "sore.throat",
      q3000_4 = "headache",
      q3000_5 = "muscle.and.or.joint.pain",
      q3000_6 = "chest.pain",
      q3000_7 = "stomach.ache",
      q3000_8 = "diarrhoea",
      q3000_9 = "nausea",
      q3000_10 = "chills",
      q3000_11 = "weakness",
      q3000_12 = "eye.irritation",
      q3000_13 = "fever.symptom",
      q3000_14 = "no.symptoms",
      q3005_1 = "phone.medical.service.gp",
      q3005_2 = "phone.medical.service.hospital",
      q3005_3 = "phone.medical.service.other",
      q3005_4 = "phone.medical.service.no",
      q3005_5 = "phone.medical.service.ae",
      q30051_1 = "visit.medical.service.gp",
      q30051_2 = "visit.medical.service.hospital",
      q30051_3 = "visit.medical.service.other",
      q30051_4 = "visit.medical.service.no",
      q30051_5 = "visit.medical.service.ae",
      q300501 = "howsoon.phone.medical.service",
      q300502 = "howsoon.visit.medical.service",
      q30081 = "still.altered",
      q3009_2 = "medication.painkillers",
      q3009_3 = "medication.cough",
      q3009_4 = "medication.tamiflu",
      q3009_5 = "medication.relenza",
      q3009_6 = "medication.none",
      q3011_1 = "encountered.flu.yes",
      q3011_2 = "encountered.flu.no",
      q3011_3 = "encountered.flu.dontknow")

questions[["2010"]][["contact"]] <-
    c(q4000 = "conversational.home.0-4",
      q4001 = "conversational.home.5-18",
      q4002 = "conversational.home.19-64",
      q4003 = "conversational.home.65+",
      q4004 = "conversational.work.0-4",
      q4005 = "conversational.work.5-18",
      q4006 = "conversational.work.19-64",
      q4007 = "conversational.work.65+",
      q4008 = "conversational.other.0-4",
      q4009 = "conversational.other.5-18",
      q4010 = "conversational.other.19-64",
      q4011 = "conversational.other.65+",
      q4012 = "physical.home.0-4",
      q4013 = "physical.home.5-18",
      q4014 = "physical.home.19-64",
      q4015 = "physical.home.65+",
      q4016 = "physical.work.0-4",
      q4017 = "physical.work.5-18",
      q4018 = "physical.work.19-64",
      q4019 = "physical.work.65+",
      q4020 = "physical.other.0-4",
      q4021 = "physical.other.5-18",
      q4022 = "physical.other.19-64",
      q4023 = "physical.other.65+",
      q4024 = "public.transport",
      q4025 = "enclosed.indoor.space")

questions[["2011"]] <- list()

questions[["2011"]][["background"]] <-
    c(created = "date",
      IntakeQ1 = "gender",
      IntakeQ2 = "birthmonth",
      IntakeQ3 = "postcode",
      IntakeQ4 = "work.postcode",
      IntakeQ5 = "frequent.contact",
      IntakeQ5.0 = "frequent.contact.children",
      IntakeQ5.1 = "frequent.contact.elderly",
      IntakeQ5.2 = "frequent.contact.patients",
      IntakeQ5.3 = "frequent.contact.people",
      IntakeQ6 = "household",
      IntakeQ6.0 = "nb.household.0.4",
      IntakeQ6.1 = "nb.household.5.18",
      IntakeQ6.2 = "nb.household.19.44",
      IntakeQ6.3 = "nb.household.45.64",
      IntakeQ6.4 = "nb.household.65+",
      IntakeQ6b = "children.school",
      IntakeQ7 = "transport",
      IntakeQ7b = "howlong.transport",
      IntakeQ8 = "vaccine.swineflu",
      IntakeQ8b = "date.vaccine.swineflu",
      IntakeQ9 = "vaccine.last.year",
      IntakeQ10 = "vaccine.this.year",
      IntakeQ10b = "date.vaccine",
      IntakeQ10c = "why.vaccine",
      IntakeQ10c.0 = "why.vaccine.riskgroup",
      IntakeQ10c.1 = "why.vaccine.protected",
      IntakeQ10c.2 = "why.vaccine.protect.others",
      IntakeQ10c.3 = "why.vaccine.doctor",
      IntakeQ10c.4 = "why.vaccine.work.recommended",
      IntakeQ10c.5 = "why.vaccine.convenient",
      IntakeQ10c.6 = "why.vaccine.free",
      IntakeQ10c.7 = "why.vaccine.nomiss.work",
      IntakeQ10c.8 = "why.vaccine.always",
      IntakeQ10c.9 = "why.vaccine.other",
      IntakeQ10d = "why.not.vaccine",
      IntakeQ10d.0 = "why.not.vaccine.notyet",
      IntakeQ10d.1 = "why.not.vaccine.norisk",
      IntakeQ10d.2 = "why.not.vaccine.natural",
      IntakeQ10d.3 = "why.not.vaccine.noteffective",
      IntakeQ10d.4 = "why.not.vaccine.minor",
      IntakeQ10d.5 = "why.not.vaccine.cause",
      IntakeQ10d.6 = "why.not.vaccine.side.effects",
      IntakeQ10d.7 = "why.not.vaccine.unavailable",
      IntakeQ10d.8 = "why.not.vaccine.not.free",
      IntakeQ10d.9 = "why.not.vaccine.dislike.injections",
      IntakeQ10d.10 = "why.not.vaccine.no.reason",
      IntakeQ10d.11 = "why.not.vaccine.doctor",
      IntakeQ10d.12 = "why.not.vaccine.notoffered",
      IntakeQ12 = "risk",
      IntakeQ12.0 = "risk.asthma",
      IntakeQ12.1 = "risk.diabetes",
      IntakeQ12.2 = "risk.lung",
      IntakeQ12.3 = "risk.heart",
      IntakeQ12.4 = "risk.kidney",
      IntakeQ12.5 = "risk.immune",
      IntakeQ12.6 = "norisk",
      IntakeQ13 = "pregnant",
      IntakeQ13b = "trimester",
      IntakeQ14 = "smoke",
      IntakeQ15 = "allergy",
      IntakeQ15.0 = "allergy.hayfever",
      IntakeQ15.1 = "allergy.dust",
      IntakeQ15.2 = "allergy.animals",
      IntakeQ15.3 = "allergy.other")

questions[["2011"]][["symptom"]] <-
    c(WeeklyQ1.0 = "no.symptoms",
      WeeklyQ1.1 = "fever",
      WeeklyQ1.2 = "watery.eyes",
      WeeklyQ1.3 = "blocked.runny.nose",
      WeeklyQ1.4 = "sneezing",
      WeeklyQ1.5 = "sore.throat",
      WeeklyQ1.6 = "cough",
      WeeklyQ1.7 = "phlegm",
      WeeklyQ1.8 = "headache",
      WeeklyQ1.9 = "muscle.and.or.joint.pain",
      WeeklyQ1.10 = "chest.pain",
      WeeklyQ1.11 = "tired",
      WeeklyQ1.12 = "loss.appetite",
      WeeklyQ1.13 = "nausea",
      WeeklyQ1.14 = "vomiting",
      WeeklyQ1.15 = "diarrhoea",
      WeeklyQ1.16 = "other.symptoms",
      WeeklyQ1.17 = "chills",
      WeeklyQ1.18 = "shortness.breath",
      WeeklyQ1.19 = "stomach.ache",
      WeeklyQ1b = "symptoms.suddenly",
      WeeklyQ2 = "fever.temperature",
      WeeklyQ2b = "fever.temperature.value",
      WeeklyQ2c = "fever.start",
      WeeklyQ3 = "same",
      WeeklyQ4 = "symptoms.start.date",
      WeeklyQ5 = "symptoms.end.date",
      WeeklyQ6.0 = "visit.medical.service.gp",
      WeeklyQ6.1 = "visit.medical.service.hospital",
      WeeklyQ6.2 = "visit.medical.service.ae",
      WeeklyQ6.3 = "visit.medical.service.other",
      WeeklyQ6.4 = "visit.medical.service.no",
      WeeklyQ6.5 = "visit.medical.service.appointment",
      WeeklyQ6b = "visit.medical.service.howsoon",
      WeeklyQ6c.0 = "contact.medical.service.gp",
      WeeklyQ6c.1 = "contact.medical.service.hospital",
      WeeklyQ6c.2 = "contact.medical.service.ae",
      WeeklyQ6c.3 = "contact.medical.service.other",
      WeeklyQ6c.4 = "contact.medical.service.no",
      WeeklyQ6c.5 = "contact.medical.service.appointment",
      WeeklyQ6d = "contact.medical.service.howsoon",
      WeeklyQ7.0 = "medication.none",
      WeeklyQ7.1 = "medication.painkillers",
      WeeklyQ7.2 = "medication.cough",
      WeeklyQ7.3 = "medication.antiviral",
      WeeklyQ7.4 = "medication.antibiotic",
      WeeklyQ7.5 = "medication.other",
      WeeklyQ7b = "medication.howlong",
      WeeklyQ8 = "alter.routine",
      WeeklyQ8b = "still.altered",
      WeeklyQ8c = "howlong.altered",
      WeeklyQ9a = "howmany.household.ili",
      WeeklyQ9b = "howmany.other.ili",
      WeeklyQ10 = "vaccine.this.year.since.registration",
      WeeklyQ11 = "what.do.you.think")

questions[["2011"]][["contact"]] <-
    c(ContactQ1 = "symptom.conversational",
      ContactQ2 = "symptom.physical",
      ContactQ3 = "symptom.public.transport",
      ContactQ4 = "symptom.indoor.space",
      ContactQ1.0 = "conversational.home.0-4",
      ContactQ1.1 = "conversational.home.5-18",
      ContactQ1.2 = "conversational.home.19-44",
      ContactQ1.3 = "conversational.home.45-64",
      ContactQ1.4 = "conversational.home.65+",
      ContactQ1.5 = "conversational.work.0-4",
      ContactQ1.6 = "conversational.work.5-18",
      ContactQ1.7 = "conversational.work.19-44",
      ContactQ1.8 = "conversational.work.45-64",
      ContactQ1.9 = "conversational.work.65+",
      ContactQ1.10 = "conversational.other.0-4",
      ContactQ1.11 = "conversational.other.5-18",
      ContactQ1.12 = "conversational.other.19-44",
      ContactQ1.13 = "conversational.other.45-64",
      ContactQ1.14 = "conversational.other.65+",
      ContactQ2.0 = "physical.home.0-4",
      ContactQ2.1 = "physical.home.5-18",
      ContactQ2.2 = "physical.home.19-44",
      ContactQ2.3 = "physical.home.45-64",
      ContactQ2.4 = "physical.home.65+",
      ContactQ2.5 = "physical.work.0-4",
      ContactQ2.6 = "physical.work.5-18",
      ContactQ2.7 = "physical.work.19-44",
      ContactQ2.8 = "physical.work.45-64",
      ContactQ2.9 = "physical.work.65+",
      ContactQ2.10 = "physical.other.0-4",
      ContactQ2.11 = "physical.other.5-18",
      ContactQ2.12 = "physical.other.19-44",
      ContactQ2.13 = "physical.other.45-64",
      ContactQ2.14 = "physical.other.65+")

questions[["2012"]] <- list()

questions[["2012"]][["background"]] <-
    c(Q0 = "self",
      Q1 = "gender",
      Q2 = "birthmonth",
      Q3 = "postcode",
      Q4 = "main.activity",
      Q4b = "work.postcode.option",
      Q4b_0_open = "work.postcode",
      Q4c = "occupation",
      Q4d_0 = "no.education",
      Q4d_1 = "education.gcse",
      Q4d_2 = "education.alevels",
      Q4d_3 = "education.bsc",
      Q4d_4 = "education.msc",
      Q4d_5 = "education.stillin",
      Q5_0 = "frequent.contact.children",
      Q5_1 = "frequent.contact.elderly",
      Q5_2 = "frequent.contact.patients",
      Q5_3 = "frequent.contact.people",
      Q5_4 = "frequent.contact.none",
      Q6_0 = "household.0.4",
      Q6_0_open = "nb.household.0.4",
      Q6_1 = "household.5.18",
      Q6_1_open = "nb.household.5.18",
      Q6_2 = "household.19.44",
      Q6_2_open = "nb.household.19.44",
      Q6_3 = "household.45.64",
      Q6_3_open = "nb.household.45.64",
      Q6_4 = "household.65+",
      Q6_4_open = "nb.household.65+",
      Q6b = "children.school",
      Q7 = "transport",
      Q7b = "howlong.transport",
      Q8 = "howoften.flulike",
      Q9 = "vaccine.last.year",
      Q10 = "vaccine.this.year",
      Q10b = "date.vaccine.option",
      Q10b_1_open = "date.vaccine",
      Q10c_0 = "why.vaccine.riskgroup",
      Q10c_1 = "why.vaccine.protected",
      Q10c_2 = "why.vaccine.protect.others",
      Q10c_3 = "why.vaccine.doctor",
      Q10c_4 = "why.vaccine.work.recommended",
      Q10c_5 = "why.vaccine.convenient",
      Q10c_6 = "why.vaccine.free",
      Q10c_7 = "why.vaccine.nomiss.work",
      Q10c_8 = "why.vaccine.always",
      Q10c_9 = "why.vaccine.other",
      Q10d_0 = "why.not.vaccine.notyet",
      Q10d_1 = "why.not.vaccine.notoffered",
      Q10d_2 = "why.not.vaccine.norisk",
      Q10d_3 = "why.not.vaccine.natural",
      Q10d_4 = "why.not.vaccine.noteffective",
      Q10d_5 = "why.not.vaccine.minor",
      Q10d_6 = "why.not.vaccine.unlikely",
      Q10d_7 = "why.not.vaccine.cause",
      Q10d_8 = "why.not.vaccine.side.effects",
      Q10d_9 = "why.not.vaccine.dont.like",
      Q10d_10 = "why.not.vaccine.unavailable",
      Q10d_11 = "why.not.vaccine.not.free",
      Q10d_12 = "why.not.vaccine.no.reason",
      Q10d_13 = "why.not.vaccine.doctor",
      Q10d_14 = "why.not.vaccine.other",
      Q11_0 = "norisk",
      Q11_1 = "risk.asthma",
      Q11_2 = "risk.diabetes",
      Q11_3 = "risk.lung",
      Q11_4 = "risk.heart",
      Q11_5 = "risk.kidney",
      Q11_6 = "risk.immune",
      Q12 = "pregnant",
      Q12b = "pregnant.trimester",
      Q13 = "smoke",
      Q14_1 = "allergy.hayfever",
      Q14_2 = "allergy.dust",
      Q14_3 = "allergy.animals",
      Q14_4 = "allergy.other",
      Q14_5 = "allergy.none",
      Q15_0 = "diet.none",
      Q15_1 = "diet.vegetarian",
      Q15_2 = "diet.vegan",
      Q15_3 = "diet.low-calorie",
      Q15_4 = "diet.other",
      Q16_0 = "pets.none",
      Q16_1 = "pets.dogs",
      Q16_2 = "pets.cats",
      Q16_3 = "pets.birds",
      Q16_4 = "pets.other",
      Q17_0 = "howhear.radio.tv",
      Q17_1 = "howhear.paper.magazine",
      Q17_2 = "howhear.internet",
      Q17_3 = "howhear.poster",
      Q17_4 = "howhear.family.friends",
      Q17_5 = "howhear.other")

questions[["2012"]][["symptom"]] <-
    c(Q1_0 = "no.symptoms",
      Q1_1 = "fever",
      Q1_2 = "chills",
      Q1_3 = "blocked.runny.nose",
      Q1_4 = "sneezing",
      Q1_5 = "sore.throat",
      Q1_6 = "cough",
      Q1_7 = "shortness.breath",
      Q1_8 = "headache",
      Q1_9 = "muscle.and.or.joint.pain",
      Q1_10 = "chest.pain",
      Q1_11 = "tired",
      Q1_12 = "loss.appetite",
      Q1_13 = "phlegm",
      Q1_14 = "watery.eyes",
      Q1_15 = "nausea",
      Q1_16 = "vomiting",
      Q1_17 = "diarrhoea",
      Q1_18 = "stomach.ache",
      Q1_19 = "other.symptoms",
      Q2 = "same",
      Q3 = "symptoms.start.option",
      Q3_0_open = "symptoms.start.date",
      Q4 = "symptoms.end.option",
      Q4_0_open = "symptoms.end.date",
      Q5 = "symptoms.suddenly",
      Q6 = "fever.start.option",
      Q6_1_open = "fever.start",
      Q6b = "fever.suddenly",
      Q6c = "fever.temperature",
      Q6d = "fever.temperature.value",
      Q7_0 = "visit.medical.service.no",
      Q7_1 = "visit.medical.service.gp",
      Q7_2 = "visit.medical.service.ae",
      Q7_3 = "visit.medical.service.hospital",
      Q7_4 = "visit.medical.service.other",
      Q7_5 = "visit.medical.service.appointment",
      Q7b = "visit.medical.service.howsoon",
      Q8_0 = "contact.medical.service.no",
      Q8_1 = "contact.medical.service.gp.receptionist",
      Q8_2 = "contact.medical.service.gp.doctor",
      Q8_3 = "contact.medical.service.nhs",
      Q8_4 = "contact.medical.service.npfs",
      Q8_5 = "contact.medical.service.other",
      Q8b = "contact.medical.service.howsoon",
      Q9_0 = "no.medication",
      Q9_1 = "medication.painkillers",
      Q9_2 = "medication.cough",
      Q9_3 = "medication.antiviral",
      Q9_4 = "medication.antibiotic",
      Q9_5 = "medication.other",
      Q9_6 = "medication.dontknow",
      Q9b = "medication.howsoon",
      Q10 = "alter.routine",
      Q10b = "still.altered",
      Q10c = "howlong.altered",
      Q11 = "what.do.you.think",
      Q12_multi_row1_col1 = "howmany.household.ili",
      Q13_multi_row1_col1 = "howmany.other.ili")

questions[["2012"]][["contact"]] <-
    c(Q1_multi_row1_col1 = "conversational.home.0-4",
      Q1_multi_row1_col2 = "conversational.home.5-18",
      Q1_multi_row1_col3 = "conversational.home.19-44",
      Q1_multi_row1_col4 = "conversational.home.45-64",
      Q1_multi_row1_col5 = "conversational.home.65+",
      Q1_multi_row2_col1 = "conversational.work.0-4",
      Q1_multi_row2_col2 = "conversational.work.5-18",
      Q1_multi_row2_col3 = "conversational.work.19-44",
      Q1_multi_row2_col4 = "conversational.work.45-64",
      Q1_multi_row2_col5 = "conversational.work.65+",
      Q1_multi_row3_col1 = "conversational.other.0-4",
      Q1_multi_row3_col2 = "conversational.other.5-18",
      Q1_multi_row3_col3 = "conversational.other.19-44",
      Q1_multi_row3_col4 = "conversational.other.45-64",
      Q1_multi_row3_col5 = "conversational.other.65+",
      Q2_multi_row1_col1 = "physical.home.0-4",
      Q2_multi_row1_col2 = "physical.home.5-18",
      Q2_multi_row1_col3 = "physical.home.19-44",
      Q2_multi_row1_col4 = "physical.home.45-64",
      Q2_multi_row1_col5 = "physical.home.65+",
      Q2_multi_row2_col1 = "physical.work.0-4",
      Q2_multi_row2_col2 = "physical.work.5-18",
      Q2_multi_row2_col3 = "physical.work.19-44",
      Q2_multi_row2_col4 = "physical.work.45-64",
      Q2_multi_row2_col5 = "physical.work.65+",
      Q2_multi_row3_col1 = "physical.other.0-4",
      Q2_multi_row3_col2 = "physical.other.5-18",
      Q2_multi_row3_col3 = "physical.other.19-44",
      Q2_multi_row3_col4 = "physical.other.45-64",
      Q2_multi_row3_col5 = "physical.other.65+",
      Q3 = "public.transport",
      Q4 = "enclosed.indoor.space",
      Q5 = "furthest.travelled")

questions[["2013"]] <- questions[["2012"]]

## changes 2012->2013
questions[["2013"]][["symptom"]] <-
    header_replace(questions[["2013"]][["symptom"]], "Q7b",
                   c(Q7b_multi_row1_col1 = "visit.medical.service.howsoon.gp.receptionist",
                     Q7b_multi_row2_col1 = "visit.medical.service.howsoon.gp.doctor.nurse",
                     Q7b_multi_row3_col1 = "visit.medical.service.howsoon.nhs",
                     Q7b_multi_row4_col1 = "visit.medical.service.howsoon.other"))

questions[["2013"]][["symptom"]] <-
    header_replace(questions[["2013"]][["symptom"]], "Q8b",
                   c(Q8b_multi_row1_col1 = "contact.medical.service.howsoon.gp.receptionist",
                     Q8b_multi_row2_col1 = "contact.medical.service.howsoon.gp.doctor.nurse",
                     Q8b_multi_row3_col1 = "contact.medical.service.howsoon.nhs",
                     Q8b_multi_row4_col1 = "contact.medical.service.howsoon.other"))

questions[["2013"]][["symptom"]] <-
    header_replace(questions[["2013"]][["symptom"]],
                   c("Q12_multi_row1_col1", "Q13_multi_row1_col1"),
                   c(Q12 = "health.score"))

questions[["2014"]] <- questions[["2013"]]

## changes 2013->2014
questions[["2014"]][["background"]] <-
    header_replace(questions[["2014"]][["background"]],
                   c("Q4d_0", "Q4d_1", "Q4d_2", "Q4d_3", "Q4d_4", "Q4d_5"),
                   c(Q4d = "education"))


questions[["2014"]][["background"]] <-
    header_replace(questions[["2014"]][["background"]],
                   c("Q10c_0", "Q10c_1", "Q10c_2", "Q10c_3", "Q10c_4", "Q10c_5", "Q10c_6", "Q10c_7", "Q10c_8", "Q10c_9"),
                   c(Q10c_0 = "why.vaccine.riskgroup",
                     Q10c_1 = "why.vaccine.protected",
                     Q10c_2 = "why.vaccine.protect.others",
                     Q10c_3 = "why.vaccine.given.at.school",
                     Q10c_4 = "why.vaccine.doctor",
                     Q10c_5 = "why.vaccine.work.recommended",
                     Q10c_6 = "why.vaccine.convenient",
                     Q10c_7 = "why.vaccine.free",
                     Q10c_8 = "why.vaccine.nomiss.work",
                     Q10c_9 = "why.vaccine.always",
                     Q10c_10 = "why.vaccine.other"))

questions[["2014"]][["background"]] <-
    header_replace(questions[["2014"]][["background"]],
                   c("Q17_0", "Q17_1", "Q17_2", "Q17_3", "Q17_4", "Q17_5"),
                   c(Q18_0 = "howhear.radio.tv",
                     Q18_1 = "howhear.paper.magazine",
                     Q18_2 = "howhear.internet",
                     Q18_3 = "howhear.poster",
                     Q18_4 = "howhear.school.work",
                     Q18_5 = "howhear.bsa",
                     Q18_6 = "howhear.family.friends",
                     Q18 = "howhear.who",
                     Q19a = "activity.vigorous",
                     Q19b = "activity.moderate",
                     Q19c = "activity.winter"))

questions[["2014"]][["background"]] <-
    header_replace(questions[["2014"]][["background"]],
                   c("Q15_4"))

questions[["2015"]] <- questions[["2014"]]

questions[["2015"]][["background"]] <-
    header_replace(questions[["2015"]][["background"]],
                   c("Q8", "Q9"))

questions[["2015"]][["background"]] <-
    header_replace(questions[["2015"]][["background"]],
                   c("Q19a", "Q19b", "Q19c"),
                   c(Q19 = "smart.phone",
                     Q20 = "self.swabbing"))

questions[["2016"]] <- questions[["2015"]]
questions[["2016"]][["symptom"]] <-
  header_replace(questions[["2016"]][["symptom"]],
                    c("Q2", "Q12"))

questions[["2017"]] <- questions[["2016"]]

options <-
    list(self = c("0" = "self", "1" = "household_member", "2" = "someone_else"),
         gender = c("0" = "male", "1" = "female"),
         main.activity = c("0" = "paid_employment_full_time", "1" = "paid_employment_part_time", "2" = "self_employed", "3" = "school", "4" = "home_maker", "5" = "unemployed", "6" = "long_term_leave", "7" = "retired", "8" = "other"),
         work.postcode.option = c("0" = "yes", "1" = "dont_know", "2" = NA),
         occupation = c("0" = "professional", "1" = "office_worker", "2" = "retail", "3" = "skilled_manual", "4" = "other_manual", "5" = "other"),
         children.school = c("0" = "none", "1" = "1", "2" = "2", "3" = "3", "4" = "4", "5" = "5", "6" = "(5,)"), 
         transport = c("0" = "walking", "1" = "bike", "2" = "motorbike", "3" = "car", "4" = "public_transport", "5" = "other"),
         howlong.transport = c("0" = "none", "1" = "(0,30)_minutes", "2" = "[30,90)_minutes", "3" = "[90,360)_minutes", "4" = "(360,)_minutes"),
         howoften.flulike = c("0" = "almost_never", "1" = "(0,2]_annually", "2" = "(2,5]_annually", "3" = "(5-10]_annually", "4" = "(10,)_annually", "5" = "dont_know"),
         vaccine.last.year = c("0" = "yes", "1" = "no", "2" = "dont_know"),
         vaccine.this.year = c("0" = "yes", "1" = "no", "2" = "dont_know"),
         date.vaccine.option = c("0" = "dont_know", "1" = "yes"),
         pregnant = c("0" = "yes", "1" = "no", "2" = "dont_know"),
         smoke = c("0" = "no", "1" = "occasionally", "2" = "(,10]_daily", "3" = "(10,)_daily", "4" = "dont_know"),
         smart.phone = c("0" = "yes", "1" = "no"),
         self.swabbing = c("0" = "yes", "1" = "no"), 
         same = c("0" = "yes", "1" = "no", "2" = "dont_know", "3" = NA),
         symptoms.start.option = c("0" = "date", "1" = "dont_know"),
         symptoms.end.option = c("0" = "date", "1" = "dont_know", "2" = "still_ill"),
         symptoms.suddenly = c("0" = "yes", "1" = "no", "2" = "dont_know"),
         fever.start.option = c("1" = "date", "2" = "dont_know"),
         fever.suddenly = c("0" = "yes", "1" = "no", "2" = "dont_know"),
         fever.temperature = c("0" = "yes", "1" = "no", "2" = "dont_know"),
         fever.temperature.value = c("0" = "[,37)", "1" = "[37,37.5)", "2" = "[37.5,38)", "3" = "[38,39)", "4" = "[39,40)", "5" = "[40,)", "6" = "dont_know"),
         visit.medical.service.howsoon.gp.receptionist = c("100" = NA, "0" = "same_day", "1" = "1_day", "2" = "2_days", "3" = "3_days", "4" = "4_days", "5" = "[5,7]_days", "6" = "(7,)_days", "7" = "dont_know"),
         visit.medical.service.howsoon.gp.doctor.nurse = c("100" = NA, "0" = "same_day", "1" = "1_day", "2" = "2_days", "3" = "3_days", "4" = "4_days", "5" = "[5,7]_days", "6" = "(7,)_days", "7" = "dont_know"),
         visit.medical.service.howsoon.nhs = c("100" = NA, "0" = "same_day", "1" = "1_day", "2" = "2_days", "3" = "3_days", "4" = "4_days", "5" = "[5,7]_days", "6" = "(7,)_days", "7" = "dont_know"),
         visit.medical.service.howsoon.other = c("100" = NA, "0" = "same_day", "1" = "1_day", "2" = "2_days", "3" = "3_days", "4" = "4_days", "5" = "[5,7]_days", "6" = "(7,)_days", "7" = "dont_know"),
         contact.medical.service.howsoon.gp.receptionist = c("100" = NA, "0" = "same_day", "1" = "1_day", "2" = "2_days", "3" = "3_days", "4" = "4_days", "5" = "[5,7]_days", "6" = "(7,)_days", "7" = "dont_know"),
         contact.medical.service.howsoon.gp.doctor.nurse = c("100" = NA, "0" = "same_day", "1" = "1_day", "2" = "2_days", "3" = "3_days", "4" = "4_days", "5" = "[5,7]_days", "6" = "(7,)_days", "7" = "dont_know"),
         contact.medical.service.howsoon.nhs = c("100" = NA, "0" = "same.day", "1" = "1_day", "2" = "2_days", "3" = "3_days", "4" = "4_days", "5" = "[5,7]_days", "6" = "(7,)_days", "7" = "dont_know"),
         contact.medical.service.howsoon.other = c("100" = NA, "0" = "same_day", "1" = "1_day", "2" = "2_days", "3" = "3_days", "4" = "4_days", "5" = "[5,7]_days", "6" = "(7,)_days", "7" = "dont_know"),
         howsoon.medication = c("0" = "same_day", "1" = "1_day", "2" = "2_days", "3" = "3_days", "4" = "4_days", "5" = "[5,7]_days", "6" = "(7,)_days", "7" = "dont_know"),
         alter.routine = c("0" = "no", "1" = "yes_but_no_time_off", "2" = "yes_time_off"),
         still.altered = c("0" = "yes", "1" = "no", "3" = "other"),
         howlong.altered = c("0" = "1_day", "1" = "2_days", "2" = "3_days", "3" = "4_days", "4" = "5_days", "5" = "(5,10]_days", "6" = "(10,15]_days", "7" = "(15,) days"),
         what.do.you.think = c("0" = "ili", "1" = "common_cold", "2" = "allergy_hay_fever", "3" = "gastro", "4" = "other", "5" = "dont_know", "6" = "asthma"),
         public.transport = c("0" = "none", "1" = "(0,30]_minutes", "2" = "(30,90]_minutes", "3" = "(90,240]_minutes", "4" = "(240,)_minutes"),
         enclosed.indoor.space = c("0" = "none", "1" = "(0,30]_minutes", "2" = "(30,90]_minutes", "3" = "(90,240]_minutes", "4" = "(240,)_minutes"),
         furthest.travelled =  c("0" = "[0,1)_miles", "1" = "[1,5)_miles", "2" = "[5,10)_miles", "3" = "[10,30)_miles", "4" = "[30,100]_miles", "5" = "(100,)_miles"))

