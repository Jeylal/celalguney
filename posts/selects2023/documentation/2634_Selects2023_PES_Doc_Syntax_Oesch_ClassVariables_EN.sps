* Encoding: UTF-8.
*********************************************************************************************************************************************
* OESCH CLASS SCHEMA
* Create 16-Class schema, 8-Class schema and 5-Class schema
* Author: Amal Tawfik, University of Lausanne & HESAV
*********************************************************************************************************************************************

**** References:
**** Oesch, D. (2006a) "Coming to grips with a changing class structure" International Sociology 21 (2): 263-288.
**** Oesch, D. (2006b) "Redrawing the Class Map. Stratification and Institutions in Britain, Germany, Sweden and Switzerland", Basingstoke: Palgrave Macmillan.
**** A few minor changes were made with respect to the procedure described in these two sources (decisions taken by Oesch and Tawfik in 2014)

**** 16-Class schema constructed
  *1 Large employers
  *2 Self-employed professionals
  *3 Small business owners with employees
  *4 Small business owners without employees
  *5 Technical experts
  *6 Technicians
  *7 Skilled manual
  *8 Low-skilled manual
  *9 Higher-grade managers and administrators
  *10 Lower-grade managers and administrators
  *11 Skilled clerks
  *12 Unskilled clerks
  *13 Socio-cultural professionals
  *14 Socio-cultural semi-professionals
  *15 Skilled service
  *16 Low-skilled service

**** 8-Class schema constructed
  *1 Self-employed professionals and large employers
  *2 Small business owners
  *3 Technical (semi-)professionals
  *4 Production workers
  *5 (Associate) managers
  *6 Clerks
  *7 Socio-cultural (semi-)professionals
  *8 Service workers

**** 5-Class schema constructed
  *1 Higher-grade service class
  *2 Lower-grade service class
  *3 Small business owners
  *4 Skilled workers
  *5 Unskilled workers

**** Variables used to construct Oesch class schema: 
**** ISCO08prof_r, f21500_f24900, f21550_f24950, f21750_f25150
**** ISCO08prof_p, f22100_f22500, f22150_f22550, f22350_f22750
**** ISCO08prof_me, f23100_f23500, f23150_f23550, f23350_f23750


****************************************************************************************
* Respondent's Oesch class position
* Recode and create variables used to construct class variable for respondents
* Variables used to construct class variable for respondents: ISCO08prof_r, f21500_f24900, f21550_f24950, f21750_f25150
****************************************************************************************

**** Recode occupation variable (isco08 4-digit) for respondents
 
fre ISCO08prof_r.

recode ISCO08prof_r (sysmis, -99, 9997=-9) (else=copy) into isco_mainjob.
variable labels isco_mainjob "Current occupation of respondent - isco08 4-digit" .
fre isco_mainjob.


**** Recode employment status for respondents

fre f21500_f24900 f21550_f24950.

recode f21500_f24900 (1, 2, 3=1)(4=2)(else=9) into emplrel_r.
value labels emplrel_r
1"Employee"
2"Self-employed"
9"Missing".
fre emplrel_r.

recode f21550_f24950 (1 thru 9=1)(10 thru 6000=2)(else=0) into emplno_r.
value labels emplno_r
0"0 employees"
1"1-9 employees"
2"10+ employees".
fre emplno_r.


if (emplrel_r = 1 or emplrel_r = 9) selfem_mainjob = 1.
if (emplrel_r = 2 and emplno_r = 0) selfem_mainjob = 2.
if (emplrel_r = 2 and emplno_r = 1) selfem_mainjob = 3.
if (emplrel_r = 2 and emplno_r = 2) selfem_mainjob = 4.
variable labels selfem_mainjob "Employment status for respondants".
value labels selfem_mainjob
1 "Not self-employed"
2 "Self-empl without employees"
3 "Self-empl with 1-9 employees"
4 "Self-empl with 10 or more".
fre selfem_mainjob.


**** Recode activity sector for respondents

fre f21750_f25150.

compute sector_act_r= f21750_f25150.
fre sector_act_r.

********************************************
* Create Oesch class schema for respondents 
********************************************

compute class16_r = -9.

/* Large employers (1)

if (selfem_mainjob=4) class16_r=1.

/* Self-employed professionals (2)

if (selfem_mainjob=2 or selfem_mainjob=3) and (isco_mainjob >= 2000 and isco_mainjob <= 2162) class16_r=2.
if (selfem_mainjob=2 or selfem_mainjob=3) and (isco_mainjob >= 2164 and isco_mainjob <= 2165) class16_r=2.
if (selfem_mainjob=2 or selfem_mainjob=3) and (isco_mainjob >= 2200 and isco_mainjob <= 2212) class16_r=2.
if (selfem_mainjob=2 or selfem_mainjob=3) and (isco_mainjob = 2250) class16_r=2.
if (selfem_mainjob=2 or selfem_mainjob=3) and (isco_mainjob >= 2261 and isco_mainjob <= 2262) class16_r=2.
if (selfem_mainjob=2 or selfem_mainjob=3) and (isco_mainjob >= 2300 and isco_mainjob <= 2330) class16_r=2.
if (selfem_mainjob=2 or selfem_mainjob=3) and (isco_mainjob >= 2350 and isco_mainjob <= 2352) class16_r=2.
if (selfem_mainjob=2 or selfem_mainjob=3) and (isco_mainjob >= 2359 and isco_mainjob <= 2432) class16_r=2.
if (selfem_mainjob=2 or selfem_mainjob=3) and (isco_mainjob >= 2500 and isco_mainjob <= 2619) class16_r=2.
if (selfem_mainjob=2 or selfem_mainjob=3) and (isco_mainjob = 2621) class16_r=2.
if (selfem_mainjob=2 or selfem_mainjob=3) and (isco_mainjob >= 2630 and isco_mainjob <= 2634) class16_r=2.
if (selfem_mainjob=2 or selfem_mainjob=3) and (isco_mainjob >= 2636 and isco_mainjob <= 2640) class16_r=2.
if (selfem_mainjob=2 or selfem_mainjob=3) and (isco_mainjob >= 2642 and isco_mainjob <= 2643) class16_r=2.

/* Small business owners with employees (3)

if (selfem_mainjob=3) and (isco_mainjob >= 1000 and isco_mainjob <= 1439) class16_r=3.
if (selfem_mainjob=3) and (isco_mainjob = 2163) class16_r=3.
if (selfem_mainjob=3) and (isco_mainjob = 2166) class16_r=3.
if (selfem_mainjob=3) and (isco_mainjob >= 2220 and isco_mainjob <= 2240) class16_r=3.
if (selfem_mainjob=3) and (isco_mainjob = 2260) class16_r=3.
if (selfem_mainjob=3) and (isco_mainjob >= 2263 and isco_mainjob <= 2269) class16_r=3.
if (selfem_mainjob=3) and (isco_mainjob >= 2340 and isco_mainjob <= 2342) class16_r=3.
if (selfem_mainjob=3) and (isco_mainjob >= 2353 and isco_mainjob <= 2356) class16_r=3.
if (selfem_mainjob=3) and (isco_mainjob >= 2433 and isco_mainjob <= 2434) class16_r=3.
if (selfem_mainjob=3) and (isco_mainjob = 2620) class16_r=3.
if (selfem_mainjob=3) and (isco_mainjob = 2622) class16_r=3.
if (selfem_mainjob=3) and (isco_mainjob = 2635) class16_r=3.
if (selfem_mainjob=3) and (isco_mainjob = 2641) class16_r=3.
if (selfem_mainjob=3) and (isco_mainjob >= 2650 and isco_mainjob <= 2659) class16_r=3.
if (selfem_mainjob=3) and (isco_mainjob >= 3000 and isco_mainjob <= 9629) class16_r=3.

/* Small business owners without employees (4)

if (selfem_mainjob=2) and (isco_mainjob >= 1000 and isco_mainjob <= 1439) class16_r=4.
if (selfem_mainjob=2) and (isco_mainjob = 2163) class16_r=4.
if (selfem_mainjob=2) and (isco_mainjob = 2166) class16_r=4.
if (selfem_mainjob=2) and (isco_mainjob >= 2220 and isco_mainjob <= 2240) class16_r=4.
if (selfem_mainjob=2) and (isco_mainjob = 2260) class16_r=4.
if (selfem_mainjob=2) and (isco_mainjob >= 2263 and isco_mainjob <= 2269) class16_r=4.
if (selfem_mainjob=2) and (isco_mainjob >= 2340 and isco_mainjob <= 2342) class16_r=4.
if (selfem_mainjob=2) and (isco_mainjob >= 2353 and isco_mainjob <= 2356) class16_r=4.
if (selfem_mainjob=2) and (isco_mainjob >= 2433 and isco_mainjob <= 2434) class16_r=4.
if (selfem_mainjob=2) and (isco_mainjob = 2620) class16_r=4.
if (selfem_mainjob=2) and (isco_mainjob = 2622) class16_r=4.
if (selfem_mainjob=2) and (isco_mainjob = 2635) class16_r=4.
if (selfem_mainjob=2) and (isco_mainjob = 2641) class16_r=4.
if (selfem_mainjob=2) and (isco_mainjob >= 2650 and isco_mainjob <= 2659) class16_r=4.
if (selfem_mainjob=2) and (isco_mainjob >= 3000 and isco_mainjob <= 9629) class16_r=4.

/* Technical experts (5)

if (selfem_mainjob=1) and (isco_mainjob >= 2100 and  isco_mainjob <= 2162) class16_r=5 .
if (selfem_mainjob=1) and (isco_mainjob >= 2164 and  isco_mainjob <= 2165) class16_r=5 .
if (selfem_mainjob=1) and (isco_mainjob >= 2500 and  isco_mainjob <= 2529) class16_r=5 .

/* Technicians (6)

if (selfem_mainjob=1) and (isco_mainjob >= 3100 and  isco_mainjob <= 3155) class16_r=6 .
if (selfem_mainjob=1) and (isco_mainjob >= 3210 and  isco_mainjob <= 3214) class16_r=6 .
if (selfem_mainjob=1) and (isco_mainjob = 3252) class16_r=6 .
if (selfem_mainjob=1) and (isco_mainjob >= 3500 and  isco_mainjob <= 3522) class16_r=6 .

/* Skilled manual (7)

if (selfem_mainjob=1) and (isco_mainjob >= 6000 and  isco_mainjob <= 7549) class16_r=7 .
if (selfem_mainjob=1) and (isco_mainjob >= 8310 and  isco_mainjob <= 8312) class16_r=7 .
if (selfem_mainjob=1) and (isco_mainjob = 8330) class16_r=7 .
if (selfem_mainjob=1) and (isco_mainjob >= 8332 and  isco_mainjob <= 8340) class16_r=7 .
if (selfem_mainjob=1) and (isco_mainjob >= 8342 and  isco_mainjob <= 8344) class16_r=7 .

/* Low-skilled manual (8)

if (selfem_mainjob=1) and (isco_mainjob >= 8000 and  isco_mainjob <= 8300) class16_r=8 .
if (selfem_mainjob=1) and (isco_mainjob >= 8320 and  isco_mainjob <= 8321) class16_r=8 .
if (selfem_mainjob=1) and (isco_mainjob = 8341) class16_r=8 .
if (selfem_mainjob=1) and (isco_mainjob = 8350) class16_r=8 .
if (selfem_mainjob=1) and (isco_mainjob >= 9200 and  isco_mainjob <= 9334) class16_r=8 .
if (selfem_mainjob=1) and (isco_mainjob >= 9600 and  isco_mainjob <= 9620) class16_r=8 .
if (selfem_mainjob=1) and (isco_mainjob >= 9622 and  isco_mainjob <= 9629) class16_r=8 .

/* Higher-grade managers and administrators (9)

if (selfem_mainjob=1) and (isco_mainjob >= 1000 and  isco_mainjob <= 1300) class16_r=9 .
if (selfem_mainjob=1) and (isco_mainjob >= 1320 and  isco_mainjob <= 1349) class16_r=9 .
if (selfem_mainjob=1) and (isco_mainjob >= 2400 and  isco_mainjob <= 2432) class16_r=9 .
if (selfem_mainjob=1) and (isco_mainjob >= 2610 and  isco_mainjob <= 2619) class16_r=9 .
if (selfem_mainjob=1) and (isco_mainjob = 2631) class16_r=9 .
if (selfem_mainjob=1) and (isco_mainjob >= 100 and  isco_mainjob <= 110) class16_r=9 .

/* Lower-grade managers and administrators (10)

if (selfem_mainjob=1) and (isco_mainjob >= 1310 and  isco_mainjob <= 1312) class16_r=10 .
if (selfem_mainjob=1) and (isco_mainjob >= 1400 and  isco_mainjob <= 1439) class16_r=10 .
if (selfem_mainjob=1) and (isco_mainjob >= 2433 and  isco_mainjob <= 2434) class16_r=10 .
if (selfem_mainjob=1) and (isco_mainjob >= 3300 and  isco_mainjob <= 3339) class16_r=10 .
if (selfem_mainjob=1) and (isco_mainjob = 3343) class16_r=10 .
if (selfem_mainjob=1) and (isco_mainjob >= 3350 and  isco_mainjob <= 3359) class16_r=10 .
if (selfem_mainjob=1) and (isco_mainjob = 3411) class16_r=10 .
if (selfem_mainjob=1) and (isco_mainjob = 5221) class16_r=10 .
if (selfem_mainjob=1) and (isco_mainjob >= 200 and  isco_mainjob <= 210) class16_r=10 .

/* Skilled clerks (11)

if (selfem_mainjob=1) and (isco_mainjob >= 3340 and  isco_mainjob <= 3342) class16_r=11 .
if (selfem_mainjob=1) and (isco_mainjob = 3344) class16_r=11 .
if (selfem_mainjob=1) and (isco_mainjob >= 4000 and  isco_mainjob <= 4131) class16_r=11 .
if (selfem_mainjob=1) and (isco_mainjob >= 4200 and  isco_mainjob <= 4221) class16_r=11 .
if (selfem_mainjob=1) and (isco_mainjob >= 4224 and  isco_mainjob <= 4413) class16_r=11 .
if (selfem_mainjob=1) and (isco_mainjob >= 4415 and  isco_mainjob <= 4419) class16_r=11 .

/* Unskilled clerks (12)

if (selfem_mainjob=1) and (isco_mainjob = 4132) class16_r=12 .
if (selfem_mainjob=1) and (isco_mainjob = 4222) class16_r=12 .
if (selfem_mainjob=1) and (isco_mainjob = 4223) class16_r=12 .
if (selfem_mainjob=1) and (isco_mainjob = 5230) class16_r=12 .
if (selfem_mainjob=1) and (isco_mainjob = 9621) class16_r=12 .

/* Socio-cultural professionals (13)

if (selfem_mainjob=1) and (isco_mainjob >= 2200 and  isco_mainjob <= 2212) class16_r=13 .
if (selfem_mainjob=1) and (isco_mainjob = 2250) class16_r=13 .
if (selfem_mainjob=1) and (isco_mainjob >= 2261 and  isco_mainjob <= 2262) class16_r=13 .
if (selfem_mainjob=1) and (isco_mainjob >= 2300 and  isco_mainjob <= 2330) class16_r=13 .
if (selfem_mainjob=1) and (isco_mainjob >= 2350 and  isco_mainjob <= 2352) class16_r=13 .
if (selfem_mainjob=1) and (isco_mainjob = 2359) class16_r=13 .
if (selfem_mainjob=1) and (isco_mainjob = 2600) class16_r=13 .
if (selfem_mainjob=1) and (isco_mainjob = 2621) class16_r=13 .
if (selfem_mainjob=1) and (isco_mainjob = 2630) class16_r=13 .
if (selfem_mainjob=1) and (isco_mainjob >= 2632 and  isco_mainjob <= 2634) class16_r=13 .
if (selfem_mainjob=1) and (isco_mainjob >= 2636 and  isco_mainjob <= 2640) class16_r=13 .
if (selfem_mainjob=1) and (isco_mainjob >= 2642 and  isco_mainjob <= 2643) class16_r=13 .

/* Socio-cultural semi-professionals (14)

if (selfem_mainjob=1) and (isco_mainjob = 2163) class16_r=14 .
if (selfem_mainjob=1) and (isco_mainjob = 2166) class16_r=14 .
if (selfem_mainjob=1) and (isco_mainjob >= 2220 and  isco_mainjob <= 2240) class16_r=14 .
if (selfem_mainjob=1) and (isco_mainjob = 2260) class16_r=14 .
if (selfem_mainjob=1) and (isco_mainjob >= 2263 and  isco_mainjob <= 2269) class16_r=14 .
if (selfem_mainjob=1) and (isco_mainjob >= 2340 and  isco_mainjob <= 2342) class16_r=14 .
if (selfem_mainjob=1) and (isco_mainjob >= 2353 and  isco_mainjob <= 2356) class16_r=14 .
if (selfem_mainjob=1) and (isco_mainjob = 2620) class16_r=14 .
if (selfem_mainjob=1) and (isco_mainjob = 2622) class16_r=14 .
if (selfem_mainjob=1) and (isco_mainjob = 2635) class16_r=14 .
if (selfem_mainjob=1) and (isco_mainjob = 2641) class16_r=14 .
if (selfem_mainjob=1) and (isco_mainjob >= 2650 and  isco_mainjob <= 2659) class16_r=14 .
if (selfem_mainjob=1) and (isco_mainjob = 3200) class16_r=14 .
if (selfem_mainjob=1) and (isco_mainjob >= 3220 and  isco_mainjob <= 3230) class16_r=14 .
if (selfem_mainjob=1) and (isco_mainjob = 3250) class16_r=14 .
if (selfem_mainjob=1) and (isco_mainjob >= 3253 and  isco_mainjob <= 3257) class16_r=14 .
if (selfem_mainjob=1) and (isco_mainjob = 3259) class16_r=14 .
if (selfem_mainjob=1) and (isco_mainjob >= 3400 and  isco_mainjob <= 3410) class16_r=14 .
if (selfem_mainjob=1) and (isco_mainjob >= 3412 and  isco_mainjob <= 3413) class16_r=14 .
if (selfem_mainjob=1) and (isco_mainjob >= 3430 and  isco_mainjob <= 3433) class16_r=14 .
if (selfem_mainjob=1) and (isco_mainjob = 3435) class16_r=14 .
if (selfem_mainjob=1) and (isco_mainjob = 4414) class16_r=14 .

/* Skilled service (15)

if (selfem_mainjob=1) and (isco_mainjob = 3240) class16_r=15 .
if (selfem_mainjob=1) and (isco_mainjob = 3251) class16_r=15 .
if (selfem_mainjob=1) and (isco_mainjob = 3258) class16_r=15 .
if (selfem_mainjob=1) and (isco_mainjob >= 3420 and  isco_mainjob <= 3423) class16_r=15 .
if (selfem_mainjob=1) and (isco_mainjob = 3434) class16_r=15 .
if (selfem_mainjob=1) and (isco_mainjob >= 5000 and  isco_mainjob <= 5120) class16_r=15 .
if (selfem_mainjob=1) and (isco_mainjob >= 5140 and  isco_mainjob <= 5142) class16_r=15 .
if (selfem_mainjob=1) and (isco_mainjob = 5163) class16_r=15 .
if (selfem_mainjob=1) and (isco_mainjob = 5165) class16_r=15 .
if (selfem_mainjob=1) and (isco_mainjob = 5200) class16_r=15 .
if (selfem_mainjob=1) and (isco_mainjob = 5220) class16_r=15 .
if (selfem_mainjob=1) and (isco_mainjob >= 5222 and  isco_mainjob <= 5223) class16_r=15 .
if (selfem_mainjob=1) and (isco_mainjob >= 5241 and  isco_mainjob <= 5242) class16_r=15 .
if (selfem_mainjob=1) and (isco_mainjob >= 5300 and  isco_mainjob <= 5321) class16_r=15 .
if (selfem_mainjob=1) and (isco_mainjob >= 5400 and  isco_mainjob <= 5413) class16_r=15 .
if (selfem_mainjob=1) and (isco_mainjob = 5419) class16_r=15 .
if (selfem_mainjob=1) and (isco_mainjob = 8331) class16_r=15 .

/* Low-skilled service (16)

if (selfem_mainjob=1) and (isco_mainjob >= 5130 and  isco_mainjob <= 5132) class16_r=16 .
if (selfem_mainjob=1) and (isco_mainjob >= 5150 and  isco_mainjob <= 5162) class16_r=16 .
if (selfem_mainjob=1) and (isco_mainjob = 5164) class16_r=16 .
if (selfem_mainjob=1) and (isco_mainjob = 5169) class16_r=16 .
if (selfem_mainjob=1) and (isco_mainjob >= 5210 and  isco_mainjob <= 5212) class16_r=16 .
if (selfem_mainjob=1) and (isco_mainjob = 5240) class16_r=16 .
if (selfem_mainjob=1) and (isco_mainjob >= 5243 and  isco_mainjob <= 5249) class16_r=16 .
if (selfem_mainjob=1) and (isco_mainjob >= 5322 and  isco_mainjob <= 5329) class16_r=16 .
if (selfem_mainjob=1) and (isco_mainjob = 5414) class16_r=16 .
if (selfem_mainjob=1) and (isco_mainjob = 8322) class16_r=16 .
if (selfem_mainjob=1) and (isco_mainjob >= 9100 and  isco_mainjob <= 9129) class16_r=16 .
if (selfem_mainjob=1) and (isco_mainjob >= 9400 and  isco_mainjob <= 9520) class16_r=16 .

do if (class16_r=-9).
if (isco_mainjob=0310) class16_r=10.
if (isco_mainjob=2000) and (sector_act_r=1 or sector_act_r=5 or sector_act_r=10 or sector_act_r=12 or sector_act_r=14) class16_r=5.
if (isco_mainjob=2000) and (sector_act_r=2 or sector_act_r=7 or sector_act_r=8 or sector_act_r=9  or sector_act_r=11) class16_r=9.
if (isco_mainjob=2000) and (sector_act_r=3 or sector_act_r=4 or sector_act_r=6 or sector_act_r=13) class16_r=13.
if (isco_mainjob=3000) and (sector_act_r=1 or sector_act_r=5 or sector_act_r=10 or sector_act_r=12 or sector_act_r=14) class16_r=6.
if (isco_mainjob=3000) and (sector_act_r=2 or sector_act_r=7 or sector_act_r=8 or sector_act_r=9  or sector_act_r=11) class16_r=10.
if (isco_mainjob=3000) and (sector_act_r=3 or sector_act_r=4 or sector_act_r=6 or sector_act_r=13) class16_r=14.
if (isco_mainjob=9000) and (sector_act_r=1 or sector_act_r=5 or sector_act_r=10 or sector_act_r=12 or sector_act_r=14) class16_r=8.
if (isco_mainjob=9000) and (sector_act_r=2 or sector_act_r=7 or sector_act_r=8 or sector_act_r=9  or sector_act_r=11) class16_r=12.
if (isco_mainjob=9000) and (sector_act_r=3 or sector_act_r=4 or sector_act_r=6 or sector_act_r=13) class16_r=16.
end if.


recode class16_r (-9=sysmis)(else=copy).
variable labels class16_r "Respondent's Oesch class position - 16 classes" .
value labels class16_r 
1 "Large employers"
2 "Self-employed professionals"
3 "Small business owners with employees"
4 "Small business owners without employees"
5 "Technical experts"
6 "Technicians"
7 "Skilled manual"
8 "Low-skilled manual"
9 "Higher-grade managers and administrators"
10 "Lower-grade managers and administrators"
11 "Skilled clerks"
12 "Unskilled clerks"
13 "Socio-cultural professionals"
14 "Socio-cultural semi-professionals"
15 "Skilled service"
16 "Low-skilled service".
formats class16_r (f1).
fre class16_r.

recode class16_r (1,2=1)(3,4=2)(5,6=3)(7,8=4)(9,10=5)(11,12=6)(13,14=7)(15,16=8) into class8_r.
variable labels class8_r "Respondent's Oesch class position - 8 classes".
value labels class8_r
1 "Self-employed professionals and large employers"
2 "Small business owners"
3 "Technical (semi-)professionals"
4 "Production workers"
5 "(Associate) managers"
6 "Clerks"
7 "Socio-cultural (semi-)professionals"
8 "Service workers".
formats class8_r (f1).
fre class8_r.

recode class16_r (1,2,5,9,13=1)(6,10,14=2)(3,4=3)(7,11,15=4)(8,12,16=5) into class5_r.
variable labels class5_r "Respondent's Oesch class position - 5 classes".
value labels class5_r
1 "Higher-grade service class"
2 "Lower-grade service class"
3 "Small business owners"
4 "Skilled workers"
5 "Unskilled workers".
formats class5_r (f1).
fre class5_r.


***************************************************************************************
* Partner's Oesch class position
* Recode and create variables used to construct class variable for partners
* Variables used to construct class variable for partners: ISCO08prof_p, f22100_f22500, f22150_f22550, f22350_f22750
***************************************************************************************

**** Recode occupation variable (isco88 com 4-digit) for partners

fre ISCO08prof_p.

recode ISCO08prof_p (sysmis, 9997, -99=-9) (else=copy) into isco_partner.
variable labels isco_partner "Current occupation of partner - isco08 4-digit" .
fre isco_partner.


**** Recode employment status for partners

fre f22100_f22500 f22150_f22550.

recode f22100_f22500 (1, 2 ,3=1)(4=2)(else=9) into emplrel_p.
value labels emplrel_p
1"Employee"
2"Self-employed"
9"Missing".
fre emplrel_p.

recode f22150_f22550 (1 thru 9=1)(10 thru 9500=2)(else=0) into emplno_p.
value labels emplno_p
0"0 employees"
1"1-9 employees"
2"10+ employees".
fre emplno_p.

if (emplrel_p = 1 or emplrel_p = 9) selfem_partner = 1.
if (emplrel_p = 2 and emplno_p = 0) selfem_partner = 2.
if (emplrel_p = 2 and emplno_p = 1) selfem_partner = 3.
if (emplrel_p = 2 and emplno_p = 2) selfem_partner = 4.
variable labels selfem_partner "Employment status for partners".
value labels selfem_partner
1 "Not self-employed"
2 "Self-empl without employees"
3 "Self-empl with 1-9 employees"
4 "Self-empl with 10 or more".
fre selfem_partner.


**** Recode activity sector for partners

fre f22350_f22750.

compute sector_act_p= f22350_f22750.
fre sector_act_p.

********************************************
* Create Oesch class schema for partners 
********************************************

compute class16_p = -9.

/* Large employers (1)

if (selfem_partner=4) class16_p=1.

/* Self-employed professionals (2)

if (selfem_partner=2 or selfem_partner=3) and (isco_partner >= 2000 and isco_partner <= 2162) class16_p=2.
if (selfem_partner=2 or selfem_partner=3) and (isco_partner >= 2164 and isco_partner <= 2165) class16_p=2.
if (selfem_partner=2 or selfem_partner=3) and (isco_partner >= 2200 and isco_partner <= 2212) class16_p=2.
if (selfem_partner=2 or selfem_partner=3) and (isco_partner = 2250) class16_p=2.
if (selfem_partner=2 or selfem_partner=3) and (isco_partner >= 2261 and isco_partner <= 2262) class16_p=2.
if (selfem_partner=2 or selfem_partner=3) and (isco_partner >= 2300 and isco_partner <= 2330) class16_p=2.
if (selfem_partner=2 or selfem_partner=3) and (isco_partner >= 2350 and isco_partner <= 2352) class16_p=2.
if (selfem_partner=2 or selfem_partner=3) and (isco_partner >= 2359 and isco_partner <= 2432) class16_p=2.
if (selfem_partner=2 or selfem_partner=3) and (isco_partner >= 2500 and isco_partner <= 2619) class16_p=2.
if (selfem_partner=2 or selfem_partner=3) and (isco_partner = 2621) class16_p=2.
if (selfem_partner=2 or selfem_partner=3) and (isco_partner >= 2630 and isco_partner <= 2634) class16_p=2.
if (selfem_partner=2 or selfem_partner=3) and (isco_partner >= 2636 and isco_partner <= 2640) class16_p=2.
if (selfem_partner=2 or selfem_partner=3) and (isco_partner >= 2642 and isco_partner <= 2643) class16_p=2.

/* Small business owners with employees (3)

if (selfem_partner=3) and (isco_partner >= 1000 and isco_partner <= 1439) class16_p=3.
if (selfem_partner=3) and (isco_partner = 2163) class16_p=3.
if (selfem_partner=3) and (isco_partner = 2166) class16_p=3.
if (selfem_partner=3) and (isco_partner >= 2220 and isco_partner <= 2240) class16_p=3.
if (selfem_partner=3) and (isco_partner = 2260) class16_p=3.
if (selfem_partner=3) and (isco_partner >= 2263 and isco_partner <= 2269) class16_p=3.
if (selfem_partner=3) and (isco_partner >= 2340 and isco_partner <= 2342) class16_p=3.
if (selfem_partner=3) and (isco_partner >= 2353 and isco_partner <= 2356) class16_p=3.
if (selfem_partner=3) and (isco_partner >= 2433 and isco_partner <= 2434) class16_p=3.
if (selfem_partner=3) and (isco_partner = 2620) class16_p=3.
if (selfem_partner=3) and (isco_partner = 2622) class16_p=3.
if (selfem_partner=3) and (isco_partner = 2635) class16_p=3.
if (selfem_partner=3) and (isco_partner = 2641) class16_p=3.
if (selfem_partner=3) and (isco_partner >= 2650 and isco_partner <= 2659) class16_p=3.
if (selfem_partner=3) and (isco_partner >= 3000 and isco_partner <= 9629) class16_p=3.

/* Small business owners without employees (4)

if (selfem_partner=2) and (isco_partner >= 1000 and isco_partner <= 1439) class16_p=4.
if (selfem_partner=2) and (isco_partner = 2163) class16_p=4.
if (selfem_partner=2) and (isco_partner = 2166) class16_p=4.
if (selfem_partner=2) and (isco_partner >= 2220 and isco_partner <= 2240) class16_p=4.
if (selfem_partner=2) and (isco_partner = 2260) class16_p=4.
if (selfem_partner=2) and (isco_partner >= 2263 and isco_partner <= 2269) class16_p=4.
if (selfem_partner=2) and (isco_partner >= 2340 and isco_partner <= 2342) class16_p=4.
if (selfem_partner=2) and (isco_partner >= 2353 and isco_partner <= 2356) class16_p=4.
if (selfem_partner=2) and (isco_partner >= 2433 and isco_partner <= 2434) class16_p=4.
if (selfem_partner=2) and (isco_partner = 2620) class16_p=4.
if (selfem_partner=2) and (isco_partner = 2622) class16_p=4.
if (selfem_partner=2) and (isco_partner = 2635) class16_p=4.
if (selfem_partner=2) and (isco_partner = 2641) class16_p=4.
if (selfem_partner=2) and (isco_partner >= 2650 and isco_partner <= 2659) class16_p=4.
if (selfem_partner=2) and (isco_partner >= 3000 and isco_partner <= 9629) class16_p=4.

/* Technical experts (5)

if (selfem_partner=1) and (isco_partner >= 2100 and  isco_partner <= 2162) class16_p=5 .
if (selfem_partner=1) and (isco_partner >= 2164 and  isco_partner <= 2165) class16_p=5 .
if (selfem_partner=1) and (isco_partner >= 2500 and  isco_partner <= 2529) class16_p=5 .

/* Technicians (6)

if (selfem_partner=1) and (isco_partner >= 3100 and  isco_partner <= 3155) class16_p=6 .
if (selfem_partner=1) and (isco_partner >= 3210 and  isco_partner <= 3214) class16_p=6 .
if (selfem_partner=1) and (isco_partner = 3252) class16_p=6 .
if (selfem_partner=1) and (isco_partner >= 3500 and  isco_partner <= 3522) class16_p=6 .

/* Skilled manual (7)

if (selfem_partner=1) and (isco_partner >= 6000 and  isco_partner <= 7549) class16_p=7 .
if (selfem_partner=1) and (isco_partner >= 8310 and  isco_partner <= 8312) class16_p=7 .
if (selfem_partner=1) and (isco_partner = 8330) class16_p=7 .
if (selfem_partner=1) and (isco_partner >= 8332 and  isco_partner <= 8340) class16_p=7 .
if (selfem_partner=1) and (isco_partner >= 8342 and  isco_partner <= 8344) class16_p=7 .

/* Low-skilled manual (8)

if (selfem_partner=1) and (isco_partner >= 8000 and  isco_partner <= 8300) class16_p=8 .
if (selfem_partner=1) and (isco_partner >= 8320 and  isco_partner <= 8321) class16_p=8 .
if (selfem_partner=1) and (isco_partner = 8341) class16_p=8 .
if (selfem_partner=1) and (isco_partner = 8350) class16_p=8 .
if (selfem_partner=1) and (isco_partner >= 9200 and  isco_partner <= 9334) class16_p=8 .
if (selfem_partner=1) and (isco_partner >= 9600 and  isco_partner <= 9620) class16_p=8 .
if (selfem_partner=1) and (isco_partner >= 9622 and  isco_partner <= 9629) class16_p=8 .

/* Higher-grade managers and administrators (9)

if (selfem_partner=1) and (isco_partner >= 1000 and  isco_partner <= 1300) class16_p=9 .
if (selfem_partner=1) and (isco_partner >= 1320 and  isco_partner <= 1349) class16_p=9 .
if (selfem_partner=1) and (isco_partner >= 2400 and  isco_partner <= 2432) class16_p=9 .
if (selfem_partner=1) and (isco_partner >= 2610 and  isco_partner <= 2619) class16_p=9 .
if (selfem_partner=1) and (isco_partner = 2631) class16_p=9 .
if (selfem_partner=1) and (isco_partner >= 100 and  isco_partner <= 110) class16_p=9 .

/* Lower-grade managers and administrators (10)

if (selfem_partner=1) and (isco_partner >= 1310 and  isco_partner <= 1312) class16_p=10 .
if (selfem_partner=1) and (isco_partner >= 1400 and  isco_partner <= 1439) class16_p=10 .
if (selfem_partner=1) and (isco_partner >= 2433 and  isco_partner <= 2434) class16_p=10 .
if (selfem_partner=1) and (isco_partner >= 3300 and  isco_partner <= 3339) class16_p=10 .
if (selfem_partner=1) and (isco_partner = 3343) class16_p=10 .
if (selfem_partner=1) and (isco_partner >= 3350 and  isco_partner <= 3359) class16_p=10 .
if (selfem_partner=1) and (isco_partner = 3411) class16_p=10 .
if (selfem_partner=1) and (isco_partner = 5221) class16_p=10 .
if (selfem_partner=1) and (isco_partner >= 200 and  isco_partner <= 210) class16_p=10 .

/* Skilled clerks (11)

if (selfem_partner=1) and (isco_partner >= 3340 and  isco_partner <= 3342) class16_p=11 .
if (selfem_partner=1) and (isco_partner = 3344) class16_p=11 .
if (selfem_partner=1) and (isco_partner >= 4000 and  isco_partner <= 4131) class16_p=11 .
if (selfem_partner=1) and (isco_partner >= 4200 and  isco_partner <= 4221) class16_p=11 .
if (selfem_partner=1) and (isco_partner >= 4224 and  isco_partner <= 4413) class16_p=11 .
if (selfem_partner=1) and (isco_partner >= 4415 and  isco_partner <= 4419) class16_p=11 .

/* Unskilled clerks (12)

if (selfem_partner=1) and (isco_partner = 4132) class16_p=12 .
if (selfem_partner=1) and (isco_partner = 4222) class16_p=12 .
if (selfem_partner=1) and (isco_partner = 4223) class16_p=12 .
if (selfem_partner=1) and (isco_partner = 5230) class16_p=12 .
if (selfem_partner=1) and (isco_partner = 9621) class16_p=12 .

/* Socio-cultural professionals (13)

if (selfem_partner=1) and (isco_partner >= 2200 and  isco_partner <= 2212) class16_p=13 .
if (selfem_partner=1) and (isco_partner = 2250) class16_p=13 .
if (selfem_partner=1) and (isco_partner >= 2261 and  isco_partner <= 2262) class16_p=13 .
if (selfem_partner=1) and (isco_partner >= 2300 and  isco_partner <= 2330) class16_p=13 .
if (selfem_partner=1) and (isco_partner >= 2350 and  isco_partner <= 2352) class16_p=13 .
if (selfem_partner=1) and (isco_partner = 2359) class16_p=13 .
if (selfem_partner=1) and (isco_partner = 2600) class16_p=13 .
if (selfem_partner=1) and (isco_partner = 2621) class16_p=13 .
if (selfem_partner=1) and (isco_partner = 2630) class16_p=13 .
if (selfem_partner=1) and (isco_partner >= 2632 and  isco_partner <= 2634) class16_p=13 .
if (selfem_partner=1) and (isco_partner >= 2636 and  isco_partner <= 2640) class16_p=13 .
if (selfem_partner=1) and (isco_partner >= 2642 and  isco_partner <= 2643) class16_p=13 .

/* Socio-cultural semi-professionals (14)

if (selfem_partner=1) and (isco_partner = 2163) class16_p=14 .
if (selfem_partner=1) and (isco_partner = 2166) class16_p=14 .
if (selfem_partner=1) and (isco_partner >= 2220 and  isco_partner <= 2240) class16_p=14 .
if (selfem_partner=1) and (isco_partner = 2260) class16_p=14 .
if (selfem_partner=1) and (isco_partner >= 2263 and  isco_partner <= 2269) class16_p=14 .
if (selfem_partner=1) and (isco_partner >= 2340 and  isco_partner <= 2342) class16_p=14 .
if (selfem_partner=1) and (isco_partner >= 2353 and  isco_partner <= 2356) class16_p=14 .
if (selfem_partner=1) and (isco_partner = 2620) class16_p=14 .
if (selfem_partner=1) and (isco_partner = 2622) class16_p=14 .
if (selfem_partner=1) and (isco_partner = 2635) class16_p=14 .
if (selfem_partner=1) and (isco_partner = 2641) class16_p=14 .
if (selfem_partner=1) and (isco_partner >= 2650 and  isco_partner <= 2659) class16_p=14 .
if (selfem_partner=1) and (isco_partner = 3200) class16_p=14 .
if (selfem_partner=1) and (isco_partner >= 3220 and  isco_partner <= 3230) class16_p=14 .
if (selfem_partner=1) and (isco_partner = 3250) class16_p=14 .
if (selfem_partner=1) and (isco_partner >= 3253 and  isco_partner <= 3257) class16_p=14 .
if (selfem_partner=1) and (isco_partner = 3259) class16_p=14 .
if (selfem_partner=1) and (isco_partner >= 3400 and  isco_partner <= 3410) class16_p=14 .
if (selfem_partner=1) and (isco_partner >= 3412 and  isco_partner <= 3413) class16_p=14 .
if (selfem_partner=1) and (isco_partner >= 3430 and  isco_partner <= 3433) class16_p=14 .
if (selfem_partner=1) and (isco_partner = 3435) class16_p=14 .
if (selfem_partner=1) and (isco_partner = 4414) class16_p=14 .

/* Skilled service (15)

if (selfem_partner=1) and (isco_partner = 3240) class16_p=15 .
if (selfem_partner=1) and (isco_partner = 3251) class16_p=15 .
if (selfem_partner=1) and (isco_partner = 3258) class16_p=15 .
if (selfem_partner=1) and (isco_partner >= 3420 and  isco_partner <= 3423) class16_p=15 .
if (selfem_partner=1) and (isco_partner = 3434) class16_p=15 .
if (selfem_partner=1) and (isco_partner >= 5000 and  isco_partner <= 5120) class16_p=15 .
if (selfem_partner=1) and (isco_partner >= 5140 and  isco_partner <= 5142) class16_p=15 .
if (selfem_partner=1) and (isco_partner = 5163) class16_p=15 .
if (selfem_partner=1) and (isco_partner = 5165) class16_p=15 .
if (selfem_partner=1) and (isco_partner = 5200) class16_p=15 .
if (selfem_partner=1) and (isco_partner = 5220) class16_p=15 .
if (selfem_partner=1) and (isco_partner >= 5222 and  isco_partner <= 5223) class16_p=15 .
if (selfem_partner=1) and (isco_partner >= 5241 and  isco_partner <= 5242) class16_p=15 .
if (selfem_partner=1) and (isco_partner >= 5300 and  isco_partner <= 5321) class16_p=15 .
if (selfem_partner=1) and (isco_partner >= 5400 and  isco_partner <= 5413) class16_p=15 .
if (selfem_partner=1) and (isco_partner = 5419) class16_p=15 .
if (selfem_partner=1) and (isco_partner = 8331) class16_p=15 .

/* Low-skilled service (16)

if (selfem_partner=1) and (isco_partner >= 5130 and  isco_partner <= 5132) class16_p=16 .
if (selfem_partner=1) and (isco_partner >= 5150 and  isco_partner <= 5162) class16_p=16 .
if (selfem_partner=1) and (isco_partner = 5164) class16_p=16 .
if (selfem_partner=1) and (isco_partner = 5169) class16_p=16 .
if (selfem_partner=1) and (isco_partner >= 5210 and  isco_partner <= 5212) class16_p=16 .
if (selfem_partner=1) and (isco_partner = 5240) class16_p=16 .
if (selfem_partner=1) and (isco_partner >= 5243 and  isco_partner <= 5249) class16_p=16 .
if (selfem_partner=1) and (isco_partner >= 5322 and  isco_partner <= 5329) class16_p=16 .
if (selfem_partner=1) and (isco_partner = 5414) class16_p=16 .
if (selfem_partner=1) and (isco_partner = 8322) class16_p=16 .
if (selfem_partner=1) and (isco_partner >= 9100 and  isco_partner <= 9129) class16_p=16 .
if (selfem_partner=1) and (isco_partner >= 9400 and  isco_partner <= 9520) class16_p=16 .

do if (class16_p=-9).
if (isco_partner=0310 or isco_partner=0) class16_p=10.
if (isco_partner=2000) and (sector_act_p=1 or sector_act_p=5 or sector_act_p=10 or sector_act_p=12 or sector_act_p=14) class16_p=5.
if (isco_partner=2000) and (sector_act_p=2 or sector_act_p=7 or sector_act_p=8 or sector_act_p=9  or sector_act_p=11) class16_p=9.
if (isco_partner=2000) and (sector_act_p=3 or sector_act_p=4 or sector_act_p=6 or sector_act_p=13) class16_p=13.
if (isco_partner=3000) and (sector_act_p=1 or sector_act_p=5 or sector_act_p=10 or sector_act_p=12 or sector_act_p=14) class16_p=6.
if (isco_partner=3000) and (sector_act_p=2 or sector_act_p=7 or sector_act_p=8 or sector_act_p=9  or sector_act_p=11) class16_p=10.
if (isco_partner=3000) and (sector_act_p=3 or sector_act_p=4 or sector_act_p=6 or sector_act_p=13) class16_p=14.
if (isco_partner=9000) and (sector_act_p=1 or sector_act_p=5 or sector_act_p=10 or sector_act_p=12 or sector_act_p=14) class16_p=8.
if (isco_partner=9000) and (sector_act_p=2 or sector_act_p=7 or sector_act_p=8 or sector_act_p=9  or sector_act_p=11) class16_p=12.
if (isco_partner=9000) and (sector_act_p=3 or sector_act_p=4 or sector_act_p=6 or sector_act_p=13) class16_p=16.
end if.


recode class16_p (-9=sysmis)(else=copy).
variable labels class16_p "Partner's Oesch class position - 16 classes" .
value labels class16_p 
1 "Large employers"
2 "Self-employed professionals"
3 "Small business owners with employees"
4 "Small business owners without employees"
5 "Technical experts"
6 "Technicians"
7 "Skilled manual"
8 "Low-skilled manual"
9 "Higher-grade managers and administrators"
10 "Lower-grade managers and administrators"
11 "Skilled clerks"
12 "Unskilled clerks"
13 "Socio-cultural professionals"
14 "Socio-cultural semi-professionals"
15 "Skilled service"
16 "Low-skilled service".
formats class16_p (f1).
fre class16_p.

recode class16_p (1,2=1)(3,4=2)(5,6=3)(7,8=4)(9,10=5)(11,12=6)(13,14=7)(15,16=8) into class8_p.
variable labels class8_p "Partner's Oesch class position - 8 classes".
value labels class8_p
1 "Self-employed professionals and large employers"
2 "Small business owners"
3 "Technical (semi-)professionals"
4 "Production workers"
5 "(Associate) managers"
6 "Clerks"
7 "Socio-cultural (semi-)professionals"
8 "Service workers".
formats class8_p (f1).
fre class8_p.

recode class16_p (1,2,5,9,13=1)(6,10,14=2)(3,4=3)(7,11,15=4)(8,12,16=5) into class5_p.
variable labels class5_p "Partner's Oesch class position - 5 classes".
value labels class5_p
1 "Higher-grade service class"
2 "Lower-grade service class"
3 "Small business owners"
4 "Skilled workers"
5 "Unskilled workers".
formats class5_p (f1).
fre class5_p.


***************************************************************************************
* Main earner's Oesch class position
* Recode and create variables used to construct class variable for main earners
* Variables used to construct class variable for main earners: ISCO08prof_me, f23100_f23500, f23150_f23550, f23350_f23750
***************************************************************************************

**** Recode occupation variable (isco88 com 4-digit) for main earners

fre ISCO08prof_me.

recode ISCO08prof_me (sysmis, -99, 9997=-9) (else=copy) into isco_mainearner.
variable labels isco_mainearner "Current occupation of main earner - isco08 4-digit" .
fre isco_mainearner.


**** Recode employment status for main earners

fre f23100_f23500 f23150_f23550.

recode f23100_f23500 (1, 2 ,3=1)(4=2)(else=9) into emplrel_h.
value labels emplrel_h
1"Employee"
2"Self-employed"
9"Missing".
fre emplrel_h.

recode f23150_f23550 (1 thru 9=1)(10 thru 10000=2)(else=0) into emplno_h.
value labels emplno_h
0"0 employees"
1"1-9 employees"
2"10+ employees".
fre emplno_h.


if (emplrel_h = 1 or emplrel_h = 9) selfem_mainearner = 1.
if (emplrel_h = 2 and emplno_h = 0) selfem_mainearner = 2.
if (emplrel_h = 2 and emplno_h = 1) selfem_mainearner = 3.
if (emplrel_h = 2 and emplno_h = 2) selfem_mainearner = 4.
variable labels selfem_mainearner "Employment status for main earners".
value labels selfem_mainearner
1 "Not self-employed"
2 "Self-empl without employees"
3 "Self-empl with 1-9 employees"
4 "Self-empl with 10 or more".
fre selfem_mainearner.


**** Recode activity sector for main earners

fre f23350_f23750.

compute sector_act_h=f23350_f23750.
fre sector_act_h.

********************************************
* Create Oesch class schema for main earners 
********************************************

compute class16_h = -9.

/* Large employers (1)

if (selfem_mainearner=4) class16_h=1.

/* Self-employed professionals (2)

if (selfem_mainearner=2 or selfem_mainearner=3) and (isco_mainearner >= 2000 and isco_mainearner <= 2162) class16_h=2.
if (selfem_mainearner=2 or selfem_mainearner=3) and (isco_mainearner >= 2164 and isco_mainearner <= 2165) class16_h=2.
if (selfem_mainearner=2 or selfem_mainearner=3) and (isco_mainearner >= 2200 and isco_mainearner <= 2212) class16_h=2.
if (selfem_mainearner=2 or selfem_mainearner=3) and (isco_mainearner = 2250) class16_h=2.
if (selfem_mainearner=2 or selfem_mainearner=3) and (isco_mainearner >= 2261 and isco_mainearner <= 2262) class16_h=2.
if (selfem_mainearner=2 or selfem_mainearner=3) and (isco_mainearner >= 2300 and isco_mainearner <= 2330) class16_h=2.
if (selfem_mainearner=2 or selfem_mainearner=3) and (isco_mainearner >= 2350 and isco_mainearner <= 2352) class16_h=2.
if (selfem_mainearner=2 or selfem_mainearner=3) and (isco_mainearner >= 2359 and isco_mainearner <= 2432) class16_h=2.
if (selfem_mainearner=2 or selfem_mainearner=3) and (isco_mainearner >= 2500 and isco_mainearner <= 2619) class16_h=2.
if (selfem_mainearner=2 or selfem_mainearner=3) and (isco_mainearner = 2621) class16_h=2.
if (selfem_mainearner=2 or selfem_mainearner=3) and (isco_mainearner >= 2630 and isco_mainearner <= 2634) class16_h=2.
if (selfem_mainearner=2 or selfem_mainearner=3) and (isco_mainearner >= 2636 and isco_mainearner <= 2640) class16_h=2.
if (selfem_mainearner=2 or selfem_mainearner=3) and (isco_mainearner >= 2642 and isco_mainearner <= 2643) class16_h=2.

/* Small business owners with employees (3)

if (selfem_mainearner=3) and (isco_mainearner >= 1000 and isco_mainearner <= 1439) class16_h=3.
if (selfem_mainearner=3) and (isco_mainearner = 2163) class16_h=3.
if (selfem_mainearner=3) and (isco_mainearner = 2166) class16_h=3.
if (selfem_mainearner=3) and (isco_mainearner >= 2220 and isco_mainearner <= 2240) class16_h=3.
if (selfem_mainearner=3) and (isco_mainearner = 2260) class16_h=3.
if (selfem_mainearner=3) and (isco_mainearner >= 2263 and isco_mainearner <= 2269) class16_h=3.
if (selfem_mainearner=3) and (isco_mainearner >= 2340 and isco_mainearner <= 2342) class16_h=3.
if (selfem_mainearner=3) and (isco_mainearner >= 2353 and isco_mainearner <= 2356) class16_h=3.
if (selfem_mainearner=3) and (isco_mainearner >= 2433 and isco_mainearner <= 2434) class16_h=3.
if (selfem_mainearner=3) and (isco_mainearner = 2620) class16_h=3.
if (selfem_mainearner=3) and (isco_mainearner = 2622) class16_h=3.
if (selfem_mainearner=3) and (isco_mainearner = 2635) class16_h=3.
if (selfem_mainearner=3) and (isco_mainearner = 2641) class16_h=3.
if (selfem_mainearner=3) and (isco_mainearner >= 2650 and isco_mainearner <= 2659) class16_h=3.
if (selfem_mainearner=3) and (isco_mainearner >= 3000 and isco_mainearner <= 9629) class16_h=3.

/* Small business owners without employees (4)

if (selfem_mainearner=2) and (isco_mainearner >= 1000 and isco_mainearner <= 1439) class16_h=4.
if (selfem_mainearner=2) and (isco_mainearner = 2163) class16_h=4.
if (selfem_mainearner=2) and (isco_mainearner = 2166) class16_h=4.
if (selfem_mainearner=2) and (isco_mainearner >= 2220 and isco_mainearner <= 2240) class16_h=4.
if (selfem_mainearner=2) and (isco_mainearner = 2260) class16_h=4.
if (selfem_mainearner=2) and (isco_mainearner >= 2263 and isco_mainearner <= 2269) class16_h=4.
if (selfem_mainearner=2) and (isco_mainearner >= 2340 and isco_mainearner <= 2342) class16_h=4.
if (selfem_mainearner=2) and (isco_mainearner >= 2353 and isco_mainearner <= 2356) class16_h=4.
if (selfem_mainearner=2) and (isco_mainearner >= 2433 and isco_mainearner <= 2434) class16_h=4.
if (selfem_mainearner=2) and (isco_mainearner = 2620) class16_h=4.
if (selfem_mainearner=2) and (isco_mainearner = 2622) class16_h=4.
if (selfem_mainearner=2) and (isco_mainearner = 2635) class16_h=4.
if (selfem_mainearner=2) and (isco_mainearner = 2641) class16_h=4.
if (selfem_mainearner=2) and (isco_mainearner >= 2650 and isco_mainearner <= 2659) class16_h=4.
if (selfem_mainearner=2) and (isco_mainearner >= 3000 and isco_mainearner <= 9629) class16_h=4.

/* Technical experts (5)

if (selfem_mainearner=1) and (isco_mainearner >= 2100 and  isco_mainearner <= 2162) class16_h=5 .
if (selfem_mainearner=1) and (isco_mainearner >= 2164 and  isco_mainearner <= 2165) class16_h=5 .
if (selfem_mainearner=1) and (isco_mainearner >= 2500 and  isco_mainearner <= 2529) class16_h=5 .

/* Technicians (6)

if (selfem_mainearner=1) and (isco_mainearner >= 3100 and  isco_mainearner <= 3155) class16_h=6 .
if (selfem_mainearner=1) and (isco_mainearner >= 3210 and  isco_mainearner <= 3214) class16_h=6 .
if (selfem_mainearner=1) and (isco_mainearner = 3252) class16_h=6 .
if (selfem_mainearner=1) and (isco_mainearner >= 3500 and  isco_mainearner <= 3522) class16_h=6 .

/* Skilled manual (7)

if (selfem_mainearner=1) and (isco_mainearner >= 6000 and  isco_mainearner <= 7549) class16_h=7 .
if (selfem_mainearner=1) and (isco_mainearner >= 8310 and  isco_mainearner <= 8312) class16_h=7 .
if (selfem_mainearner=1) and (isco_mainearner = 8330) class16_h=7 .
if (selfem_mainearner=1) and (isco_mainearner >= 8332 and  isco_mainearner <= 8340) class16_h=7 .
if (selfem_mainearner=1) and (isco_mainearner >= 8342 and  isco_mainearner <= 8344) class16_h=7 .

/* Low-skilled manual (8)

if (selfem_mainearner=1) and (isco_mainearner >= 8000 and  isco_mainearner <= 8300) class16_h=8 .
if (selfem_mainearner=1) and (isco_mainearner >= 8320 and  isco_mainearner <= 8321) class16_h=8 .
if (selfem_mainearner=1) and (isco_mainearner = 8341) class16_h=8 .
if (selfem_mainearner=1) and (isco_mainearner = 8350) class16_h=8 .
if (selfem_mainearner=1) and (isco_mainearner >= 9200 and  isco_mainearner <= 9334) class16_h=8 .
if (selfem_mainearner=1) and (isco_mainearner >= 9600 and  isco_mainearner <= 9620) class16_h=8 .
if (selfem_mainearner=1) and (isco_mainearner >= 9622 and  isco_mainearner <= 9629) class16_h=8 .

/* Higher-grade managers and administrators (9)

if (selfem_mainearner=1) and (isco_mainearner >= 1000 and  isco_mainearner <= 1300) class16_h=9 .
if (selfem_mainearner=1) and (isco_mainearner >= 1320 and  isco_mainearner <= 1349) class16_h=9 .
if (selfem_mainearner=1) and (isco_mainearner >= 2400 and  isco_mainearner <= 2432) class16_h=9 .
if (selfem_mainearner=1) and (isco_mainearner >= 2610 and  isco_mainearner <= 2619) class16_h=9 .
if (selfem_mainearner=1) and (isco_mainearner = 2631) class16_h=9 .
if (selfem_mainearner=1) and (isco_mainearner >= 100 and  isco_mainearner <= 110) class16_h=9 .

/* Lower-grade managers and administrators (10)

if (selfem_mainearner=1) and (isco_mainearner >= 1310 and  isco_mainearner <= 1312) class16_h=10 .
if (selfem_mainearner=1) and (isco_mainearner >= 1400 and  isco_mainearner <= 1439) class16_h=10 .
if (selfem_mainearner=1) and (isco_mainearner >= 2433 and  isco_mainearner <= 2434) class16_h=10 .
if (selfem_mainearner=1) and (isco_mainearner >= 3300 and  isco_mainearner <= 3339) class16_h=10 .
if (selfem_mainearner=1) and (isco_mainearner = 3343) class16_h=10 .
if (selfem_mainearner=1) and (isco_mainearner >= 3350 and  isco_mainearner <= 3359) class16_h=10 .
if (selfem_mainearner=1) and (isco_mainearner = 3411) class16_h=10 .
if (selfem_mainearner=1) and (isco_mainearner = 5221) class16_h=10 .
if (selfem_mainearner=1) and (isco_mainearner >= 200 and  isco_mainearner <= 210) class16_h=10 .

/* Skilled clerks (11)

if (selfem_mainearner=1) and (isco_mainearner >= 3340 and  isco_mainearner <= 3342) class16_h=11 .
if (selfem_mainearner=1) and (isco_mainearner = 3344) class16_h=11 .
if (selfem_mainearner=1) and (isco_mainearner >= 4000 and  isco_mainearner <= 4131) class16_h=11 .
if (selfem_mainearner=1) and (isco_mainearner >= 4200 and  isco_mainearner <= 4221) class16_h=11 .
if (selfem_mainearner=1) and (isco_mainearner >= 4224 and  isco_mainearner <= 4413) class16_h=11 .
if (selfem_mainearner=1) and (isco_mainearner >= 4415 and  isco_mainearner <= 4419) class16_h=11 .

/* Unskilled clerks (12)

if (selfem_mainearner=1) and (isco_mainearner = 4132) class16_h=12 .
if (selfem_mainearner=1) and (isco_mainearner = 4222) class16_h=12 .
if (selfem_mainearner=1) and (isco_mainearner = 4223) class16_h=12 .
if (selfem_mainearner=1) and (isco_mainearner = 5230) class16_h=12 .
if (selfem_mainearner=1) and (isco_mainearner = 9621) class16_h=12 .

/* Socio-cultural professionals (13)

if (selfem_mainearner=1) and (isco_mainearner >= 2200 and  isco_mainearner <= 2212) class16_h=13 .
if (selfem_mainearner=1) and (isco_mainearner = 2250) class16_h=13 .
if (selfem_mainearner=1) and (isco_mainearner >= 2261 and  isco_mainearner <= 2262) class16_h=13 .
if (selfem_mainearner=1) and (isco_mainearner >= 2300 and  isco_mainearner <= 2330) class16_h=13 .
if (selfem_mainearner=1) and (isco_mainearner >= 2350 and  isco_mainearner <= 2352) class16_h=13 .
if (selfem_mainearner=1) and (isco_mainearner = 2359) class16_h=13 .
if (selfem_mainearner=1) and (isco_mainearner = 2600) class16_h=13 .
if (selfem_mainearner=1) and (isco_mainearner = 2621) class16_h=13 .
if (selfem_mainearner=1) and (isco_mainearner = 2630) class16_h=13 .
if (selfem_mainearner=1) and (isco_mainearner >= 2632 and  isco_mainearner <= 2634) class16_h=13 .
if (selfem_mainearner=1) and (isco_mainearner >= 2636 and  isco_mainearner <= 2640) class16_h=13 .
if (selfem_mainearner=1) and (isco_mainearner >= 2642 and  isco_mainearner <= 2643) class16_h=13 .

/* Socio-cultural semi-professionals (14)

if (selfem_mainearner=1) and (isco_mainearner = 2163) class16_h=14 .
if (selfem_mainearner=1) and (isco_mainearner = 2166) class16_h=14 .
if (selfem_mainearner=1) and (isco_mainearner >= 2220 and  isco_mainearner <= 2240) class16_h=14 .
if (selfem_mainearner=1) and (isco_mainearner = 2260) class16_h=14 .
if (selfem_mainearner=1) and (isco_mainearner >= 2263 and  isco_mainearner <= 2269) class16_h=14 .
if (selfem_mainearner=1) and (isco_mainearner >= 2340 and  isco_mainearner <= 2342) class16_h=14 .
if (selfem_mainearner=1) and (isco_mainearner >= 2353 and  isco_mainearner <= 2356) class16_h=14 .
if (selfem_mainearner=1) and (isco_mainearner = 2620) class16_h=14 .
if (selfem_mainearner=1) and (isco_mainearner = 2622) class16_h=14 .
if (selfem_mainearner=1) and (isco_mainearner = 2635) class16_h=14 .
if (selfem_mainearner=1) and (isco_mainearner = 2641) class16_h=14 .
if (selfem_mainearner=1) and (isco_mainearner >= 2650 and  isco_mainearner <= 2659) class16_h=14 .
if (selfem_mainearner=1) and (isco_mainearner = 3200) class16_h=14 .
if (selfem_mainearner=1) and (isco_mainearner >= 3220 and  isco_mainearner <= 3230) class16_h=14 .
if (selfem_mainearner=1) and (isco_mainearner = 3250) class16_h=14 .
if (selfem_mainearner=1) and (isco_mainearner >= 3253 and  isco_mainearner <= 3257) class16_h=14 .
if (selfem_mainearner=1) and (isco_mainearner = 3259) class16_h=14 .
if (selfem_mainearner=1) and (isco_mainearner >= 3400 and  isco_mainearner <= 3410) class16_h=14 .
if (selfem_mainearner=1) and (isco_mainearner >= 3412 and  isco_mainearner <= 3413) class16_h=14 .
if (selfem_mainearner=1) and (isco_mainearner >= 3430 and  isco_mainearner <= 3433) class16_h=14 .
if (selfem_mainearner=1) and (isco_mainearner = 3435) class16_h=14 .
if (selfem_mainearner=1) and (isco_mainearner = 4414) class16_h=14 .

/* Skilled service (15)

if (selfem_mainearner=1) and (isco_mainearner = 3240) class16_h=15 .
if (selfem_mainearner=1) and (isco_mainearner = 3251) class16_h=15 .
if (selfem_mainearner=1) and (isco_mainearner = 3258) class16_h=15 .
if (selfem_mainearner=1) and (isco_mainearner >= 3420 and  isco_mainearner <= 3423) class16_h=15 .
if (selfem_mainearner=1) and (isco_mainearner = 3434) class16_h=15 .
if (selfem_mainearner=1) and (isco_mainearner >= 5000 and  isco_mainearner <= 5120) class16_h=15 .
if (selfem_mainearner=1) and (isco_mainearner >= 5140 and  isco_mainearner <= 5142) class16_h=15 .
if (selfem_mainearner=1) and (isco_mainearner = 5163) class16_h=15 .
if (selfem_mainearner=1) and (isco_mainearner = 5165) class16_h=15 .
if (selfem_mainearner=1) and (isco_mainearner = 5200) class16_h=15 .
if (selfem_mainearner=1) and (isco_mainearner = 5220) class16_h=15 .
if (selfem_mainearner=1) and (isco_mainearner >= 5222 and  isco_mainearner <= 5223) class16_h=15 .
if (selfem_mainearner=1) and (isco_mainearner >= 5241 and  isco_mainearner <= 5242) class16_h=15 .
if (selfem_mainearner=1) and (isco_mainearner >= 5300 and  isco_mainearner <= 5321) class16_h=15 .
if (selfem_mainearner=1) and (isco_mainearner >= 5400 and  isco_mainearner <= 5413) class16_h=15 .
if (selfem_mainearner=1) and (isco_mainearner = 5419) class16_h=15 .
if (selfem_mainearner=1) and (isco_mainearner = 8331) class16_h=15 .

/* Low-skilled service (16)

if (selfem_mainearner=1) and (isco_mainearner >= 5130 and  isco_mainearner <= 5132) class16_h=16 .
if (selfem_mainearner=1) and (isco_mainearner >= 5150 and  isco_mainearner <= 5162) class16_h=16 .
if (selfem_mainearner=1) and (isco_mainearner = 5164) class16_h=16 .
if (selfem_mainearner=1) and (isco_mainearner = 5169) class16_h=16 .
if (selfem_mainearner=1) and (isco_mainearner >= 5210 and  isco_mainearner <= 5212) class16_h=16 .
if (selfem_mainearner=1) and (isco_mainearner = 5240) class16_h=16 .
if (selfem_mainearner=1) and (isco_mainearner >= 5243 and  isco_mainearner <= 5249) class16_h=16 .
if (selfem_mainearner=1) and (isco_mainearner >= 5322 and  isco_mainearner <= 5329) class16_h=16 .
if (selfem_mainearner=1) and (isco_mainearner = 5414) class16_h=16 .
if (selfem_mainearner=1) and (isco_mainearner = 8322) class16_h=16 .
if (selfem_mainearner=1) and (isco_mainearner >= 9100 and  isco_mainearner <= 9129) class16_h=16 .
if (selfem_mainearner=1) and (isco_mainearner >= 9400 and  isco_mainearner <= 9520) class16_h=16 .

do if (class16_h=-9).
if (isco_mainearner=0310) class16_h=10.
if (isco_mainearner=2000) and (sector_act_h=1 or sector_act_h=5 or sector_act_h=10 or sector_act_h=12 or sector_act_h=14) class16_h=5.
if (isco_mainearner=2000) and (sector_act_h=2 or sector_act_h=7 or sector_act_h=8 or sector_act_h=9  or sector_act_h=11) class16_h=9.
if (isco_mainearner=2000) and (sector_act_h=3 or sector_act_h=4 or sector_act_h=6 or sector_act_h=13) class16_h=13.
if (isco_mainearner=3000) and (sector_act_h=1 or sector_act_h=5 or sector_act_h=10 or sector_act_h=12 or sector_act_h=14) class16_h=6.
if (isco_mainearner=3000) and (sector_act_h=2 or sector_act_h=7 or sector_act_h=8 or sector_act_h=9  or sector_act_h=11) class16_h=10.
if (isco_mainearner=3000) and (sector_act_h=3 or sector_act_h=4 or sector_act_h=6 or sector_act_h=13) class16_h=14.
if (isco_mainearner=9000) and (sector_act_h=1 or sector_act_h=5 or sector_act_h=10 or sector_act_h=12 or sector_act_h=14) class16_h=8.
if (isco_mainearner=9000) and (sector_act_h=2 or sector_act_h=7 or sector_act_h=8 or sector_act_h=9  or sector_act_h=11) class16_h=12.
if (isco_mainearner=9000) and (sector_act_h=3 or sector_act_h=4 or sector_act_h=6 or sector_act_h=13) class16_h=16.
end if.


recode class16_h (-9=sysmis)(else=copy).
variable labels class16_h "Main earner's Oesch class position - 16 classes" .
value labels class16_h 
1 "Large employers"
2 "Self-employed professionals"
3 "Small business owners with employees"
4 "Small business owners without employees"
5 "Technical experts"
6 "Technicians"
7 "Skilled manual"
8 "Low-skilled manual"
9 "Higher-grade managers and administrators"
10 "Lower-grade managers and administrators"
11 "Skilled clerks"
12 "Unskilled clerks"
13 "Socio-cultural professionals"
14 "Socio-cultural semi-professionals"
15 "Skilled service"
16 "Low-skilled service".
formats class16_h (f1).
fre class16_h.

recode class16_h (1,2=1)(3,4=2)(5,6=3)(7,8=4)(9,10=5)(11,12=6)(13,14=7)(15,16=8) into class8_h.
variable labels class8_h "Main earner's Oesch class position - 8 classes".
value labels class8_h
1 "Self-employed professionals and large employers"
2 "Small business owners"
3 "Technical (semi-)professionals"
4 "Production workers"
5 "(Associate) managers"
6 "Clerks"
7 "Socio-cultural (semi-)professionals"
8 "Service workers".
formats class8_h (f1).
fre class8_h.

recode class16_h (1,2,5,9,13=1)(6,10,14=2)(3,4=3)(7,11,15=4)(8,12,16=5) into class5_h.
variable labels class5_h "Main earner's Oesch class position - 5 classes".
value labels class5_h
1 "Higher-grade service class"
2 "Lower-grade service class"
3 "Small business owners"
4 "Skilled workers"
5 "Unskilled workers".
formats class5_h (f1).
fre class5_h.


****************************************************************************************************
* Final Oesch class position
* Merge three class variables (respondents, partners and main earners)
* Assign the partner's Oesch class position when the respondent's Oesch class position is missing; assign the main earner's Oesch class position when the respondent's Oesch class position and the partner's Oesch class position are missing:
****************************************************************************************************

compute class16=class16_r.

do if missing(class16_r).
compute class16=class16_p.
end if.
do if missing(class16_r) and missing(class16_p).
compute class16=class16_h.
end if.


variable labels class16 "Final Oesch class position - 16 classes" .
value labels class16
1 "Large employers"
2 "Self-employed professionals"
3 "Small business owners with employees"
4 "Small business owners without employees"
5 "Technical experts"
6 "Technicians"
7 "Skilled manual"
8 "Low-skilled manual"
9 "Higher-grade managers and administrators"
10 "Lower-grade managers and administrators"
11 "Skilled clerks"
12 "Unskilled clerks"
13 "Socio-cultural professionals"
14 "Socio-cultural semi-professionals"
15 "Skilled service"
16 "Low-skilled service".
formats class16 (f1).
fre class16.

recode class16 (1,2=1)(3,4=2)(5,6=3)(7,8=4)(9,10=5)(11,12=6)(13,14=7)(15,16=8) into class8.
variable labels class8 "Final Oesch class position - 8 classes".
value labels class8
1 "Self-employed professionals and large employers"
2 "Small business owners"
3 "Technical (semi-)professionals"
4 "Production workers"
5 "(Associate) managers"
6 "Clerks"
7 "Socio-cultural (semi-)professionals"
8 "Service workers".
formats class8 (f1).
fre class8.

recode class16 (1,2,5,9,13=1)(6,10,14=2)(3,4=3)(7,11,15=4)(8,12,16=5) into class5.
variable labels class5 "Final Oesch class position - 5 classes".
value labels class5
1 "Higher-grade service class"
2 "Lower-grade service class"
3 "Small business owners"
4 "Skilled workers"
5 "Unskilled workers".
formats class5 (f1).
fre class5.

delete variables isco_mainjob emplrel_r emplno_r selfem_mainjob sector_act_r isco_partner emplrel_p emplno_p selfem_partner sector_act_p isco_mainearner emplrel_h emplno_h selfem_mainearner sector_act_h.

**********************************
* End
**********************************

var lab class16_r "Respondent's class position (Oesch) - 16 classes".
var lab class8_r "Respondent's class position (Oesch) - 8 classes".
var lab class5_r "Respondent's class position (Oesch) - 5 classes".
var lab class16_p "Partner's class position (Oesch) - 16 classes".
var lab class8_p "Partner's class position (Oesch) - 8 classes".
var lab class5_p "Partner's class position (Oesch) - 5 classes".
var lab class16_h "Main earner's class position (Oesch) - 16 classes".
var lab class8_h "Main earner's class position (Oesch) - 8 classes".
var lab class5_h "Main earner's class position (Oesch) - 5 classes".
var lab class8 "Class position (Oesch) based on R's, P's or M-E's class, 8 classes".
var lab class16 "Class position (Oesch) based on R's, P's or M-E's class, 16 classes".
var lab class5 "Class position (Oesch) based on R's, P's or M-E's class, 5 classes".