#!/bin/bash
cd '/home/grad/sz127/Downloads/third year/FPCA_1007'
R CMD BATCH --vanilla '--args m=1 f=1 adv_id=1 age.truncate.female=18 l_m=11 K_m=7 l_y=11 K_y=7 lag_outcome=1' main_template.R main_1_lag1.out &
R CMD BATCH --vanilla '--args m=1 f=1 adv_id=2 age.truncate.female=18 l_m=11 K_m=7 l_y=11 K_y=7 lag_outcome=1' main_template.R main_2_lag1.out &
R CMD BATCH --vanilla '--args m=1 f=1 adv_id=3 age.truncate.female=18 l_m=11 K_m=7 l_y=11 K_y=7 lag_outcome=1' main_template.R main_3_lag1.out &
R CMD BATCH --vanilla '--args m=1 f=1 adv_id=4 age.truncate.female=18 l_m=11 K_m=7 l_y=11 K_y=7 lag_outcome=1' main_template.R main_4_lag1.out &
R CMD BATCH --vanilla '--args m=1 f=1 adv_id=5 age.truncate.female=18 l_m=11 K_m=7 l_y=11 K_y=7 lag_outcome=1' main_template.R main_5_lag1.out &
R CMD BATCH --vanilla '--args m=1 f=1 adv_id=6 age.truncate.female=18 l_m=11 K_m=7 l_y=11 K_y=7 lag_outcome=1' main_template.R main_6_lag1.out &
R CMD BATCH --vanilla '--args m=1 f=1 adv_id=8 age.truncate.female=18 l_m=11 K_m=7 l_y=11 K_y=7 lag_outcome=1' main_template.R main_8_lag1.out &
R CMD BATCH --vanilla '--args m=1 f=1 adv_id=9 age.truncate.female=18 l_m=11 K_m=7 l_y=11 K_y=7 lag_outcome=1' main_template.R main_9_lag1.out &