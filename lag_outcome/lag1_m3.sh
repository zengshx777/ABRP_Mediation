#!/bin/bash
cd '/home/grad/sz127/Downloads/third year/FPCA_1007'
R CMD BATCH --vanilla '--args m=3 f=1 adv_id=1 age.truncate.female=18 l_m=11 K_m=7 l_y=11 K_y=7 lag_outcome=1' main_template.R main_31_lag1.out &
R CMD BATCH --vanilla '--args m=3 f=1 adv_id=2 age.truncate.female=18 l_m=11 K_m=7 l_y=11 K_y=7 lag_outcome=1' main_template.R main_32_lag1.out &
R CMD BATCH --vanilla '--args m=3 f=1 adv_id=3 age.truncate.female=18 l_m=11 K_m=7 l_y=11 K_y=7 lag_outcome=1' main_template.R main_33_lag1.out &
R CMD BATCH --vanilla '--args m=3 f=1 adv_id=4 age.truncate.female=18 l_m=11 K_m=7 l_y=11 K_y=7 lag_outcome=1' main_template.R main_34_lag1.out &
R CMD BATCH --vanilla '--args m=3 f=1 adv_id=5 age.truncate.female=18 l_m=11 K_m=7 l_y=11 K_y=7 lag_outcome=1' main_template.R main_35_lag1.out &
R CMD BATCH --vanilla '--args m=3 f=1 adv_id=6 age.truncate.female=18 l_m=11 K_m=7 l_y=11 K_y=7 lag_outcome=1' main_template.R main_36_lag1.out &
R CMD BATCH --vanilla '--args m=3 f=1 adv_id=8 age.truncate.female=18 l_m=11 K_m=7 l_y=11 K_y=7 lag_outcome=1' main_template.R main_38_lag1.out &
R CMD BATCH --vanilla '--args m=3 f=1 adv_id=9 age.truncate.female=18 l_m=11 K_m=7 l_y=11 K_y=7 lag_outcome=1' main_template.R main_39_lag1.out &