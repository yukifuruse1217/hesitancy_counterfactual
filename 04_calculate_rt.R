library(ggplot2)
library(MASS)
# # install.packages('maxLik')
library(maxLik)
# #install.packages('LaplacesDemon')
library(LaplacesDemon)
library(dplyr)
library(magrittr)
library(lubridate)
library(stats4)

# load saved environment

##########################
### NEED 87.3_rev data ###
##########################

load("scenario000_87.3_10_rev.RData") # to include hypothetical case without vaccine effect on symptoms

load("after_1112_environment.RData")



all_data$case_00_diff_original <- all_data$case_00_diff
all_data$case_10_diff_original <- all_data$case_10_diff
all_data$case_20_diff_original <- all_data$case_20_diff
all_data$case_30_diff_original <- all_data$case_30_diff
all_data$case_40_diff_original <- all_data$case_40_diff
all_data$case_50_diff_original <- all_data$case_50_diff
all_data$case_60_diff_original <- all_data$case_60_diff
all_data$case_70_diff_original <- all_data$case_70_diff
all_data$case_80_diff_original <- all_data$case_80_diff

all_data$case_00_diff[1:99] <- df_plot$loop_sim_list_case00.mean[1:99]
all_data$case_10_diff[1:99] <- df_plot$loop_sim_list_case10.mean[1:99]
all_data$case_20_diff[1:99] <- df_plot$loop_sim_list_case20.mean[1:99]
all_data$case_30_diff[1:99] <- df_plot$loop_sim_list_case30.mean[1:99]
all_data$case_40_diff[1:99] <- df_plot$loop_sim_list_case40.mean[1:99]
all_data$case_50_diff[1:99] <- df_plot$loop_sim_list_case50.mean[1:99]
all_data$case_60_diff[1:99] <- df_plot$loop_sim_list_case60.mean[1:99]
all_data$case_70_diff[1:99] <- df_plot$loop_sim_list_case70.mean[1:99]
all_data$case_80_diff[1:99] <- df_plot$loop_sim_list_case80.mean[1:99]



# Define the log-likelihood function
rt_loglik <- function(params,
                      nn,
                      new_case00,
                      new_case10,
                      new_case20,
                      new_case30,
                      new_case40,
                      new_case50,
                      new_case60,
                      new_case70,
                      new_case80,
                      vaccine00,
                      vaccine10,
                      vaccine20,
                      vaccine30,
                      vaccine40,
                      vaccine50,
                      vaccine60,
                      vaccine70,
                      vaccine80,
                      pred00_sofar,
                      pred10_sofar,
                      pred20_sofar,
                      pred30_sofar,
                      pred40_sofar,
                      pred50_sofar,
                      pred60_sofar,
                      pred70_sofar,
                      pred80_sofar,
                      cum_vac00,
                      cum_vac10,
                      cum_vac20,
                      cum_vac30,
                      cum_vac40,
                      cum_vac50,
                      cum_vac60,
                      cum_vac70,
                      cum_vac80,
                      population00,
                      population10,
                      population20,
                      population30,
                      population40,
                      population50,
                      population60,
                      population70,
                      population80,
                      alpha_inf_young_model,
                      delta_inf_young_model,
                      alpha_hosp_young_model2, 
                      delta_hosp_young_model2,
                      alpha_inf_old_model,
                      delta_inf_old_model,
                      alpha_hosp_old_model2,
                      delta_hosp_old_model2,
                      trans_alpha,
                      trans_delta,
                      delta_prop) {                  
  
  
  
  rt <- exp(params[1])
  n <- nn
  
  
  ### week NN
  
  n00 <- nn
  n01 <- ifelse(nn-1<1, 1, nn-1)
  n02 <- ifelse(nn-2<1, 1, nn-2)
  n03 <- ifelse(nn-3<1, 1, nn-3)
  n04 <- ifelse(nn-4<1, 1, nn-4)
  n05 <- ifelse(nn-5<1, 1, nn-5)
  n06 <- ifelse(nn-6<1, 1, nn-6)
  n07 <- ifelse(nn-7<1, 1, nn-7)
  n08 <- ifelse(nn-8<1, 1, nn-8)
  n09 <- ifelse(nn-9<1, 1, nn-9)
  n10 <- ifelse(nn-10<1, 1, nn-10)
  n11 <- ifelse(nn-11<1, 1, nn-11)
  n12 <- ifelse(nn-12<1, 1, nn-12)
  n13 <- ifelse(nn-13<1, 1, nn-13)
  n14 <- ifelse(nn-14<1, 1, nn-14)
  n15 <- ifelse(nn-15<1, 1, nn-15)
  n16 <- ifelse(nn-16<1, 1, nn-16)
  n17 <- ifelse(nn-17<1, 1, nn-17)
  n18 <- ifelse(nn-18<1, 1, nn-18)
  n19 <- ifelse(nn-19<1, 1, nn-19)
  n20 <- ifelse(nn-20<1, 1, nn-20)
  n21 <- ifelse(nn-21<1, 1, nn-21)
  n22 <- ifelse(nn-22<1, 1, nn-22)
  n23 <- ifelse(nn-23<1, 1, nn-23)
  n24 <- ifelse(nn-24<1, 1, nn-24)
  n25 <- ifelse(nn-25<1, 1, nn-25)
  n26 <- ifelse(nn-26<1, 1, nn-26)
  n27 <- ifelse(nn-27<1, 1, nn-27)
  n28 <- ifelse(nn-28<1, 1, nn-28)
  n29 <- ifelse(nn-29<1, 1, nn-29)
  n30 <- ifelse(nn-30<1, 1, nn-30)
  n31 <- ifelse(nn-31<1, 1, nn-31)
  n32 <- ifelse(nn-32<1, 1, nn-32)
  n33 <- ifelse(nn-33<1, 1, nn-33)
  n34 <- ifelse(nn-34<1, 1, nn-34)
  n35 <- ifelse(nn-35<1, 1, nn-35)
  n36 <- ifelse(nn-36<1, 1, nn-36)
  n37 <- ifelse(nn-37<1, 1, nn-37)
  n38 <- ifelse(nn-38<1, 1, nn-38)
  n39 <- ifelse(nn-39<1, 1, nn-39)
  n40 <- ifelse(nn-40<1, 1, nn-40)
  n41 <- ifelse(nn-41<1, 1, nn-41)
  n42 <- ifelse(nn-42<1, 1, nn-42)
  n43 <- ifelse(nn-43<1, 1, nn-43)
  n44 <- ifelse(nn-44<1, 1, nn-44)
  n45 <- ifelse(nn-45<1, 1, nn-45)
  n46 <- ifelse(nn-46<1, 1, nn-46)
  n47 <- ifelse(nn-47<1, 1, nn-47)
  n48 <- ifelse(nn-48<1, 1, nn-48)
  n49 <- ifelse(nn-49<1, 1, nn-49)
  n50 <- ifelse(nn-50<1, 1, nn-50)
  n51 <- ifelse(nn-51<1, 1, nn-51)
  n52 <- ifelse(nn-52<1, 1, nn-52)
  n53 <- ifelse(nn-53<1, 1, nn-53)
  n54 <- ifelse(nn-54<1, 1, nn-54)
  n55 <- ifelse(nn-55<1, 1, nn-55)
  n56 <- ifelse(nn-56<1, 1, nn-56)
  n57 <- ifelse(nn-57<1, 1, nn-57)
  n58 <- ifelse(nn-58<1, 1, nn-58)
  n59 <- ifelse(nn-59<1, 1, nn-59)
  n60 <- ifelse(nn-60<1, 1, nn-60)
  n61 <- ifelse(nn-61<1, 1, nn-61)
  n62 <- ifelse(nn-62<1, 1, nn-62)
  n63 <- ifelse(nn-63<1, 1, nn-63)
  n64 <- ifelse(nn-64<1, 1, nn-64)
  n65 <- ifelse(nn-65<1, 1, nn-65)
  n66 <- ifelse(nn-66<1, 1, nn-66)
  n67 <- ifelse(nn-67<1, 1, nn-67)
  n68 <- ifelse(nn-68<1, 1, nn-68)
  n69 <- ifelse(nn-69<1, 1, nn-69)
  n70 <- ifelse(nn-70<1, 1, nn-70)
  n71 <- ifelse(nn-71<1, 1, nn-71)
  n72 <- ifelse(nn-72<1, 1, nn-72)
  n73 <- ifelse(nn-73<1, 1, nn-73)
  n74 <- ifelse(nn-74<1, 1, nn-74)
  n75 <- ifelse(nn-75<1, 1, nn-75)
  n76 <- ifelse(nn-76<1, 1, nn-76)
  n77 <- ifelse(nn-77<1, 1, nn-77)
  n78 <- ifelse(nn-78<1, 1, nn-78)
  n79 <- ifelse(nn-79<1, 1, nn-79)
  n80 <- ifelse(nn-80<1, 1, nn-80)
  n81 <- ifelse(nn-81<1, 1, nn-81)
  n82 <- ifelse(nn-82<1, 1, nn-82)
  n83 <- ifelse(nn-83<1, 1, nn-83)
  n84 <- ifelse(nn-84<1, 1, nn-84)
  n85 <- ifelse(nn-85<1, 1, nn-85)
  n86 <- ifelse(nn-86<1, 1, nn-86)
  n87 <- ifelse(nn-87<1, 1, nn-87)
  n88 <- ifelse(nn-88<1, 1, nn-88)
  n89 <- ifelse(nn-89<1, 1, nn-89)
  n90 <- ifelse(nn-90<1, 1, nn-90)
  n91 <- ifelse(nn-91<1, 1, nn-91)
  n92 <- ifelse(nn-92<1, 1, nn-92)
  n93 <- ifelse(nn-93<1, 1, nn-93)
  n94 <- ifelse(nn-94<1, 1, nn-94)
  n95 <- ifelse(nn-95<1, 1, nn-95)
  n96 <- ifelse(nn-96<1, 1, nn-96)
  n97 <- ifelse(nn-97<1, 1, nn-97)
  n98 <- ifelse(nn-98<1, 1, nn-98)
  
  
  
  
  
  
  poi00 <- pred00_sofar[n] *
    (vaccine00[1] *  (1 - trans_alpha[n00] * (1 - delta_prop[n]) - trans_delta[n00] * delta_prop[n]) +
       vaccine00[2] *  (1 - trans_alpha[n01] * (1 - delta_prop[n]) - trans_delta[n01] * delta_prop[n]) +
       vaccine00[3] *  (1 - trans_alpha[n02] * (1 - delta_prop[n]) - trans_delta[n02] * delta_prop[n]) +
       vaccine00[4] *  (1 - trans_alpha[n03] * (1 - delta_prop[n]) - trans_delta[n03] * delta_prop[n]) +
       vaccine00[5] *  (1 - trans_alpha[n04] * (1 - delta_prop[n]) - trans_delta[n04] * delta_prop[n]) +
       vaccine00[6] *  (1 - trans_alpha[n05] * (1 - delta_prop[n]) - trans_delta[n05] * delta_prop[n]) +
       vaccine00[7] *  (1 - trans_alpha[n06] * (1 - delta_prop[n]) - trans_delta[n06] * delta_prop[n]) +
       vaccine00[8] *  (1 - trans_alpha[n07] * (1 - delta_prop[n]) - trans_delta[n07] * delta_prop[n]) +
       vaccine00[9] *  (1 - trans_alpha[n08] * (1 - delta_prop[n]) - trans_delta[n08] * delta_prop[n]) +
       vaccine00[10] * (1 - trans_alpha[n09] * (1 - delta_prop[n]) - trans_delta[n09] * delta_prop[n]) +
       vaccine00[11] * (1 - trans_alpha[n10] * (1 - delta_prop[n]) - trans_delta[n10] * delta_prop[n]) +
       vaccine00[12] * (1 - trans_alpha[n11] * (1 - delta_prop[n]) - trans_delta[n11] * delta_prop[n]) +
       vaccine00[13] * (1 - trans_alpha[n12] * (1 - delta_prop[n]) - trans_delta[n12] * delta_prop[n]) +
       vaccine00[14] * (1 - trans_alpha[n13] * (1 - delta_prop[n]) - trans_delta[n13] * delta_prop[n]) +
       vaccine00[15] * (1 - trans_alpha[n14] * (1 - delta_prop[n]) - trans_delta[n14] * delta_prop[n]) +
       vaccine00[16] * (1 - trans_alpha[n15] * (1 - delta_prop[n]) - trans_delta[n15] * delta_prop[n]) +
       vaccine00[17] * (1 - trans_alpha[n16] * (1 - delta_prop[n]) - trans_delta[n16] * delta_prop[n]) +
       vaccine00[18] * (1 - trans_alpha[n17] * (1 - delta_prop[n]) - trans_delta[n17] * delta_prop[n]) +
       vaccine00[19] * (1 - trans_alpha[n18] * (1 - delta_prop[n]) - trans_delta[n18] * delta_prop[n]) +
       vaccine00[20] * (1 - trans_alpha[n19] * (1 - delta_prop[n]) - trans_delta[n19] * delta_prop[n]) +
       vaccine00[21] * (1 - trans_alpha[n20] * (1 - delta_prop[n]) - trans_delta[n20] * delta_prop[n]) +
       vaccine00[22] * (1 - trans_alpha[n21] * (1 - delta_prop[n]) - trans_delta[n21] * delta_prop[n]) +
       vaccine00[23] * (1 - trans_alpha[n22] * (1 - delta_prop[n]) - trans_delta[n22] * delta_prop[n]) +
       vaccine00[24] * (1 - trans_alpha[n23] * (1 - delta_prop[n]) - trans_delta[n23] * delta_prop[n]) +
       vaccine00[25] * (1 - trans_alpha[n24] * (1 - delta_prop[n]) - trans_delta[n24] * delta_prop[n]) +
       vaccine00[26] * (1 - trans_alpha[n25] * (1 - delta_prop[n]) - trans_delta[n25] * delta_prop[n]) +
       vaccine00[27] * (1 - trans_alpha[n26] * (1 - delta_prop[n]) - trans_delta[n26] * delta_prop[n]) +
       vaccine00[28] * (1 - trans_alpha[n27] * (1 - delta_prop[n]) - trans_delta[n27] * delta_prop[n]) +
       vaccine00[29] * (1 - trans_alpha[n28] * (1 - delta_prop[n]) - trans_delta[n28] * delta_prop[n]) +
       vaccine00[30] * (1 - trans_alpha[n29] * (1 - delta_prop[n]) - trans_delta[n29] * delta_prop[n]) +
       vaccine00[31] * (1 - trans_alpha[n30] * (1 - delta_prop[n]) - trans_delta[n30] * delta_prop[n]) +
       vaccine00[32] * (1 - trans_alpha[n31] * (1 - delta_prop[n]) - trans_delta[n31] * delta_prop[n]) +
       vaccine00[33] * (1 - trans_alpha[n32] * (1 - delta_prop[n]) - trans_delta[n32] * delta_prop[n]) +
       vaccine00[34] * (1 - trans_alpha[n33] * (1 - delta_prop[n]) - trans_delta[n33] * delta_prop[n]) +
       vaccine00[35] * (1 - trans_alpha[n34] * (1 - delta_prop[n]) - trans_delta[n34] * delta_prop[n]) +
       vaccine00[36] * (1 - trans_alpha[n35] * (1 - delta_prop[n]) - trans_delta[n35] * delta_prop[n]) +
       vaccine00[37] * (1 - trans_alpha[n36] * (1 - delta_prop[n]) - trans_delta[n36] * delta_prop[n]) +
       vaccine00[38] * (1 - trans_alpha[n37] * (1 - delta_prop[n]) - trans_delta[n37] * delta_prop[n]) +
       vaccine00[39] * (1 - trans_alpha[n38] * (1 - delta_prop[n]) - trans_delta[n38] * delta_prop[n]) +
       vaccine00[40] * (1 - trans_alpha[n39] * (1 - delta_prop[n]) - trans_delta[n39] * delta_prop[n]) +
       vaccine00[41] * (1 - trans_alpha[n40] * (1 - delta_prop[n]) - trans_delta[n40] * delta_prop[n]) +
       vaccine00[42] * (1 - trans_alpha[n41] * (1 - delta_prop[n]) - trans_delta[n41] * delta_prop[n]) +
       vaccine00[43] * (1 - trans_alpha[n42] * (1 - delta_prop[n]) - trans_delta[n42] * delta_prop[n]) +
       vaccine00[44] * (1 - trans_alpha[n43] * (1 - delta_prop[n]) - trans_delta[n43] * delta_prop[n]) +
       vaccine00[45] * (1 - trans_alpha[n44] * (1 - delta_prop[n]) - trans_delta[n44] * delta_prop[n]) +
       vaccine00[46] * (1 - trans_alpha[n45] * (1 - delta_prop[n]) - trans_delta[n45] * delta_prop[n]) +
       vaccine00[47] * (1 - trans_alpha[n46] * (1 - delta_prop[n]) - trans_delta[n46] * delta_prop[n]) +
       vaccine00[48] * (1 - trans_alpha[n47] * (1 - delta_prop[n]) - trans_delta[n47] * delta_prop[n]) +
       vaccine00[49] * (1 - trans_alpha[n48] * (1 - delta_prop[n]) - trans_delta[n48] * delta_prop[n]) +
       vaccine00[50] * (1 - trans_alpha[n49] * (1 - delta_prop[n]) - trans_delta[n49] * delta_prop[n]) +
       vaccine00[51] * (1 - trans_alpha[n50] * (1 - delta_prop[n]) - trans_delta[n50] * delta_prop[n]) +
       vaccine00[52] * (1 - trans_alpha[n51] * (1 - delta_prop[n]) - trans_delta[n51] * delta_prop[n]) +
       vaccine00[53] * (1 - trans_alpha[n52] * (1 - delta_prop[n]) - trans_delta[n52] * delta_prop[n]) +
       vaccine00[54] * (1 - trans_alpha[n53] * (1 - delta_prop[n]) - trans_delta[n53] * delta_prop[n]) +
       vaccine00[55] * (1 - trans_alpha[n54] * (1 - delta_prop[n]) - trans_delta[n54] * delta_prop[n]) +
       vaccine00[56] * (1 - trans_alpha[n55] * (1 - delta_prop[n]) - trans_delta[n55] * delta_prop[n]) +
       vaccine00[57] * (1 - trans_alpha[n56] * (1 - delta_prop[n]) - trans_delta[n56] * delta_prop[n]) +
       vaccine00[58] * (1 - trans_alpha[n57] * (1 - delta_prop[n]) - trans_delta[n57] * delta_prop[n]) +
       vaccine00[59] * (1 - trans_alpha[n58] * (1 - delta_prop[n]) - trans_delta[n58] * delta_prop[n]) +
       vaccine00[60] * (1 - trans_alpha[n59] * (1 - delta_prop[n]) - trans_delta[n59] * delta_prop[n]) +
       vaccine00[61] * (1 - trans_alpha[n60] * (1 - delta_prop[n]) - trans_delta[n60] * delta_prop[n]) +
       vaccine00[62] * (1 - trans_alpha[n61] * (1 - delta_prop[n]) - trans_delta[n61] * delta_prop[n]) +
       vaccine00[63] * (1 - trans_alpha[n62] * (1 - delta_prop[n]) - trans_delta[n62] * delta_prop[n]) +
       vaccine00[64] * (1 - trans_alpha[n63] * (1 - delta_prop[n]) - trans_delta[n63] * delta_prop[n]) +
       vaccine00[65] * (1 - trans_alpha[n64] * (1 - delta_prop[n]) - trans_delta[n64] * delta_prop[n]) +
       vaccine00[66] * (1 - trans_alpha[n65] * (1 - delta_prop[n]) - trans_delta[n65] * delta_prop[n]) +
       vaccine00[67] * (1 - trans_alpha[n66] * (1 - delta_prop[n]) - trans_delta[n66] * delta_prop[n]) +
       vaccine00[68] * (1 - trans_alpha[n67] * (1 - delta_prop[n]) - trans_delta[n67] * delta_prop[n]) +
       vaccine00[69] * (1 - trans_alpha[n68] * (1 - delta_prop[n]) - trans_delta[n68] * delta_prop[n]) +
       vaccine00[70] * (1 - trans_alpha[n69] * (1 - delta_prop[n]) - trans_delta[n69] * delta_prop[n]) +
       vaccine00[71] * (1 - trans_alpha[n70] * (1 - delta_prop[n]) - trans_delta[n70] * delta_prop[n]) +
       vaccine00[72] * (1 - trans_alpha[n71] * (1 - delta_prop[n]) - trans_delta[n71] * delta_prop[n]) +
       vaccine00[73] * (1 - trans_alpha[n72] * (1 - delta_prop[n]) - trans_delta[n72] * delta_prop[n]) +
       vaccine00[74] * (1 - trans_alpha[n73] * (1 - delta_prop[n]) - trans_delta[n73] * delta_prop[n]) +
       vaccine00[75] * (1 - trans_alpha[n74] * (1 - delta_prop[n]) - trans_delta[n74] * delta_prop[n]) +
       vaccine00[76] * (1 - trans_alpha[n75] * (1 - delta_prop[n]) - trans_delta[n75] * delta_prop[n]) +
       vaccine00[77] * (1 - trans_alpha[n76] * (1 - delta_prop[n]) - trans_delta[n76] * delta_prop[n]) +
       vaccine00[78] * (1 - trans_alpha[n77] * (1 - delta_prop[n]) - trans_delta[n77] * delta_prop[n]) +
       vaccine00[79] * (1 - trans_alpha[n78] * (1 - delta_prop[n]) - trans_delta[n78] * delta_prop[n]) +
       vaccine00[80] * (1 - trans_alpha[n79] * (1 - delta_prop[n]) - trans_delta[n79] * delta_prop[n]) +
       vaccine00[81] * (1 - trans_alpha[n80] * (1 - delta_prop[n]) - trans_delta[n80] * delta_prop[n]) +
       vaccine00[82] * (1 - trans_alpha[n81] * (1 - delta_prop[n]) - trans_delta[n81] * delta_prop[n]) +
       vaccine00[83] * (1 - trans_alpha[n82] * (1 - delta_prop[n]) - trans_delta[n82] * delta_prop[n]) +
       vaccine00[84] * (1 - trans_alpha[n83] * (1 - delta_prop[n]) - trans_delta[n83] * delta_prop[n]) +
       vaccine00[85] * (1 - trans_alpha[n84] * (1 - delta_prop[n]) - trans_delta[n84] * delta_prop[n]) +
       vaccine00[86] * (1 - trans_alpha[n85] * (1 - delta_prop[n]) - trans_delta[n85] * delta_prop[n]) +
       vaccine00[87] * (1 - trans_alpha[n86] * (1 - delta_prop[n]) - trans_delta[n86] * delta_prop[n]) +
       vaccine00[88] * (1 - trans_alpha[n87] * (1 - delta_prop[n]) - trans_delta[n87] * delta_prop[n]) +
       vaccine00[89] * (1 - trans_alpha[n88] * (1 - delta_prop[n]) - trans_delta[n88] * delta_prop[n]) +
       vaccine00[90] * (1 - trans_alpha[n89] * (1 - delta_prop[n]) - trans_delta[n89] * delta_prop[n]) +
       vaccine00[91] * (1 - trans_alpha[n90] * (1 - delta_prop[n]) - trans_delta[n90] * delta_prop[n]) +
       vaccine00[92] * (1 - trans_alpha[n91] * (1 - delta_prop[n]) - trans_delta[n91] * delta_prop[n]) +
       vaccine00[93] * (1 - trans_alpha[n92] * (1 - delta_prop[n]) - trans_delta[n92] * delta_prop[n]) +
       vaccine00[94] * (1 - trans_alpha[n93] * (1 - delta_prop[n]) - trans_delta[n93] * delta_prop[n]) +
       vaccine00[95] * (1 - trans_alpha[n94] * (1 - delta_prop[n]) - trans_delta[n94] * delta_prop[n]) +
       vaccine00[96] * (1 - trans_alpha[n95] * (1 - delta_prop[n]) - trans_delta[n95] * delta_prop[n]) +
       vaccine00[97] * (1 - trans_alpha[n96] * (1 - delta_prop[n]) - trans_delta[n96] * delta_prop[n]) +
       vaccine00[98] * (1 - trans_alpha[n97] * (1 - delta_prop[n]) - trans_delta[n97] * delta_prop[n]) +
       vaccine00[99] * (1 - trans_alpha[n98] * (1 - delta_prop[n]) - trans_delta[n98] * delta_prop[n]) +
       ifelse((1 - cum_vac00[n] - ((sum(new_case00[1:9]) + sum(pred00_sofar[10:n])) / population00)) < 0,
              0,
              1 - cum_vac00[n] - ((sum(new_case00[1:9]) + sum(pred00_sofar[10:n])) / population00)))
  
  poi10 <- pred10_sofar[n] *
    (vaccine10[1] *  (1 - trans_alpha[n00] * (1 - delta_prop[n]) - trans_delta[n00] * delta_prop[n]) +
       vaccine10[2] *  (1 - trans_alpha[n01] * (1 - delta_prop[n]) - trans_delta[n01] * delta_prop[n]) +
       vaccine10[3] *  (1 - trans_alpha[n02] * (1 - delta_prop[n]) - trans_delta[n02] * delta_prop[n]) +
       vaccine10[4] *  (1 - trans_alpha[n03] * (1 - delta_prop[n]) - trans_delta[n03] * delta_prop[n]) +
       vaccine10[5] *  (1 - trans_alpha[n04] * (1 - delta_prop[n]) - trans_delta[n04] * delta_prop[n]) +
       vaccine10[6] *  (1 - trans_alpha[n05] * (1 - delta_prop[n]) - trans_delta[n05] * delta_prop[n]) +
       vaccine10[7] *  (1 - trans_alpha[n06] * (1 - delta_prop[n]) - trans_delta[n06] * delta_prop[n]) +
       vaccine10[8] *  (1 - trans_alpha[n07] * (1 - delta_prop[n]) - trans_delta[n07] * delta_prop[n]) +
       vaccine10[9] *  (1 - trans_alpha[n08] * (1 - delta_prop[n]) - trans_delta[n08] * delta_prop[n]) +
       vaccine10[10] * (1 - trans_alpha[n09] * (1 - delta_prop[n]) - trans_delta[n09] * delta_prop[n]) +
       vaccine10[11] * (1 - trans_alpha[n10] * (1 - delta_prop[n]) - trans_delta[n10] * delta_prop[n]) +
       vaccine10[12] * (1 - trans_alpha[n11] * (1 - delta_prop[n]) - trans_delta[n11] * delta_prop[n]) +
       vaccine10[13] * (1 - trans_alpha[n12] * (1 - delta_prop[n]) - trans_delta[n12] * delta_prop[n]) +
       vaccine10[14] * (1 - trans_alpha[n13] * (1 - delta_prop[n]) - trans_delta[n13] * delta_prop[n]) +
       vaccine10[15] * (1 - trans_alpha[n14] * (1 - delta_prop[n]) - trans_delta[n14] * delta_prop[n]) +
       vaccine10[16] * (1 - trans_alpha[n15] * (1 - delta_prop[n]) - trans_delta[n15] * delta_prop[n]) +
       vaccine10[17] * (1 - trans_alpha[n16] * (1 - delta_prop[n]) - trans_delta[n16] * delta_prop[n]) +
       vaccine10[18] * (1 - trans_alpha[n17] * (1 - delta_prop[n]) - trans_delta[n17] * delta_prop[n]) +
       vaccine10[19] * (1 - trans_alpha[n18] * (1 - delta_prop[n]) - trans_delta[n18] * delta_prop[n]) +
       vaccine10[20] * (1 - trans_alpha[n19] * (1 - delta_prop[n]) - trans_delta[n19] * delta_prop[n]) +
       vaccine10[21] * (1 - trans_alpha[n20] * (1 - delta_prop[n]) - trans_delta[n20] * delta_prop[n]) +
       vaccine10[22] * (1 - trans_alpha[n21] * (1 - delta_prop[n]) - trans_delta[n21] * delta_prop[n]) +
       vaccine10[23] * (1 - trans_alpha[n22] * (1 - delta_prop[n]) - trans_delta[n22] * delta_prop[n]) +
       vaccine10[24] * (1 - trans_alpha[n23] * (1 - delta_prop[n]) - trans_delta[n23] * delta_prop[n]) +
       vaccine10[25] * (1 - trans_alpha[n24] * (1 - delta_prop[n]) - trans_delta[n24] * delta_prop[n]) +
       vaccine10[26] * (1 - trans_alpha[n25] * (1 - delta_prop[n]) - trans_delta[n25] * delta_prop[n]) +
       vaccine10[27] * (1 - trans_alpha[n26] * (1 - delta_prop[n]) - trans_delta[n26] * delta_prop[n]) +
       vaccine10[28] * (1 - trans_alpha[n27] * (1 - delta_prop[n]) - trans_delta[n27] * delta_prop[n]) +
       vaccine10[29] * (1 - trans_alpha[n28] * (1 - delta_prop[n]) - trans_delta[n28] * delta_prop[n]) +
       vaccine10[30] * (1 - trans_alpha[n29] * (1 - delta_prop[n]) - trans_delta[n29] * delta_prop[n]) +
       vaccine10[31] * (1 - trans_alpha[n30] * (1 - delta_prop[n]) - trans_delta[n30] * delta_prop[n]) +
       vaccine10[32] * (1 - trans_alpha[n31] * (1 - delta_prop[n]) - trans_delta[n31] * delta_prop[n]) +
       vaccine10[33] * (1 - trans_alpha[n32] * (1 - delta_prop[n]) - trans_delta[n32] * delta_prop[n]) +
       vaccine10[34] * (1 - trans_alpha[n33] * (1 - delta_prop[n]) - trans_delta[n33] * delta_prop[n]) +
       vaccine10[35] * (1 - trans_alpha[n34] * (1 - delta_prop[n]) - trans_delta[n34] * delta_prop[n]) +
       vaccine10[36] * (1 - trans_alpha[n35] * (1 - delta_prop[n]) - trans_delta[n35] * delta_prop[n]) +
       vaccine10[37] * (1 - trans_alpha[n36] * (1 - delta_prop[n]) - trans_delta[n36] * delta_prop[n]) +
       vaccine10[38] * (1 - trans_alpha[n37] * (1 - delta_prop[n]) - trans_delta[n37] * delta_prop[n]) +
       vaccine10[39] * (1 - trans_alpha[n38] * (1 - delta_prop[n]) - trans_delta[n38] * delta_prop[n]) +
       vaccine10[40] * (1 - trans_alpha[n39] * (1 - delta_prop[n]) - trans_delta[n39] * delta_prop[n]) +
       vaccine10[41] * (1 - trans_alpha[n40] * (1 - delta_prop[n]) - trans_delta[n40] * delta_prop[n]) +
       vaccine10[42] * (1 - trans_alpha[n41] * (1 - delta_prop[n]) - trans_delta[n41] * delta_prop[n]) +
       vaccine10[43] * (1 - trans_alpha[n42] * (1 - delta_prop[n]) - trans_delta[n42] * delta_prop[n]) +
       vaccine10[44] * (1 - trans_alpha[n43] * (1 - delta_prop[n]) - trans_delta[n43] * delta_prop[n]) +
       vaccine10[45] * (1 - trans_alpha[n44] * (1 - delta_prop[n]) - trans_delta[n44] * delta_prop[n]) +
       vaccine10[46] * (1 - trans_alpha[n45] * (1 - delta_prop[n]) - trans_delta[n45] * delta_prop[n]) +
       vaccine10[47] * (1 - trans_alpha[n46] * (1 - delta_prop[n]) - trans_delta[n46] * delta_prop[n]) +
       vaccine10[48] * (1 - trans_alpha[n47] * (1 - delta_prop[n]) - trans_delta[n47] * delta_prop[n]) +
       vaccine10[49] * (1 - trans_alpha[n48] * (1 - delta_prop[n]) - trans_delta[n48] * delta_prop[n]) +
       vaccine10[50] * (1 - trans_alpha[n49] * (1 - delta_prop[n]) - trans_delta[n49] * delta_prop[n]) +
       vaccine10[51] * (1 - trans_alpha[n50] * (1 - delta_prop[n]) - trans_delta[n50] * delta_prop[n]) +
       vaccine10[52] * (1 - trans_alpha[n51] * (1 - delta_prop[n]) - trans_delta[n51] * delta_prop[n]) +
       vaccine10[53] * (1 - trans_alpha[n52] * (1 - delta_prop[n]) - trans_delta[n52] * delta_prop[n]) +
       vaccine10[54] * (1 - trans_alpha[n53] * (1 - delta_prop[n]) - trans_delta[n53] * delta_prop[n]) +
       vaccine10[55] * (1 - trans_alpha[n54] * (1 - delta_prop[n]) - trans_delta[n54] * delta_prop[n]) +
       vaccine10[56] * (1 - trans_alpha[n55] * (1 - delta_prop[n]) - trans_delta[n55] * delta_prop[n]) +
       vaccine10[57] * (1 - trans_alpha[n56] * (1 - delta_prop[n]) - trans_delta[n56] * delta_prop[n]) +
       vaccine10[58] * (1 - trans_alpha[n57] * (1 - delta_prop[n]) - trans_delta[n57] * delta_prop[n]) +
       vaccine10[59] * (1 - trans_alpha[n58] * (1 - delta_prop[n]) - trans_delta[n58] * delta_prop[n]) +
       vaccine10[60] * (1 - trans_alpha[n59] * (1 - delta_prop[n]) - trans_delta[n59] * delta_prop[n]) +
       vaccine10[61] * (1 - trans_alpha[n60] * (1 - delta_prop[n]) - trans_delta[n60] * delta_prop[n]) +
       vaccine10[62] * (1 - trans_alpha[n61] * (1 - delta_prop[n]) - trans_delta[n61] * delta_prop[n]) +
       vaccine10[63] * (1 - trans_alpha[n62] * (1 - delta_prop[n]) - trans_delta[n62] * delta_prop[n]) +
       vaccine10[64] * (1 - trans_alpha[n63] * (1 - delta_prop[n]) - trans_delta[n63] * delta_prop[n]) +
       vaccine10[65] * (1 - trans_alpha[n64] * (1 - delta_prop[n]) - trans_delta[n64] * delta_prop[n]) +
       vaccine10[66] * (1 - trans_alpha[n65] * (1 - delta_prop[n]) - trans_delta[n65] * delta_prop[n]) +
       vaccine10[67] * (1 - trans_alpha[n66] * (1 - delta_prop[n]) - trans_delta[n66] * delta_prop[n]) +
       vaccine10[68] * (1 - trans_alpha[n67] * (1 - delta_prop[n]) - trans_delta[n67] * delta_prop[n]) +
       vaccine10[69] * (1 - trans_alpha[n68] * (1 - delta_prop[n]) - trans_delta[n68] * delta_prop[n]) +
       vaccine10[70] * (1 - trans_alpha[n69] * (1 - delta_prop[n]) - trans_delta[n69] * delta_prop[n]) +
       vaccine10[71] * (1 - trans_alpha[n70] * (1 - delta_prop[n]) - trans_delta[n70] * delta_prop[n]) +
       vaccine10[72] * (1 - trans_alpha[n71] * (1 - delta_prop[n]) - trans_delta[n71] * delta_prop[n]) +
       vaccine10[73] * (1 - trans_alpha[n72] * (1 - delta_prop[n]) - trans_delta[n72] * delta_prop[n]) +
       vaccine10[74] * (1 - trans_alpha[n73] * (1 - delta_prop[n]) - trans_delta[n73] * delta_prop[n]) +
       vaccine10[75] * (1 - trans_alpha[n74] * (1 - delta_prop[n]) - trans_delta[n74] * delta_prop[n]) +
       vaccine10[76] * (1 - trans_alpha[n75] * (1 - delta_prop[n]) - trans_delta[n75] * delta_prop[n]) +
       vaccine10[77] * (1 - trans_alpha[n76] * (1 - delta_prop[n]) - trans_delta[n76] * delta_prop[n]) +
       vaccine10[78] * (1 - trans_alpha[n77] * (1 - delta_prop[n]) - trans_delta[n77] * delta_prop[n]) +
       vaccine10[79] * (1 - trans_alpha[n78] * (1 - delta_prop[n]) - trans_delta[n78] * delta_prop[n]) +
       vaccine10[80] * (1 - trans_alpha[n79] * (1 - delta_prop[n]) - trans_delta[n79] * delta_prop[n]) +
       vaccine10[81] * (1 - trans_alpha[n80] * (1 - delta_prop[n]) - trans_delta[n80] * delta_prop[n]) +
       vaccine10[82] * (1 - trans_alpha[n81] * (1 - delta_prop[n]) - trans_delta[n81] * delta_prop[n]) +
       vaccine10[83] * (1 - trans_alpha[n82] * (1 - delta_prop[n]) - trans_delta[n82] * delta_prop[n]) +
       vaccine10[84] * (1 - trans_alpha[n83] * (1 - delta_prop[n]) - trans_delta[n83] * delta_prop[n]) +
       vaccine10[85] * (1 - trans_alpha[n84] * (1 - delta_prop[n]) - trans_delta[n84] * delta_prop[n]) +
       vaccine10[86] * (1 - trans_alpha[n85] * (1 - delta_prop[n]) - trans_delta[n85] * delta_prop[n]) +
       vaccine10[87] * (1 - trans_alpha[n86] * (1 - delta_prop[n]) - trans_delta[n86] * delta_prop[n]) +
       vaccine10[88] * (1 - trans_alpha[n87] * (1 - delta_prop[n]) - trans_delta[n87] * delta_prop[n]) +
       vaccine10[89] * (1 - trans_alpha[n88] * (1 - delta_prop[n]) - trans_delta[n88] * delta_prop[n]) +
       vaccine10[90] * (1 - trans_alpha[n89] * (1 - delta_prop[n]) - trans_delta[n89] * delta_prop[n]) +
       vaccine10[91] * (1 - trans_alpha[n90] * (1 - delta_prop[n]) - trans_delta[n90] * delta_prop[n]) +
       vaccine10[92] * (1 - trans_alpha[n91] * (1 - delta_prop[n]) - trans_delta[n91] * delta_prop[n]) +
       vaccine10[93] * (1 - trans_alpha[n92] * (1 - delta_prop[n]) - trans_delta[n92] * delta_prop[n]) +
       vaccine10[94] * (1 - trans_alpha[n93] * (1 - delta_prop[n]) - trans_delta[n93] * delta_prop[n]) +
       vaccine10[95] * (1 - trans_alpha[n94] * (1 - delta_prop[n]) - trans_delta[n94] * delta_prop[n]) +
       vaccine10[96] * (1 - trans_alpha[n95] * (1 - delta_prop[n]) - trans_delta[n95] * delta_prop[n]) +
       vaccine10[97] * (1 - trans_alpha[n96] * (1 - delta_prop[n]) - trans_delta[n96] * delta_prop[n]) +
       vaccine10[98] * (1 - trans_alpha[n97] * (1 - delta_prop[n]) - trans_delta[n97] * delta_prop[n]) +
       vaccine10[99] * (1 - trans_alpha[n98] * (1 - delta_prop[n]) - trans_delta[n98] * delta_prop[n]) +
       ifelse((1 - cum_vac10[n] - ((sum(new_case10[1:9]) + sum(pred10_sofar[10:n])) / population10)) < 0,
              0,
              1 - cum_vac10[n] - ((sum(new_case10[1:9]) + sum(pred10_sofar[10:n])) / population10)))
  
  poi20 <- pred20_sofar[n] *
    (vaccine20[1] *  (1 - trans_alpha[n00] * (1 - delta_prop[n]) - trans_delta[n00] * delta_prop[n]) +
       vaccine20[2] *  (1 - trans_alpha[n01] * (1 - delta_prop[n]) - trans_delta[n01] * delta_prop[n]) +
       vaccine20[3] *  (1 - trans_alpha[n02] * (1 - delta_prop[n]) - trans_delta[n02] * delta_prop[n]) +
       vaccine20[4] *  (1 - trans_alpha[n03] * (1 - delta_prop[n]) - trans_delta[n03] * delta_prop[n]) +
       vaccine20[5] *  (1 - trans_alpha[n04] * (1 - delta_prop[n]) - trans_delta[n04] * delta_prop[n]) +
       vaccine20[6] *  (1 - trans_alpha[n05] * (1 - delta_prop[n]) - trans_delta[n05] * delta_prop[n]) +
       vaccine20[7] *  (1 - trans_alpha[n06] * (1 - delta_prop[n]) - trans_delta[n06] * delta_prop[n]) +
       vaccine20[8] *  (1 - trans_alpha[n07] * (1 - delta_prop[n]) - trans_delta[n07] * delta_prop[n]) +
       vaccine20[9] *  (1 - trans_alpha[n08] * (1 - delta_prop[n]) - trans_delta[n08] * delta_prop[n]) +
       vaccine20[10] * (1 - trans_alpha[n09] * (1 - delta_prop[n]) - trans_delta[n09] * delta_prop[n]) +
       vaccine20[11] * (1 - trans_alpha[n10] * (1 - delta_prop[n]) - trans_delta[n10] * delta_prop[n]) +
       vaccine20[12] * (1 - trans_alpha[n11] * (1 - delta_prop[n]) - trans_delta[n11] * delta_prop[n]) +
       vaccine20[13] * (1 - trans_alpha[n12] * (1 - delta_prop[n]) - trans_delta[n12] * delta_prop[n]) +
       vaccine20[14] * (1 - trans_alpha[n13] * (1 - delta_prop[n]) - trans_delta[n13] * delta_prop[n]) +
       vaccine20[15] * (1 - trans_alpha[n14] * (1 - delta_prop[n]) - trans_delta[n14] * delta_prop[n]) +
       vaccine20[16] * (1 - trans_alpha[n15] * (1 - delta_prop[n]) - trans_delta[n15] * delta_prop[n]) +
       vaccine20[17] * (1 - trans_alpha[n16] * (1 - delta_prop[n]) - trans_delta[n16] * delta_prop[n]) +
       vaccine20[18] * (1 - trans_alpha[n17] * (1 - delta_prop[n]) - trans_delta[n17] * delta_prop[n]) +
       vaccine20[19] * (1 - trans_alpha[n18] * (1 - delta_prop[n]) - trans_delta[n18] * delta_prop[n]) +
       vaccine20[20] * (1 - trans_alpha[n19] * (1 - delta_prop[n]) - trans_delta[n19] * delta_prop[n]) +
       vaccine20[21] * (1 - trans_alpha[n20] * (1 - delta_prop[n]) - trans_delta[n20] * delta_prop[n]) +
       vaccine20[22] * (1 - trans_alpha[n21] * (1 - delta_prop[n]) - trans_delta[n21] * delta_prop[n]) +
       vaccine20[23] * (1 - trans_alpha[n22] * (1 - delta_prop[n]) - trans_delta[n22] * delta_prop[n]) +
       vaccine20[24] * (1 - trans_alpha[n23] * (1 - delta_prop[n]) - trans_delta[n23] * delta_prop[n]) +
       vaccine20[25] * (1 - trans_alpha[n24] * (1 - delta_prop[n]) - trans_delta[n24] * delta_prop[n]) +
       vaccine20[26] * (1 - trans_alpha[n25] * (1 - delta_prop[n]) - trans_delta[n25] * delta_prop[n]) +
       vaccine20[27] * (1 - trans_alpha[n26] * (1 - delta_prop[n]) - trans_delta[n26] * delta_prop[n]) +
       vaccine20[28] * (1 - trans_alpha[n27] * (1 - delta_prop[n]) - trans_delta[n27] * delta_prop[n]) +
       vaccine20[29] * (1 - trans_alpha[n28] * (1 - delta_prop[n]) - trans_delta[n28] * delta_prop[n]) +
       vaccine20[30] * (1 - trans_alpha[n29] * (1 - delta_prop[n]) - trans_delta[n29] * delta_prop[n]) +
       vaccine20[31] * (1 - trans_alpha[n30] * (1 - delta_prop[n]) - trans_delta[n30] * delta_prop[n]) +
       vaccine20[32] * (1 - trans_alpha[n31] * (1 - delta_prop[n]) - trans_delta[n31] * delta_prop[n]) +
       vaccine20[33] * (1 - trans_alpha[n32] * (1 - delta_prop[n]) - trans_delta[n32] * delta_prop[n]) +
       vaccine20[34] * (1 - trans_alpha[n33] * (1 - delta_prop[n]) - trans_delta[n33] * delta_prop[n]) +
       vaccine20[35] * (1 - trans_alpha[n34] * (1 - delta_prop[n]) - trans_delta[n34] * delta_prop[n]) +
       vaccine20[36] * (1 - trans_alpha[n35] * (1 - delta_prop[n]) - trans_delta[n35] * delta_prop[n]) +
       vaccine20[37] * (1 - trans_alpha[n36] * (1 - delta_prop[n]) - trans_delta[n36] * delta_prop[n]) +
       vaccine20[38] * (1 - trans_alpha[n37] * (1 - delta_prop[n]) - trans_delta[n37] * delta_prop[n]) +
       vaccine20[39] * (1 - trans_alpha[n38] * (1 - delta_prop[n]) - trans_delta[n38] * delta_prop[n]) +
       vaccine20[40] * (1 - trans_alpha[n39] * (1 - delta_prop[n]) - trans_delta[n39] * delta_prop[n]) +
       vaccine20[41] * (1 - trans_alpha[n40] * (1 - delta_prop[n]) - trans_delta[n40] * delta_prop[n]) +
       vaccine20[42] * (1 - trans_alpha[n41] * (1 - delta_prop[n]) - trans_delta[n41] * delta_prop[n]) +
       vaccine20[43] * (1 - trans_alpha[n42] * (1 - delta_prop[n]) - trans_delta[n42] * delta_prop[n]) +
       vaccine20[44] * (1 - trans_alpha[n43] * (1 - delta_prop[n]) - trans_delta[n43] * delta_prop[n]) +
       vaccine20[45] * (1 - trans_alpha[n44] * (1 - delta_prop[n]) - trans_delta[n44] * delta_prop[n]) +
       vaccine20[46] * (1 - trans_alpha[n45] * (1 - delta_prop[n]) - trans_delta[n45] * delta_prop[n]) +
       vaccine20[47] * (1 - trans_alpha[n46] * (1 - delta_prop[n]) - trans_delta[n46] * delta_prop[n]) +
       vaccine20[48] * (1 - trans_alpha[n47] * (1 - delta_prop[n]) - trans_delta[n47] * delta_prop[n]) +
       vaccine20[49] * (1 - trans_alpha[n48] * (1 - delta_prop[n]) - trans_delta[n48] * delta_prop[n]) +
       vaccine20[50] * (1 - trans_alpha[n49] * (1 - delta_prop[n]) - trans_delta[n49] * delta_prop[n]) +
       vaccine20[51] * (1 - trans_alpha[n50] * (1 - delta_prop[n]) - trans_delta[n50] * delta_prop[n]) +
       vaccine20[52] * (1 - trans_alpha[n51] * (1 - delta_prop[n]) - trans_delta[n51] * delta_prop[n]) +
       vaccine20[53] * (1 - trans_alpha[n52] * (1 - delta_prop[n]) - trans_delta[n52] * delta_prop[n]) +
       vaccine20[54] * (1 - trans_alpha[n53] * (1 - delta_prop[n]) - trans_delta[n53] * delta_prop[n]) +
       vaccine20[55] * (1 - trans_alpha[n54] * (1 - delta_prop[n]) - trans_delta[n54] * delta_prop[n]) +
       vaccine20[56] * (1 - trans_alpha[n55] * (1 - delta_prop[n]) - trans_delta[n55] * delta_prop[n]) +
       vaccine20[57] * (1 - trans_alpha[n56] * (1 - delta_prop[n]) - trans_delta[n56] * delta_prop[n]) +
       vaccine20[58] * (1 - trans_alpha[n57] * (1 - delta_prop[n]) - trans_delta[n57] * delta_prop[n]) +
       vaccine20[59] * (1 - trans_alpha[n58] * (1 - delta_prop[n]) - trans_delta[n58] * delta_prop[n]) +
       vaccine20[60] * (1 - trans_alpha[n59] * (1 - delta_prop[n]) - trans_delta[n59] * delta_prop[n]) +
       vaccine20[61] * (1 - trans_alpha[n60] * (1 - delta_prop[n]) - trans_delta[n60] * delta_prop[n]) +
       vaccine20[62] * (1 - trans_alpha[n61] * (1 - delta_prop[n]) - trans_delta[n61] * delta_prop[n]) +
       vaccine20[63] * (1 - trans_alpha[n62] * (1 - delta_prop[n]) - trans_delta[n62] * delta_prop[n]) +
       vaccine20[64] * (1 - trans_alpha[n63] * (1 - delta_prop[n]) - trans_delta[n63] * delta_prop[n]) +
       vaccine20[65] * (1 - trans_alpha[n64] * (1 - delta_prop[n]) - trans_delta[n64] * delta_prop[n]) +
       vaccine20[66] * (1 - trans_alpha[n65] * (1 - delta_prop[n]) - trans_delta[n65] * delta_prop[n]) +
       vaccine20[67] * (1 - trans_alpha[n66] * (1 - delta_prop[n]) - trans_delta[n66] * delta_prop[n]) +
       vaccine20[68] * (1 - trans_alpha[n67] * (1 - delta_prop[n]) - trans_delta[n67] * delta_prop[n]) +
       vaccine20[69] * (1 - trans_alpha[n68] * (1 - delta_prop[n]) - trans_delta[n68] * delta_prop[n]) +
       vaccine20[70] * (1 - trans_alpha[n69] * (1 - delta_prop[n]) - trans_delta[n69] * delta_prop[n]) +
       vaccine20[71] * (1 - trans_alpha[n70] * (1 - delta_prop[n]) - trans_delta[n70] * delta_prop[n]) +
       vaccine20[72] * (1 - trans_alpha[n71] * (1 - delta_prop[n]) - trans_delta[n71] * delta_prop[n]) +
       vaccine20[73] * (1 - trans_alpha[n72] * (1 - delta_prop[n]) - trans_delta[n72] * delta_prop[n]) +
       vaccine20[74] * (1 - trans_alpha[n73] * (1 - delta_prop[n]) - trans_delta[n73] * delta_prop[n]) +
       vaccine20[75] * (1 - trans_alpha[n74] * (1 - delta_prop[n]) - trans_delta[n74] * delta_prop[n]) +
       vaccine20[76] * (1 - trans_alpha[n75] * (1 - delta_prop[n]) - trans_delta[n75] * delta_prop[n]) +
       vaccine20[77] * (1 - trans_alpha[n76] * (1 - delta_prop[n]) - trans_delta[n76] * delta_prop[n]) +
       vaccine20[78] * (1 - trans_alpha[n77] * (1 - delta_prop[n]) - trans_delta[n77] * delta_prop[n]) +
       vaccine20[79] * (1 - trans_alpha[n78] * (1 - delta_prop[n]) - trans_delta[n78] * delta_prop[n]) +
       vaccine20[80] * (1 - trans_alpha[n79] * (1 - delta_prop[n]) - trans_delta[n79] * delta_prop[n]) +
       vaccine20[81] * (1 - trans_alpha[n80] * (1 - delta_prop[n]) - trans_delta[n80] * delta_prop[n]) +
       vaccine20[82] * (1 - trans_alpha[n81] * (1 - delta_prop[n]) - trans_delta[n81] * delta_prop[n]) +
       vaccine20[83] * (1 - trans_alpha[n82] * (1 - delta_prop[n]) - trans_delta[n82] * delta_prop[n]) +
       vaccine20[84] * (1 - trans_alpha[n83] * (1 - delta_prop[n]) - trans_delta[n83] * delta_prop[n]) +
       vaccine20[85] * (1 - trans_alpha[n84] * (1 - delta_prop[n]) - trans_delta[n84] * delta_prop[n]) +
       vaccine20[86] * (1 - trans_alpha[n85] * (1 - delta_prop[n]) - trans_delta[n85] * delta_prop[n]) +
       vaccine20[87] * (1 - trans_alpha[n86] * (1 - delta_prop[n]) - trans_delta[n86] * delta_prop[n]) +
       vaccine20[88] * (1 - trans_alpha[n87] * (1 - delta_prop[n]) - trans_delta[n87] * delta_prop[n]) +
       vaccine20[89] * (1 - trans_alpha[n88] * (1 - delta_prop[n]) - trans_delta[n88] * delta_prop[n]) +
       vaccine20[90] * (1 - trans_alpha[n89] * (1 - delta_prop[n]) - trans_delta[n89] * delta_prop[n]) +
       vaccine20[91] * (1 - trans_alpha[n90] * (1 - delta_prop[n]) - trans_delta[n90] * delta_prop[n]) +
       vaccine20[92] * (1 - trans_alpha[n91] * (1 - delta_prop[n]) - trans_delta[n91] * delta_prop[n]) +
       vaccine20[93] * (1 - trans_alpha[n92] * (1 - delta_prop[n]) - trans_delta[n92] * delta_prop[n]) +
       vaccine20[94] * (1 - trans_alpha[n93] * (1 - delta_prop[n]) - trans_delta[n93] * delta_prop[n]) +
       vaccine20[95] * (1 - trans_alpha[n94] * (1 - delta_prop[n]) - trans_delta[n94] * delta_prop[n]) +
       vaccine20[96] * (1 - trans_alpha[n95] * (1 - delta_prop[n]) - trans_delta[n95] * delta_prop[n]) +
       vaccine20[97] * (1 - trans_alpha[n96] * (1 - delta_prop[n]) - trans_delta[n96] * delta_prop[n]) +
       vaccine20[98] * (1 - trans_alpha[n97] * (1 - delta_prop[n]) - trans_delta[n97] * delta_prop[n]) +
       vaccine20[99] * (1 - trans_alpha[n98] * (1 - delta_prop[n]) - trans_delta[n98] * delta_prop[n]) +
       ifelse((1 - cum_vac20[n] - ((sum(new_case20[1:9]) + sum(pred20_sofar[10:n])) / population20)) < 0,
              0,
              1 - cum_vac20[n] - ((sum(new_case20[1:9]) + sum(pred20_sofar[10:n])) / population20)))
  
  poi30 <- pred30_sofar[n] *
    (vaccine30[1] *  (1 - trans_alpha[n00] * (1 - delta_prop[n]) - trans_delta[n00] * delta_prop[n]) +
       vaccine30[2] *  (1 - trans_alpha[n01] * (1 - delta_prop[n]) - trans_delta[n01] * delta_prop[n]) +
       vaccine30[3] *  (1 - trans_alpha[n02] * (1 - delta_prop[n]) - trans_delta[n02] * delta_prop[n]) +
       vaccine30[4] *  (1 - trans_alpha[n03] * (1 - delta_prop[n]) - trans_delta[n03] * delta_prop[n]) +
       vaccine30[5] *  (1 - trans_alpha[n04] * (1 - delta_prop[n]) - trans_delta[n04] * delta_prop[n]) +
       vaccine30[6] *  (1 - trans_alpha[n05] * (1 - delta_prop[n]) - trans_delta[n05] * delta_prop[n]) +
       vaccine30[7] *  (1 - trans_alpha[n06] * (1 - delta_prop[n]) - trans_delta[n06] * delta_prop[n]) +
       vaccine30[8] *  (1 - trans_alpha[n07] * (1 - delta_prop[n]) - trans_delta[n07] * delta_prop[n]) +
       vaccine30[9] *  (1 - trans_alpha[n08] * (1 - delta_prop[n]) - trans_delta[n08] * delta_prop[n]) +
       vaccine30[10] * (1 - trans_alpha[n09] * (1 - delta_prop[n]) - trans_delta[n09] * delta_prop[n]) +
       vaccine30[11] * (1 - trans_alpha[n10] * (1 - delta_prop[n]) - trans_delta[n10] * delta_prop[n]) +
       vaccine30[12] * (1 - trans_alpha[n11] * (1 - delta_prop[n]) - trans_delta[n11] * delta_prop[n]) +
       vaccine30[13] * (1 - trans_alpha[n12] * (1 - delta_prop[n]) - trans_delta[n12] * delta_prop[n]) +
       vaccine30[14] * (1 - trans_alpha[n13] * (1 - delta_prop[n]) - trans_delta[n13] * delta_prop[n]) +
       vaccine30[15] * (1 - trans_alpha[n14] * (1 - delta_prop[n]) - trans_delta[n14] * delta_prop[n]) +
       vaccine30[16] * (1 - trans_alpha[n15] * (1 - delta_prop[n]) - trans_delta[n15] * delta_prop[n]) +
       vaccine30[17] * (1 - trans_alpha[n16] * (1 - delta_prop[n]) - trans_delta[n16] * delta_prop[n]) +
       vaccine30[18] * (1 - trans_alpha[n17] * (1 - delta_prop[n]) - trans_delta[n17] * delta_prop[n]) +
       vaccine30[19] * (1 - trans_alpha[n18] * (1 - delta_prop[n]) - trans_delta[n18] * delta_prop[n]) +
       vaccine30[20] * (1 - trans_alpha[n19] * (1 - delta_prop[n]) - trans_delta[n19] * delta_prop[n]) +
       vaccine30[21] * (1 - trans_alpha[n20] * (1 - delta_prop[n]) - trans_delta[n20] * delta_prop[n]) +
       vaccine30[22] * (1 - trans_alpha[n21] * (1 - delta_prop[n]) - trans_delta[n21] * delta_prop[n]) +
       vaccine30[23] * (1 - trans_alpha[n22] * (1 - delta_prop[n]) - trans_delta[n22] * delta_prop[n]) +
       vaccine30[24] * (1 - trans_alpha[n23] * (1 - delta_prop[n]) - trans_delta[n23] * delta_prop[n]) +
       vaccine30[25] * (1 - trans_alpha[n24] * (1 - delta_prop[n]) - trans_delta[n24] * delta_prop[n]) +
       vaccine30[26] * (1 - trans_alpha[n25] * (1 - delta_prop[n]) - trans_delta[n25] * delta_prop[n]) +
       vaccine30[27] * (1 - trans_alpha[n26] * (1 - delta_prop[n]) - trans_delta[n26] * delta_prop[n]) +
       vaccine30[28] * (1 - trans_alpha[n27] * (1 - delta_prop[n]) - trans_delta[n27] * delta_prop[n]) +
       vaccine30[29] * (1 - trans_alpha[n28] * (1 - delta_prop[n]) - trans_delta[n28] * delta_prop[n]) +
       vaccine30[30] * (1 - trans_alpha[n29] * (1 - delta_prop[n]) - trans_delta[n29] * delta_prop[n]) +
       vaccine30[31] * (1 - trans_alpha[n30] * (1 - delta_prop[n]) - trans_delta[n30] * delta_prop[n]) +
       vaccine30[32] * (1 - trans_alpha[n31] * (1 - delta_prop[n]) - trans_delta[n31] * delta_prop[n]) +
       vaccine30[33] * (1 - trans_alpha[n32] * (1 - delta_prop[n]) - trans_delta[n32] * delta_prop[n]) +
       vaccine30[34] * (1 - trans_alpha[n33] * (1 - delta_prop[n]) - trans_delta[n33] * delta_prop[n]) +
       vaccine30[35] * (1 - trans_alpha[n34] * (1 - delta_prop[n]) - trans_delta[n34] * delta_prop[n]) +
       vaccine30[36] * (1 - trans_alpha[n35] * (1 - delta_prop[n]) - trans_delta[n35] * delta_prop[n]) +
       vaccine30[37] * (1 - trans_alpha[n36] * (1 - delta_prop[n]) - trans_delta[n36] * delta_prop[n]) +
       vaccine30[38] * (1 - trans_alpha[n37] * (1 - delta_prop[n]) - trans_delta[n37] * delta_prop[n]) +
       vaccine30[39] * (1 - trans_alpha[n38] * (1 - delta_prop[n]) - trans_delta[n38] * delta_prop[n]) +
       vaccine30[40] * (1 - trans_alpha[n39] * (1 - delta_prop[n]) - trans_delta[n39] * delta_prop[n]) +
       vaccine30[41] * (1 - trans_alpha[n40] * (1 - delta_prop[n]) - trans_delta[n40] * delta_prop[n]) +
       vaccine30[42] * (1 - trans_alpha[n41] * (1 - delta_prop[n]) - trans_delta[n41] * delta_prop[n]) +
       vaccine30[43] * (1 - trans_alpha[n42] * (1 - delta_prop[n]) - trans_delta[n42] * delta_prop[n]) +
       vaccine30[44] * (1 - trans_alpha[n43] * (1 - delta_prop[n]) - trans_delta[n43] * delta_prop[n]) +
       vaccine30[45] * (1 - trans_alpha[n44] * (1 - delta_prop[n]) - trans_delta[n44] * delta_prop[n]) +
       vaccine30[46] * (1 - trans_alpha[n45] * (1 - delta_prop[n]) - trans_delta[n45] * delta_prop[n]) +
       vaccine30[47] * (1 - trans_alpha[n46] * (1 - delta_prop[n]) - trans_delta[n46] * delta_prop[n]) +
       vaccine30[48] * (1 - trans_alpha[n47] * (1 - delta_prop[n]) - trans_delta[n47] * delta_prop[n]) +
       vaccine30[49] * (1 - trans_alpha[n48] * (1 - delta_prop[n]) - trans_delta[n48] * delta_prop[n]) +
       vaccine30[50] * (1 - trans_alpha[n49] * (1 - delta_prop[n]) - trans_delta[n49] * delta_prop[n]) +
       vaccine30[51] * (1 - trans_alpha[n50] * (1 - delta_prop[n]) - trans_delta[n50] * delta_prop[n]) +
       vaccine30[52] * (1 - trans_alpha[n51] * (1 - delta_prop[n]) - trans_delta[n51] * delta_prop[n]) +
       vaccine30[53] * (1 - trans_alpha[n52] * (1 - delta_prop[n]) - trans_delta[n52] * delta_prop[n]) +
       vaccine30[54] * (1 - trans_alpha[n53] * (1 - delta_prop[n]) - trans_delta[n53] * delta_prop[n]) +
       vaccine30[55] * (1 - trans_alpha[n54] * (1 - delta_prop[n]) - trans_delta[n54] * delta_prop[n]) +
       vaccine30[56] * (1 - trans_alpha[n55] * (1 - delta_prop[n]) - trans_delta[n55] * delta_prop[n]) +
       vaccine30[57] * (1 - trans_alpha[n56] * (1 - delta_prop[n]) - trans_delta[n56] * delta_prop[n]) +
       vaccine30[58] * (1 - trans_alpha[n57] * (1 - delta_prop[n]) - trans_delta[n57] * delta_prop[n]) +
       vaccine30[59] * (1 - trans_alpha[n58] * (1 - delta_prop[n]) - trans_delta[n58] * delta_prop[n]) +
       vaccine30[60] * (1 - trans_alpha[n59] * (1 - delta_prop[n]) - trans_delta[n59] * delta_prop[n]) +
       vaccine30[61] * (1 - trans_alpha[n60] * (1 - delta_prop[n]) - trans_delta[n60] * delta_prop[n]) +
       vaccine30[62] * (1 - trans_alpha[n61] * (1 - delta_prop[n]) - trans_delta[n61] * delta_prop[n]) +
       vaccine30[63] * (1 - trans_alpha[n62] * (1 - delta_prop[n]) - trans_delta[n62] * delta_prop[n]) +
       vaccine30[64] * (1 - trans_alpha[n63] * (1 - delta_prop[n]) - trans_delta[n63] * delta_prop[n]) +
       vaccine30[65] * (1 - trans_alpha[n64] * (1 - delta_prop[n]) - trans_delta[n64] * delta_prop[n]) +
       vaccine30[66] * (1 - trans_alpha[n65] * (1 - delta_prop[n]) - trans_delta[n65] * delta_prop[n]) +
       vaccine30[67] * (1 - trans_alpha[n66] * (1 - delta_prop[n]) - trans_delta[n66] * delta_prop[n]) +
       vaccine30[68] * (1 - trans_alpha[n67] * (1 - delta_prop[n]) - trans_delta[n67] * delta_prop[n]) +
       vaccine30[69] * (1 - trans_alpha[n68] * (1 - delta_prop[n]) - trans_delta[n68] * delta_prop[n]) +
       vaccine30[70] * (1 - trans_alpha[n69] * (1 - delta_prop[n]) - trans_delta[n69] * delta_prop[n]) +
       vaccine30[71] * (1 - trans_alpha[n70] * (1 - delta_prop[n]) - trans_delta[n70] * delta_prop[n]) +
       vaccine30[72] * (1 - trans_alpha[n71] * (1 - delta_prop[n]) - trans_delta[n71] * delta_prop[n]) +
       vaccine30[73] * (1 - trans_alpha[n72] * (1 - delta_prop[n]) - trans_delta[n72] * delta_prop[n]) +
       vaccine30[74] * (1 - trans_alpha[n73] * (1 - delta_prop[n]) - trans_delta[n73] * delta_prop[n]) +
       vaccine30[75] * (1 - trans_alpha[n74] * (1 - delta_prop[n]) - trans_delta[n74] * delta_prop[n]) +
       vaccine30[76] * (1 - trans_alpha[n75] * (1 - delta_prop[n]) - trans_delta[n75] * delta_prop[n]) +
       vaccine30[77] * (1 - trans_alpha[n76] * (1 - delta_prop[n]) - trans_delta[n76] * delta_prop[n]) +
       vaccine30[78] * (1 - trans_alpha[n77] * (1 - delta_prop[n]) - trans_delta[n77] * delta_prop[n]) +
       vaccine30[79] * (1 - trans_alpha[n78] * (1 - delta_prop[n]) - trans_delta[n78] * delta_prop[n]) +
       vaccine30[80] * (1 - trans_alpha[n79] * (1 - delta_prop[n]) - trans_delta[n79] * delta_prop[n]) +
       vaccine30[81] * (1 - trans_alpha[n80] * (1 - delta_prop[n]) - trans_delta[n80] * delta_prop[n]) +
       vaccine30[82] * (1 - trans_alpha[n81] * (1 - delta_prop[n]) - trans_delta[n81] * delta_prop[n]) +
       vaccine30[83] * (1 - trans_alpha[n82] * (1 - delta_prop[n]) - trans_delta[n82] * delta_prop[n]) +
       vaccine30[84] * (1 - trans_alpha[n83] * (1 - delta_prop[n]) - trans_delta[n83] * delta_prop[n]) +
       vaccine30[85] * (1 - trans_alpha[n84] * (1 - delta_prop[n]) - trans_delta[n84] * delta_prop[n]) +
       vaccine30[86] * (1 - trans_alpha[n85] * (1 - delta_prop[n]) - trans_delta[n85] * delta_prop[n]) +
       vaccine30[87] * (1 - trans_alpha[n86] * (1 - delta_prop[n]) - trans_delta[n86] * delta_prop[n]) +
       vaccine30[88] * (1 - trans_alpha[n87] * (1 - delta_prop[n]) - trans_delta[n87] * delta_prop[n]) +
       vaccine30[89] * (1 - trans_alpha[n88] * (1 - delta_prop[n]) - trans_delta[n88] * delta_prop[n]) +
       vaccine30[90] * (1 - trans_alpha[n89] * (1 - delta_prop[n]) - trans_delta[n89] * delta_prop[n]) +
       vaccine30[91] * (1 - trans_alpha[n90] * (1 - delta_prop[n]) - trans_delta[n90] * delta_prop[n]) +
       vaccine30[92] * (1 - trans_alpha[n91] * (1 - delta_prop[n]) - trans_delta[n91] * delta_prop[n]) +
       vaccine30[93] * (1 - trans_alpha[n92] * (1 - delta_prop[n]) - trans_delta[n92] * delta_prop[n]) +
       vaccine30[94] * (1 - trans_alpha[n93] * (1 - delta_prop[n]) - trans_delta[n93] * delta_prop[n]) +
       vaccine30[95] * (1 - trans_alpha[n94] * (1 - delta_prop[n]) - trans_delta[n94] * delta_prop[n]) +
       vaccine30[96] * (1 - trans_alpha[n95] * (1 - delta_prop[n]) - trans_delta[n95] * delta_prop[n]) +
       vaccine30[97] * (1 - trans_alpha[n96] * (1 - delta_prop[n]) - trans_delta[n96] * delta_prop[n]) +
       vaccine30[98] * (1 - trans_alpha[n97] * (1 - delta_prop[n]) - trans_delta[n97] * delta_prop[n]) +
       vaccine30[99] * (1 - trans_alpha[n98] * (1 - delta_prop[n]) - trans_delta[n98] * delta_prop[n]) +
       ifelse((1 - cum_vac30[n] - ((sum(new_case30[1:9]) + sum(pred30_sofar[10:n])) / population30)) < 0,
              0,
              1 - cum_vac30[n] - ((sum(new_case30[1:9]) + sum(pred30_sofar[10:n])) / population30)))
  
  poi40 <- pred40_sofar[n] *
    (vaccine40[1] *  (1 - trans_alpha[n00] * (1 - delta_prop[n]) - trans_delta[n00] * delta_prop[n]) +
       vaccine40[2] *  (1 - trans_alpha[n01] * (1 - delta_prop[n]) - trans_delta[n01] * delta_prop[n]) +
       vaccine40[3] *  (1 - trans_alpha[n02] * (1 - delta_prop[n]) - trans_delta[n02] * delta_prop[n]) +
       vaccine40[4] *  (1 - trans_alpha[n03] * (1 - delta_prop[n]) - trans_delta[n03] * delta_prop[n]) +
       vaccine40[5] *  (1 - trans_alpha[n04] * (1 - delta_prop[n]) - trans_delta[n04] * delta_prop[n]) +
       vaccine40[6] *  (1 - trans_alpha[n05] * (1 - delta_prop[n]) - trans_delta[n05] * delta_prop[n]) +
       vaccine40[7] *  (1 - trans_alpha[n06] * (1 - delta_prop[n]) - trans_delta[n06] * delta_prop[n]) +
       vaccine40[8] *  (1 - trans_alpha[n07] * (1 - delta_prop[n]) - trans_delta[n07] * delta_prop[n]) +
       vaccine40[9] *  (1 - trans_alpha[n08] * (1 - delta_prop[n]) - trans_delta[n08] * delta_prop[n]) +
       vaccine40[10] * (1 - trans_alpha[n09] * (1 - delta_prop[n]) - trans_delta[n09] * delta_prop[n]) +
       vaccine40[11] * (1 - trans_alpha[n10] * (1 - delta_prop[n]) - trans_delta[n10] * delta_prop[n]) +
       vaccine40[12] * (1 - trans_alpha[n11] * (1 - delta_prop[n]) - trans_delta[n11] * delta_prop[n]) +
       vaccine40[13] * (1 - trans_alpha[n12] * (1 - delta_prop[n]) - trans_delta[n12] * delta_prop[n]) +
       vaccine40[14] * (1 - trans_alpha[n13] * (1 - delta_prop[n]) - trans_delta[n13] * delta_prop[n]) +
       vaccine40[15] * (1 - trans_alpha[n14] * (1 - delta_prop[n]) - trans_delta[n14] * delta_prop[n]) +
       vaccine40[16] * (1 - trans_alpha[n15] * (1 - delta_prop[n]) - trans_delta[n15] * delta_prop[n]) +
       vaccine40[17] * (1 - trans_alpha[n16] * (1 - delta_prop[n]) - trans_delta[n16] * delta_prop[n]) +
       vaccine40[18] * (1 - trans_alpha[n17] * (1 - delta_prop[n]) - trans_delta[n17] * delta_prop[n]) +
       vaccine40[19] * (1 - trans_alpha[n18] * (1 - delta_prop[n]) - trans_delta[n18] * delta_prop[n]) +
       vaccine40[20] * (1 - trans_alpha[n19] * (1 - delta_prop[n]) - trans_delta[n19] * delta_prop[n]) +
       vaccine40[21] * (1 - trans_alpha[n20] * (1 - delta_prop[n]) - trans_delta[n20] * delta_prop[n]) +
       vaccine40[22] * (1 - trans_alpha[n21] * (1 - delta_prop[n]) - trans_delta[n21] * delta_prop[n]) +
       vaccine40[23] * (1 - trans_alpha[n22] * (1 - delta_prop[n]) - trans_delta[n22] * delta_prop[n]) +
       vaccine40[24] * (1 - trans_alpha[n23] * (1 - delta_prop[n]) - trans_delta[n23] * delta_prop[n]) +
       vaccine40[25] * (1 - trans_alpha[n24] * (1 - delta_prop[n]) - trans_delta[n24] * delta_prop[n]) +
       vaccine40[26] * (1 - trans_alpha[n25] * (1 - delta_prop[n]) - trans_delta[n25] * delta_prop[n]) +
       vaccine40[27] * (1 - trans_alpha[n26] * (1 - delta_prop[n]) - trans_delta[n26] * delta_prop[n]) +
       vaccine40[28] * (1 - trans_alpha[n27] * (1 - delta_prop[n]) - trans_delta[n27] * delta_prop[n]) +
       vaccine40[29] * (1 - trans_alpha[n28] * (1 - delta_prop[n]) - trans_delta[n28] * delta_prop[n]) +
       vaccine40[30] * (1 - trans_alpha[n29] * (1 - delta_prop[n]) - trans_delta[n29] * delta_prop[n]) +
       vaccine40[31] * (1 - trans_alpha[n30] * (1 - delta_prop[n]) - trans_delta[n30] * delta_prop[n]) +
       vaccine40[32] * (1 - trans_alpha[n31] * (1 - delta_prop[n]) - trans_delta[n31] * delta_prop[n]) +
       vaccine40[33] * (1 - trans_alpha[n32] * (1 - delta_prop[n]) - trans_delta[n32] * delta_prop[n]) +
       vaccine40[34] * (1 - trans_alpha[n33] * (1 - delta_prop[n]) - trans_delta[n33] * delta_prop[n]) +
       vaccine40[35] * (1 - trans_alpha[n34] * (1 - delta_prop[n]) - trans_delta[n34] * delta_prop[n]) +
       vaccine40[36] * (1 - trans_alpha[n35] * (1 - delta_prop[n]) - trans_delta[n35] * delta_prop[n]) +
       vaccine40[37] * (1 - trans_alpha[n36] * (1 - delta_prop[n]) - trans_delta[n36] * delta_prop[n]) +
       vaccine40[38] * (1 - trans_alpha[n37] * (1 - delta_prop[n]) - trans_delta[n37] * delta_prop[n]) +
       vaccine40[39] * (1 - trans_alpha[n38] * (1 - delta_prop[n]) - trans_delta[n38] * delta_prop[n]) +
       vaccine40[40] * (1 - trans_alpha[n39] * (1 - delta_prop[n]) - trans_delta[n39] * delta_prop[n]) +
       vaccine40[41] * (1 - trans_alpha[n40] * (1 - delta_prop[n]) - trans_delta[n40] * delta_prop[n]) +
       vaccine40[42] * (1 - trans_alpha[n41] * (1 - delta_prop[n]) - trans_delta[n41] * delta_prop[n]) +
       vaccine40[43] * (1 - trans_alpha[n42] * (1 - delta_prop[n]) - trans_delta[n42] * delta_prop[n]) +
       vaccine40[44] * (1 - trans_alpha[n43] * (1 - delta_prop[n]) - trans_delta[n43] * delta_prop[n]) +
       vaccine40[45] * (1 - trans_alpha[n44] * (1 - delta_prop[n]) - trans_delta[n44] * delta_prop[n]) +
       vaccine40[46] * (1 - trans_alpha[n45] * (1 - delta_prop[n]) - trans_delta[n45] * delta_prop[n]) +
       vaccine40[47] * (1 - trans_alpha[n46] * (1 - delta_prop[n]) - trans_delta[n46] * delta_prop[n]) +
       vaccine40[48] * (1 - trans_alpha[n47] * (1 - delta_prop[n]) - trans_delta[n47] * delta_prop[n]) +
       vaccine40[49] * (1 - trans_alpha[n48] * (1 - delta_prop[n]) - trans_delta[n48] * delta_prop[n]) +
       vaccine40[50] * (1 - trans_alpha[n49] * (1 - delta_prop[n]) - trans_delta[n49] * delta_prop[n]) +
       vaccine40[51] * (1 - trans_alpha[n50] * (1 - delta_prop[n]) - trans_delta[n50] * delta_prop[n]) +
       vaccine40[52] * (1 - trans_alpha[n51] * (1 - delta_prop[n]) - trans_delta[n51] * delta_prop[n]) +
       vaccine40[53] * (1 - trans_alpha[n52] * (1 - delta_prop[n]) - trans_delta[n52] * delta_prop[n]) +
       vaccine40[54] * (1 - trans_alpha[n53] * (1 - delta_prop[n]) - trans_delta[n53] * delta_prop[n]) +
       vaccine40[55] * (1 - trans_alpha[n54] * (1 - delta_prop[n]) - trans_delta[n54] * delta_prop[n]) +
       vaccine40[56] * (1 - trans_alpha[n55] * (1 - delta_prop[n]) - trans_delta[n55] * delta_prop[n]) +
       vaccine40[57] * (1 - trans_alpha[n56] * (1 - delta_prop[n]) - trans_delta[n56] * delta_prop[n]) +
       vaccine40[58] * (1 - trans_alpha[n57] * (1 - delta_prop[n]) - trans_delta[n57] * delta_prop[n]) +
       vaccine40[59] * (1 - trans_alpha[n58] * (1 - delta_prop[n]) - trans_delta[n58] * delta_prop[n]) +
       vaccine40[60] * (1 - trans_alpha[n59] * (1 - delta_prop[n]) - trans_delta[n59] * delta_prop[n]) +
       vaccine40[61] * (1 - trans_alpha[n60] * (1 - delta_prop[n]) - trans_delta[n60] * delta_prop[n]) +
       vaccine40[62] * (1 - trans_alpha[n61] * (1 - delta_prop[n]) - trans_delta[n61] * delta_prop[n]) +
       vaccine40[63] * (1 - trans_alpha[n62] * (1 - delta_prop[n]) - trans_delta[n62] * delta_prop[n]) +
       vaccine40[64] * (1 - trans_alpha[n63] * (1 - delta_prop[n]) - trans_delta[n63] * delta_prop[n]) +
       vaccine40[65] * (1 - trans_alpha[n64] * (1 - delta_prop[n]) - trans_delta[n64] * delta_prop[n]) +
       vaccine40[66] * (1 - trans_alpha[n65] * (1 - delta_prop[n]) - trans_delta[n65] * delta_prop[n]) +
       vaccine40[67] * (1 - trans_alpha[n66] * (1 - delta_prop[n]) - trans_delta[n66] * delta_prop[n]) +
       vaccine40[68] * (1 - trans_alpha[n67] * (1 - delta_prop[n]) - trans_delta[n67] * delta_prop[n]) +
       vaccine40[69] * (1 - trans_alpha[n68] * (1 - delta_prop[n]) - trans_delta[n68] * delta_prop[n]) +
       vaccine40[70] * (1 - trans_alpha[n69] * (1 - delta_prop[n]) - trans_delta[n69] * delta_prop[n]) +
       vaccine40[71] * (1 - trans_alpha[n70] * (1 - delta_prop[n]) - trans_delta[n70] * delta_prop[n]) +
       vaccine40[72] * (1 - trans_alpha[n71] * (1 - delta_prop[n]) - trans_delta[n71] * delta_prop[n]) +
       vaccine40[73] * (1 - trans_alpha[n72] * (1 - delta_prop[n]) - trans_delta[n72] * delta_prop[n]) +
       vaccine40[74] * (1 - trans_alpha[n73] * (1 - delta_prop[n]) - trans_delta[n73] * delta_prop[n]) +
       vaccine40[75] * (1 - trans_alpha[n74] * (1 - delta_prop[n]) - trans_delta[n74] * delta_prop[n]) +
       vaccine40[76] * (1 - trans_alpha[n75] * (1 - delta_prop[n]) - trans_delta[n75] * delta_prop[n]) +
       vaccine40[77] * (1 - trans_alpha[n76] * (1 - delta_prop[n]) - trans_delta[n76] * delta_prop[n]) +
       vaccine40[78] * (1 - trans_alpha[n77] * (1 - delta_prop[n]) - trans_delta[n77] * delta_prop[n]) +
       vaccine40[79] * (1 - trans_alpha[n78] * (1 - delta_prop[n]) - trans_delta[n78] * delta_prop[n]) +
       vaccine40[80] * (1 - trans_alpha[n79] * (1 - delta_prop[n]) - trans_delta[n79] * delta_prop[n]) +
       vaccine40[81] * (1 - trans_alpha[n80] * (1 - delta_prop[n]) - trans_delta[n80] * delta_prop[n]) +
       vaccine40[82] * (1 - trans_alpha[n81] * (1 - delta_prop[n]) - trans_delta[n81] * delta_prop[n]) +
       vaccine40[83] * (1 - trans_alpha[n82] * (1 - delta_prop[n]) - trans_delta[n82] * delta_prop[n]) +
       vaccine40[84] * (1 - trans_alpha[n83] * (1 - delta_prop[n]) - trans_delta[n83] * delta_prop[n]) +
       vaccine40[85] * (1 - trans_alpha[n84] * (1 - delta_prop[n]) - trans_delta[n84] * delta_prop[n]) +
       vaccine40[86] * (1 - trans_alpha[n85] * (1 - delta_prop[n]) - trans_delta[n85] * delta_prop[n]) +
       vaccine40[87] * (1 - trans_alpha[n86] * (1 - delta_prop[n]) - trans_delta[n86] * delta_prop[n]) +
       vaccine40[88] * (1 - trans_alpha[n87] * (1 - delta_prop[n]) - trans_delta[n87] * delta_prop[n]) +
       vaccine40[89] * (1 - trans_alpha[n88] * (1 - delta_prop[n]) - trans_delta[n88] * delta_prop[n]) +
       vaccine40[90] * (1 - trans_alpha[n89] * (1 - delta_prop[n]) - trans_delta[n89] * delta_prop[n]) +
       vaccine40[91] * (1 - trans_alpha[n90] * (1 - delta_prop[n]) - trans_delta[n90] * delta_prop[n]) +
       vaccine40[92] * (1 - trans_alpha[n91] * (1 - delta_prop[n]) - trans_delta[n91] * delta_prop[n]) +
       vaccine40[93] * (1 - trans_alpha[n92] * (1 - delta_prop[n]) - trans_delta[n92] * delta_prop[n]) +
       vaccine40[94] * (1 - trans_alpha[n93] * (1 - delta_prop[n]) - trans_delta[n93] * delta_prop[n]) +
       vaccine40[95] * (1 - trans_alpha[n94] * (1 - delta_prop[n]) - trans_delta[n94] * delta_prop[n]) +
       vaccine40[96] * (1 - trans_alpha[n95] * (1 - delta_prop[n]) - trans_delta[n95] * delta_prop[n]) +
       vaccine40[97] * (1 - trans_alpha[n96] * (1 - delta_prop[n]) - trans_delta[n96] * delta_prop[n]) +
       vaccine40[98] * (1 - trans_alpha[n97] * (1 - delta_prop[n]) - trans_delta[n97] * delta_prop[n]) +
       vaccine40[99] * (1 - trans_alpha[n98] * (1 - delta_prop[n]) - trans_delta[n98] * delta_prop[n]) +
       ifelse((1 - cum_vac40[n] - ((sum(new_case40[1:9]) + sum(pred40_sofar[10:n])) / population40)) < 0,
              0,
              1 - cum_vac40[n] - ((sum(new_case40[1:9]) + sum(pred40_sofar[10:n])) / population40)))
  
  poi50 <- pred50_sofar[n] *
    (vaccine50[1] *  (1 - trans_alpha[n00] * (1 - delta_prop[n]) - trans_delta[n00] * delta_prop[n]) +
       vaccine50[2] *  (1 - trans_alpha[n01] * (1 - delta_prop[n]) - trans_delta[n01] * delta_prop[n]) +
       vaccine50[3] *  (1 - trans_alpha[n02] * (1 - delta_prop[n]) - trans_delta[n02] * delta_prop[n]) +
       vaccine50[4] *  (1 - trans_alpha[n03] * (1 - delta_prop[n]) - trans_delta[n03] * delta_prop[n]) +
       vaccine50[5] *  (1 - trans_alpha[n04] * (1 - delta_prop[n]) - trans_delta[n04] * delta_prop[n]) +
       vaccine50[6] *  (1 - trans_alpha[n05] * (1 - delta_prop[n]) - trans_delta[n05] * delta_prop[n]) +
       vaccine50[7] *  (1 - trans_alpha[n06] * (1 - delta_prop[n]) - trans_delta[n06] * delta_prop[n]) +
       vaccine50[8] *  (1 - trans_alpha[n07] * (1 - delta_prop[n]) - trans_delta[n07] * delta_prop[n]) +
       vaccine50[9] *  (1 - trans_alpha[n08] * (1 - delta_prop[n]) - trans_delta[n08] * delta_prop[n]) +
       vaccine50[10] * (1 - trans_alpha[n09] * (1 - delta_prop[n]) - trans_delta[n09] * delta_prop[n]) +
       vaccine50[11] * (1 - trans_alpha[n10] * (1 - delta_prop[n]) - trans_delta[n10] * delta_prop[n]) +
       vaccine50[12] * (1 - trans_alpha[n11] * (1 - delta_prop[n]) - trans_delta[n11] * delta_prop[n]) +
       vaccine50[13] * (1 - trans_alpha[n12] * (1 - delta_prop[n]) - trans_delta[n12] * delta_prop[n]) +
       vaccine50[14] * (1 - trans_alpha[n13] * (1 - delta_prop[n]) - trans_delta[n13] * delta_prop[n]) +
       vaccine50[15] * (1 - trans_alpha[n14] * (1 - delta_prop[n]) - trans_delta[n14] * delta_prop[n]) +
       vaccine50[16] * (1 - trans_alpha[n15] * (1 - delta_prop[n]) - trans_delta[n15] * delta_prop[n]) +
       vaccine50[17] * (1 - trans_alpha[n16] * (1 - delta_prop[n]) - trans_delta[n16] * delta_prop[n]) +
       vaccine50[18] * (1 - trans_alpha[n17] * (1 - delta_prop[n]) - trans_delta[n17] * delta_prop[n]) +
       vaccine50[19] * (1 - trans_alpha[n18] * (1 - delta_prop[n]) - trans_delta[n18] * delta_prop[n]) +
       vaccine50[20] * (1 - trans_alpha[n19] * (1 - delta_prop[n]) - trans_delta[n19] * delta_prop[n]) +
       vaccine50[21] * (1 - trans_alpha[n20] * (1 - delta_prop[n]) - trans_delta[n20] * delta_prop[n]) +
       vaccine50[22] * (1 - trans_alpha[n21] * (1 - delta_prop[n]) - trans_delta[n21] * delta_prop[n]) +
       vaccine50[23] * (1 - trans_alpha[n22] * (1 - delta_prop[n]) - trans_delta[n22] * delta_prop[n]) +
       vaccine50[24] * (1 - trans_alpha[n23] * (1 - delta_prop[n]) - trans_delta[n23] * delta_prop[n]) +
       vaccine50[25] * (1 - trans_alpha[n24] * (1 - delta_prop[n]) - trans_delta[n24] * delta_prop[n]) +
       vaccine50[26] * (1 - trans_alpha[n25] * (1 - delta_prop[n]) - trans_delta[n25] * delta_prop[n]) +
       vaccine50[27] * (1 - trans_alpha[n26] * (1 - delta_prop[n]) - trans_delta[n26] * delta_prop[n]) +
       vaccine50[28] * (1 - trans_alpha[n27] * (1 - delta_prop[n]) - trans_delta[n27] * delta_prop[n]) +
       vaccine50[29] * (1 - trans_alpha[n28] * (1 - delta_prop[n]) - trans_delta[n28] * delta_prop[n]) +
       vaccine50[30] * (1 - trans_alpha[n29] * (1 - delta_prop[n]) - trans_delta[n29] * delta_prop[n]) +
       vaccine50[31] * (1 - trans_alpha[n30] * (1 - delta_prop[n]) - trans_delta[n30] * delta_prop[n]) +
       vaccine50[32] * (1 - trans_alpha[n31] * (1 - delta_prop[n]) - trans_delta[n31] * delta_prop[n]) +
       vaccine50[33] * (1 - trans_alpha[n32] * (1 - delta_prop[n]) - trans_delta[n32] * delta_prop[n]) +
       vaccine50[34] * (1 - trans_alpha[n33] * (1 - delta_prop[n]) - trans_delta[n33] * delta_prop[n]) +
       vaccine50[35] * (1 - trans_alpha[n34] * (1 - delta_prop[n]) - trans_delta[n34] * delta_prop[n]) +
       vaccine50[36] * (1 - trans_alpha[n35] * (1 - delta_prop[n]) - trans_delta[n35] * delta_prop[n]) +
       vaccine50[37] * (1 - trans_alpha[n36] * (1 - delta_prop[n]) - trans_delta[n36] * delta_prop[n]) +
       vaccine50[38] * (1 - trans_alpha[n37] * (1 - delta_prop[n]) - trans_delta[n37] * delta_prop[n]) +
       vaccine50[39] * (1 - trans_alpha[n38] * (1 - delta_prop[n]) - trans_delta[n38] * delta_prop[n]) +
       vaccine50[40] * (1 - trans_alpha[n39] * (1 - delta_prop[n]) - trans_delta[n39] * delta_prop[n]) +
       vaccine50[41] * (1 - trans_alpha[n40] * (1 - delta_prop[n]) - trans_delta[n40] * delta_prop[n]) +
       vaccine50[42] * (1 - trans_alpha[n41] * (1 - delta_prop[n]) - trans_delta[n41] * delta_prop[n]) +
       vaccine50[43] * (1 - trans_alpha[n42] * (1 - delta_prop[n]) - trans_delta[n42] * delta_prop[n]) +
       vaccine50[44] * (1 - trans_alpha[n43] * (1 - delta_prop[n]) - trans_delta[n43] * delta_prop[n]) +
       vaccine50[45] * (1 - trans_alpha[n44] * (1 - delta_prop[n]) - trans_delta[n44] * delta_prop[n]) +
       vaccine50[46] * (1 - trans_alpha[n45] * (1 - delta_prop[n]) - trans_delta[n45] * delta_prop[n]) +
       vaccine50[47] * (1 - trans_alpha[n46] * (1 - delta_prop[n]) - trans_delta[n46] * delta_prop[n]) +
       vaccine50[48] * (1 - trans_alpha[n47] * (1 - delta_prop[n]) - trans_delta[n47] * delta_prop[n]) +
       vaccine50[49] * (1 - trans_alpha[n48] * (1 - delta_prop[n]) - trans_delta[n48] * delta_prop[n]) +
       vaccine50[50] * (1 - trans_alpha[n49] * (1 - delta_prop[n]) - trans_delta[n49] * delta_prop[n]) +
       vaccine50[51] * (1 - trans_alpha[n50] * (1 - delta_prop[n]) - trans_delta[n50] * delta_prop[n]) +
       vaccine50[52] * (1 - trans_alpha[n51] * (1 - delta_prop[n]) - trans_delta[n51] * delta_prop[n]) +
       vaccine50[53] * (1 - trans_alpha[n52] * (1 - delta_prop[n]) - trans_delta[n52] * delta_prop[n]) +
       vaccine50[54] * (1 - trans_alpha[n53] * (1 - delta_prop[n]) - trans_delta[n53] * delta_prop[n]) +
       vaccine50[55] * (1 - trans_alpha[n54] * (1 - delta_prop[n]) - trans_delta[n54] * delta_prop[n]) +
       vaccine50[56] * (1 - trans_alpha[n55] * (1 - delta_prop[n]) - trans_delta[n55] * delta_prop[n]) +
       vaccine50[57] * (1 - trans_alpha[n56] * (1 - delta_prop[n]) - trans_delta[n56] * delta_prop[n]) +
       vaccine50[58] * (1 - trans_alpha[n57] * (1 - delta_prop[n]) - trans_delta[n57] * delta_prop[n]) +
       vaccine50[59] * (1 - trans_alpha[n58] * (1 - delta_prop[n]) - trans_delta[n58] * delta_prop[n]) +
       vaccine50[60] * (1 - trans_alpha[n59] * (1 - delta_prop[n]) - trans_delta[n59] * delta_prop[n]) +
       vaccine50[61] * (1 - trans_alpha[n60] * (1 - delta_prop[n]) - trans_delta[n60] * delta_prop[n]) +
       vaccine50[62] * (1 - trans_alpha[n61] * (1 - delta_prop[n]) - trans_delta[n61] * delta_prop[n]) +
       vaccine50[63] * (1 - trans_alpha[n62] * (1 - delta_prop[n]) - trans_delta[n62] * delta_prop[n]) +
       vaccine50[64] * (1 - trans_alpha[n63] * (1 - delta_prop[n]) - trans_delta[n63] * delta_prop[n]) +
       vaccine50[65] * (1 - trans_alpha[n64] * (1 - delta_prop[n]) - trans_delta[n64] * delta_prop[n]) +
       vaccine50[66] * (1 - trans_alpha[n65] * (1 - delta_prop[n]) - trans_delta[n65] * delta_prop[n]) +
       vaccine50[67] * (1 - trans_alpha[n66] * (1 - delta_prop[n]) - trans_delta[n66] * delta_prop[n]) +
       vaccine50[68] * (1 - trans_alpha[n67] * (1 - delta_prop[n]) - trans_delta[n67] * delta_prop[n]) +
       vaccine50[69] * (1 - trans_alpha[n68] * (1 - delta_prop[n]) - trans_delta[n68] * delta_prop[n]) +
       vaccine50[70] * (1 - trans_alpha[n69] * (1 - delta_prop[n]) - trans_delta[n69] * delta_prop[n]) +
       vaccine50[71] * (1 - trans_alpha[n70] * (1 - delta_prop[n]) - trans_delta[n70] * delta_prop[n]) +
       vaccine50[72] * (1 - trans_alpha[n71] * (1 - delta_prop[n]) - trans_delta[n71] * delta_prop[n]) +
       vaccine50[73] * (1 - trans_alpha[n72] * (1 - delta_prop[n]) - trans_delta[n72] * delta_prop[n]) +
       vaccine50[74] * (1 - trans_alpha[n73] * (1 - delta_prop[n]) - trans_delta[n73] * delta_prop[n]) +
       vaccine50[75] * (1 - trans_alpha[n74] * (1 - delta_prop[n]) - trans_delta[n74] * delta_prop[n]) +
       vaccine50[76] * (1 - trans_alpha[n75] * (1 - delta_prop[n]) - trans_delta[n75] * delta_prop[n]) +
       vaccine50[77] * (1 - trans_alpha[n76] * (1 - delta_prop[n]) - trans_delta[n76] * delta_prop[n]) +
       vaccine50[78] * (1 - trans_alpha[n77] * (1 - delta_prop[n]) - trans_delta[n77] * delta_prop[n]) +
       vaccine50[79] * (1 - trans_alpha[n78] * (1 - delta_prop[n]) - trans_delta[n78] * delta_prop[n]) +
       vaccine50[80] * (1 - trans_alpha[n79] * (1 - delta_prop[n]) - trans_delta[n79] * delta_prop[n]) +
       vaccine50[81] * (1 - trans_alpha[n80] * (1 - delta_prop[n]) - trans_delta[n80] * delta_prop[n]) +
       vaccine50[82] * (1 - trans_alpha[n81] * (1 - delta_prop[n]) - trans_delta[n81] * delta_prop[n]) +
       vaccine50[83] * (1 - trans_alpha[n82] * (1 - delta_prop[n]) - trans_delta[n82] * delta_prop[n]) +
       vaccine50[84] * (1 - trans_alpha[n83] * (1 - delta_prop[n]) - trans_delta[n83] * delta_prop[n]) +
       vaccine50[85] * (1 - trans_alpha[n84] * (1 - delta_prop[n]) - trans_delta[n84] * delta_prop[n]) +
       vaccine50[86] * (1 - trans_alpha[n85] * (1 - delta_prop[n]) - trans_delta[n85] * delta_prop[n]) +
       vaccine50[87] * (1 - trans_alpha[n86] * (1 - delta_prop[n]) - trans_delta[n86] * delta_prop[n]) +
       vaccine50[88] * (1 - trans_alpha[n87] * (1 - delta_prop[n]) - trans_delta[n87] * delta_prop[n]) +
       vaccine50[89] * (1 - trans_alpha[n88] * (1 - delta_prop[n]) - trans_delta[n88] * delta_prop[n]) +
       vaccine50[90] * (1 - trans_alpha[n89] * (1 - delta_prop[n]) - trans_delta[n89] * delta_prop[n]) +
       vaccine50[91] * (1 - trans_alpha[n90] * (1 - delta_prop[n]) - trans_delta[n90] * delta_prop[n]) +
       vaccine50[92] * (1 - trans_alpha[n91] * (1 - delta_prop[n]) - trans_delta[n91] * delta_prop[n]) +
       vaccine50[93] * (1 - trans_alpha[n92] * (1 - delta_prop[n]) - trans_delta[n92] * delta_prop[n]) +
       vaccine50[94] * (1 - trans_alpha[n93] * (1 - delta_prop[n]) - trans_delta[n93] * delta_prop[n]) +
       vaccine50[95] * (1 - trans_alpha[n94] * (1 - delta_prop[n]) - trans_delta[n94] * delta_prop[n]) +
       vaccine50[96] * (1 - trans_alpha[n95] * (1 - delta_prop[n]) - trans_delta[n95] * delta_prop[n]) +
       vaccine50[97] * (1 - trans_alpha[n96] * (1 - delta_prop[n]) - trans_delta[n96] * delta_prop[n]) +
       vaccine50[98] * (1 - trans_alpha[n97] * (1 - delta_prop[n]) - trans_delta[n97] * delta_prop[n]) +
       vaccine50[99] * (1 - trans_alpha[n98] * (1 - delta_prop[n]) - trans_delta[n98] * delta_prop[n]) +
       ifelse((1 - cum_vac50[n] - ((sum(new_case50[1:9]) + sum(pred50_sofar[10:n])) / population50)) < 0,
              0,
              1 - cum_vac50[n] - ((sum(new_case50[1:9]) + sum(pred50_sofar[10:n])) / population50)))
  
  poi60 <-pred60_sofar[n] *
    (vaccine60[1] *  (1 - trans_alpha[n00] * (1 - delta_prop[n]) - trans_delta[n00] * delta_prop[n]) +
       vaccine60[2] *  (1 - trans_alpha[n01] * (1 - delta_prop[n]) - trans_delta[n01] * delta_prop[n]) +
       vaccine60[3] *  (1 - trans_alpha[n02] * (1 - delta_prop[n]) - trans_delta[n02] * delta_prop[n]) +
       vaccine60[4] *  (1 - trans_alpha[n03] * (1 - delta_prop[n]) - trans_delta[n03] * delta_prop[n]) +
       vaccine60[5] *  (1 - trans_alpha[n04] * (1 - delta_prop[n]) - trans_delta[n04] * delta_prop[n]) +
       vaccine60[6] *  (1 - trans_alpha[n05] * (1 - delta_prop[n]) - trans_delta[n05] * delta_prop[n]) +
       vaccine60[7] *  (1 - trans_alpha[n06] * (1 - delta_prop[n]) - trans_delta[n06] * delta_prop[n]) +
       vaccine60[8] *  (1 - trans_alpha[n07] * (1 - delta_prop[n]) - trans_delta[n07] * delta_prop[n]) +
       vaccine60[9] *  (1 - trans_alpha[n08] * (1 - delta_prop[n]) - trans_delta[n08] * delta_prop[n]) +
       vaccine60[10] * (1 - trans_alpha[n09] * (1 - delta_prop[n]) - trans_delta[n09] * delta_prop[n]) +
       vaccine60[11] * (1 - trans_alpha[n10] * (1 - delta_prop[n]) - trans_delta[n10] * delta_prop[n]) +
       vaccine60[12] * (1 - trans_alpha[n11] * (1 - delta_prop[n]) - trans_delta[n11] * delta_prop[n]) +
       vaccine60[13] * (1 - trans_alpha[n12] * (1 - delta_prop[n]) - trans_delta[n12] * delta_prop[n]) +
       vaccine60[14] * (1 - trans_alpha[n13] * (1 - delta_prop[n]) - trans_delta[n13] * delta_prop[n]) +
       vaccine60[15] * (1 - trans_alpha[n14] * (1 - delta_prop[n]) - trans_delta[n14] * delta_prop[n]) +
       vaccine60[16] * (1 - trans_alpha[n15] * (1 - delta_prop[n]) - trans_delta[n15] * delta_prop[n]) +
       vaccine60[17] * (1 - trans_alpha[n16] * (1 - delta_prop[n]) - trans_delta[n16] * delta_prop[n]) +
       vaccine60[18] * (1 - trans_alpha[n17] * (1 - delta_prop[n]) - trans_delta[n17] * delta_prop[n]) +
       vaccine60[19] * (1 - trans_alpha[n18] * (1 - delta_prop[n]) - trans_delta[n18] * delta_prop[n]) +
       vaccine60[20] * (1 - trans_alpha[n19] * (1 - delta_prop[n]) - trans_delta[n19] * delta_prop[n]) +
       vaccine60[21] * (1 - trans_alpha[n20] * (1 - delta_prop[n]) - trans_delta[n20] * delta_prop[n]) +
       vaccine60[22] * (1 - trans_alpha[n21] * (1 - delta_prop[n]) - trans_delta[n21] * delta_prop[n]) +
       vaccine60[23] * (1 - trans_alpha[n22] * (1 - delta_prop[n]) - trans_delta[n22] * delta_prop[n]) +
       vaccine60[24] * (1 - trans_alpha[n23] * (1 - delta_prop[n]) - trans_delta[n23] * delta_prop[n]) +
       vaccine60[25] * (1 - trans_alpha[n24] * (1 - delta_prop[n]) - trans_delta[n24] * delta_prop[n]) +
       vaccine60[26] * (1 - trans_alpha[n25] * (1 - delta_prop[n]) - trans_delta[n25] * delta_prop[n]) +
       vaccine60[27] * (1 - trans_alpha[n26] * (1 - delta_prop[n]) - trans_delta[n26] * delta_prop[n]) +
       vaccine60[28] * (1 - trans_alpha[n27] * (1 - delta_prop[n]) - trans_delta[n27] * delta_prop[n]) +
       vaccine60[29] * (1 - trans_alpha[n28] * (1 - delta_prop[n]) - trans_delta[n28] * delta_prop[n]) +
       vaccine60[30] * (1 - trans_alpha[n29] * (1 - delta_prop[n]) - trans_delta[n29] * delta_prop[n]) +
       vaccine60[31] * (1 - trans_alpha[n30] * (1 - delta_prop[n]) - trans_delta[n30] * delta_prop[n]) +
       vaccine60[32] * (1 - trans_alpha[n31] * (1 - delta_prop[n]) - trans_delta[n31] * delta_prop[n]) +
       vaccine60[33] * (1 - trans_alpha[n32] * (1 - delta_prop[n]) - trans_delta[n32] * delta_prop[n]) +
       vaccine60[34] * (1 - trans_alpha[n33] * (1 - delta_prop[n]) - trans_delta[n33] * delta_prop[n]) +
       vaccine60[35] * (1 - trans_alpha[n34] * (1 - delta_prop[n]) - trans_delta[n34] * delta_prop[n]) +
       vaccine60[36] * (1 - trans_alpha[n35] * (1 - delta_prop[n]) - trans_delta[n35] * delta_prop[n]) +
       vaccine60[37] * (1 - trans_alpha[n36] * (1 - delta_prop[n]) - trans_delta[n36] * delta_prop[n]) +
       vaccine60[38] * (1 - trans_alpha[n37] * (1 - delta_prop[n]) - trans_delta[n37] * delta_prop[n]) +
       vaccine60[39] * (1 - trans_alpha[n38] * (1 - delta_prop[n]) - trans_delta[n38] * delta_prop[n]) +
       vaccine60[40] * (1 - trans_alpha[n39] * (1 - delta_prop[n]) - trans_delta[n39] * delta_prop[n]) +
       vaccine60[41] * (1 - trans_alpha[n40] * (1 - delta_prop[n]) - trans_delta[n40] * delta_prop[n]) +
       vaccine60[42] * (1 - trans_alpha[n41] * (1 - delta_prop[n]) - trans_delta[n41] * delta_prop[n]) +
       vaccine60[43] * (1 - trans_alpha[n42] * (1 - delta_prop[n]) - trans_delta[n42] * delta_prop[n]) +
       vaccine60[44] * (1 - trans_alpha[n43] * (1 - delta_prop[n]) - trans_delta[n43] * delta_prop[n]) +
       vaccine60[45] * (1 - trans_alpha[n44] * (1 - delta_prop[n]) - trans_delta[n44] * delta_prop[n]) +
       vaccine60[46] * (1 - trans_alpha[n45] * (1 - delta_prop[n]) - trans_delta[n45] * delta_prop[n]) +
       vaccine60[47] * (1 - trans_alpha[n46] * (1 - delta_prop[n]) - trans_delta[n46] * delta_prop[n]) +
       vaccine60[48] * (1 - trans_alpha[n47] * (1 - delta_prop[n]) - trans_delta[n47] * delta_prop[n]) +
       vaccine60[49] * (1 - trans_alpha[n48] * (1 - delta_prop[n]) - trans_delta[n48] * delta_prop[n]) +
       vaccine60[50] * (1 - trans_alpha[n49] * (1 - delta_prop[n]) - trans_delta[n49] * delta_prop[n]) +
       vaccine60[51] * (1 - trans_alpha[n50] * (1 - delta_prop[n]) - trans_delta[n50] * delta_prop[n]) +
       vaccine60[52] * (1 - trans_alpha[n51] * (1 - delta_prop[n]) - trans_delta[n51] * delta_prop[n]) +
       vaccine60[53] * (1 - trans_alpha[n52] * (1 - delta_prop[n]) - trans_delta[n52] * delta_prop[n]) +
       vaccine60[54] * (1 - trans_alpha[n53] * (1 - delta_prop[n]) - trans_delta[n53] * delta_prop[n]) +
       vaccine60[55] * (1 - trans_alpha[n54] * (1 - delta_prop[n]) - trans_delta[n54] * delta_prop[n]) +
       vaccine60[56] * (1 - trans_alpha[n55] * (1 - delta_prop[n]) - trans_delta[n55] * delta_prop[n]) +
       vaccine60[57] * (1 - trans_alpha[n56] * (1 - delta_prop[n]) - trans_delta[n56] * delta_prop[n]) +
       vaccine60[58] * (1 - trans_alpha[n57] * (1 - delta_prop[n]) - trans_delta[n57] * delta_prop[n]) +
       vaccine60[59] * (1 - trans_alpha[n58] * (1 - delta_prop[n]) - trans_delta[n58] * delta_prop[n]) +
       vaccine60[60] * (1 - trans_alpha[n59] * (1 - delta_prop[n]) - trans_delta[n59] * delta_prop[n]) +
       vaccine60[61] * (1 - trans_alpha[n60] * (1 - delta_prop[n]) - trans_delta[n60] * delta_prop[n]) +
       vaccine60[62] * (1 - trans_alpha[n61] * (1 - delta_prop[n]) - trans_delta[n61] * delta_prop[n]) +
       vaccine60[63] * (1 - trans_alpha[n62] * (1 - delta_prop[n]) - trans_delta[n62] * delta_prop[n]) +
       vaccine60[64] * (1 - trans_alpha[n63] * (1 - delta_prop[n]) - trans_delta[n63] * delta_prop[n]) +
       vaccine60[65] * (1 - trans_alpha[n64] * (1 - delta_prop[n]) - trans_delta[n64] * delta_prop[n]) +
       vaccine60[66] * (1 - trans_alpha[n65] * (1 - delta_prop[n]) - trans_delta[n65] * delta_prop[n]) +
       vaccine60[67] * (1 - trans_alpha[n66] * (1 - delta_prop[n]) - trans_delta[n66] * delta_prop[n]) +
       vaccine60[68] * (1 - trans_alpha[n67] * (1 - delta_prop[n]) - trans_delta[n67] * delta_prop[n]) +
       vaccine60[69] * (1 - trans_alpha[n68] * (1 - delta_prop[n]) - trans_delta[n68] * delta_prop[n]) +
       vaccine60[70] * (1 - trans_alpha[n69] * (1 - delta_prop[n]) - trans_delta[n69] * delta_prop[n]) +
       vaccine60[71] * (1 - trans_alpha[n70] * (1 - delta_prop[n]) - trans_delta[n70] * delta_prop[n]) +
       vaccine60[72] * (1 - trans_alpha[n71] * (1 - delta_prop[n]) - trans_delta[n71] * delta_prop[n]) +
       vaccine60[73] * (1 - trans_alpha[n72] * (1 - delta_prop[n]) - trans_delta[n72] * delta_prop[n]) +
       vaccine60[74] * (1 - trans_alpha[n73] * (1 - delta_prop[n]) - trans_delta[n73] * delta_prop[n]) +
       vaccine60[75] * (1 - trans_alpha[n74] * (1 - delta_prop[n]) - trans_delta[n74] * delta_prop[n]) +
       vaccine60[76] * (1 - trans_alpha[n75] * (1 - delta_prop[n]) - trans_delta[n75] * delta_prop[n]) +
       vaccine60[77] * (1 - trans_alpha[n76] * (1 - delta_prop[n]) - trans_delta[n76] * delta_prop[n]) +
       vaccine60[78] * (1 - trans_alpha[n77] * (1 - delta_prop[n]) - trans_delta[n77] * delta_prop[n]) +
       vaccine60[79] * (1 - trans_alpha[n78] * (1 - delta_prop[n]) - trans_delta[n78] * delta_prop[n]) +
       vaccine60[80] * (1 - trans_alpha[n79] * (1 - delta_prop[n]) - trans_delta[n79] * delta_prop[n]) +
       vaccine60[81] * (1 - trans_alpha[n80] * (1 - delta_prop[n]) - trans_delta[n80] * delta_prop[n]) +
       vaccine60[82] * (1 - trans_alpha[n81] * (1 - delta_prop[n]) - trans_delta[n81] * delta_prop[n]) +
       vaccine60[83] * (1 - trans_alpha[n82] * (1 - delta_prop[n]) - trans_delta[n82] * delta_prop[n]) +
       vaccine60[84] * (1 - trans_alpha[n83] * (1 - delta_prop[n]) - trans_delta[n83] * delta_prop[n]) +
       vaccine60[85] * (1 - trans_alpha[n84] * (1 - delta_prop[n]) - trans_delta[n84] * delta_prop[n]) +
       vaccine60[86] * (1 - trans_alpha[n85] * (1 - delta_prop[n]) - trans_delta[n85] * delta_prop[n]) +
       vaccine60[87] * (1 - trans_alpha[n86] * (1 - delta_prop[n]) - trans_delta[n86] * delta_prop[n]) +
       vaccine60[88] * (1 - trans_alpha[n87] * (1 - delta_prop[n]) - trans_delta[n87] * delta_prop[n]) +
       vaccine60[89] * (1 - trans_alpha[n88] * (1 - delta_prop[n]) - trans_delta[n88] * delta_prop[n]) +
       vaccine60[90] * (1 - trans_alpha[n89] * (1 - delta_prop[n]) - trans_delta[n89] * delta_prop[n]) +
       vaccine60[91] * (1 - trans_alpha[n90] * (1 - delta_prop[n]) - trans_delta[n90] * delta_prop[n]) +
       vaccine60[92] * (1 - trans_alpha[n91] * (1 - delta_prop[n]) - trans_delta[n91] * delta_prop[n]) +
       vaccine60[93] * (1 - trans_alpha[n92] * (1 - delta_prop[n]) - trans_delta[n92] * delta_prop[n]) +
       vaccine60[94] * (1 - trans_alpha[n93] * (1 - delta_prop[n]) - trans_delta[n93] * delta_prop[n]) +
       vaccine60[95] * (1 - trans_alpha[n94] * (1 - delta_prop[n]) - trans_delta[n94] * delta_prop[n]) +
       vaccine60[96] * (1 - trans_alpha[n95] * (1 - delta_prop[n]) - trans_delta[n95] * delta_prop[n]) +
       vaccine60[97] * (1 - trans_alpha[n96] * (1 - delta_prop[n]) - trans_delta[n96] * delta_prop[n]) +
       vaccine60[98] * (1 - trans_alpha[n97] * (1 - delta_prop[n]) - trans_delta[n97] * delta_prop[n]) +
       vaccine60[99] * (1 - trans_alpha[n98] * (1 - delta_prop[n]) - trans_delta[n98] * delta_prop[n]) +
       ifelse((1 - cum_vac60[n] - ((sum(new_case60[1:9]) + sum(pred60_sofar[10:n])) / population60)) < 0,
              0,
              1 - cum_vac60[n] - ((sum(new_case60[1:9]) + sum(pred60_sofar[10:n])) / population60)))
  
  poi70 <- pred70_sofar[n] *
    (vaccine70[1] *  (1 - trans_alpha[n00] * (1 - delta_prop[n]) - trans_delta[n00] * delta_prop[n]) +
       vaccine70[2] *  (1 - trans_alpha[n01] * (1 - delta_prop[n]) - trans_delta[n01] * delta_prop[n]) +
       vaccine70[3] *  (1 - trans_alpha[n02] * (1 - delta_prop[n]) - trans_delta[n02] * delta_prop[n]) +
       vaccine70[4] *  (1 - trans_alpha[n03] * (1 - delta_prop[n]) - trans_delta[n03] * delta_prop[n]) +
       vaccine70[5] *  (1 - trans_alpha[n04] * (1 - delta_prop[n]) - trans_delta[n04] * delta_prop[n]) +
       vaccine70[6] *  (1 - trans_alpha[n05] * (1 - delta_prop[n]) - trans_delta[n05] * delta_prop[n]) +
       vaccine70[7] *  (1 - trans_alpha[n06] * (1 - delta_prop[n]) - trans_delta[n06] * delta_prop[n]) +
       vaccine70[8] *  (1 - trans_alpha[n07] * (1 - delta_prop[n]) - trans_delta[n07] * delta_prop[n]) +
       vaccine70[9] *  (1 - trans_alpha[n08] * (1 - delta_prop[n]) - trans_delta[n08] * delta_prop[n]) +
       vaccine70[10] * (1 - trans_alpha[n09] * (1 - delta_prop[n]) - trans_delta[n09] * delta_prop[n]) +
       vaccine70[11] * (1 - trans_alpha[n10] * (1 - delta_prop[n]) - trans_delta[n10] * delta_prop[n]) +
       vaccine70[12] * (1 - trans_alpha[n11] * (1 - delta_prop[n]) - trans_delta[n11] * delta_prop[n]) +
       vaccine70[13] * (1 - trans_alpha[n12] * (1 - delta_prop[n]) - trans_delta[n12] * delta_prop[n]) +
       vaccine70[14] * (1 - trans_alpha[n13] * (1 - delta_prop[n]) - trans_delta[n13] * delta_prop[n]) +
       vaccine70[15] * (1 - trans_alpha[n14] * (1 - delta_prop[n]) - trans_delta[n14] * delta_prop[n]) +
       vaccine70[16] * (1 - trans_alpha[n15] * (1 - delta_prop[n]) - trans_delta[n15] * delta_prop[n]) +
       vaccine70[17] * (1 - trans_alpha[n16] * (1 - delta_prop[n]) - trans_delta[n16] * delta_prop[n]) +
       vaccine70[18] * (1 - trans_alpha[n17] * (1 - delta_prop[n]) - trans_delta[n17] * delta_prop[n]) +
       vaccine70[19] * (1 - trans_alpha[n18] * (1 - delta_prop[n]) - trans_delta[n18] * delta_prop[n]) +
       vaccine70[20] * (1 - trans_alpha[n19] * (1 - delta_prop[n]) - trans_delta[n19] * delta_prop[n]) +
       vaccine70[21] * (1 - trans_alpha[n20] * (1 - delta_prop[n]) - trans_delta[n20] * delta_prop[n]) +
       vaccine70[22] * (1 - trans_alpha[n21] * (1 - delta_prop[n]) - trans_delta[n21] * delta_prop[n]) +
       vaccine70[23] * (1 - trans_alpha[n22] * (1 - delta_prop[n]) - trans_delta[n22] * delta_prop[n]) +
       vaccine70[24] * (1 - trans_alpha[n23] * (1 - delta_prop[n]) - trans_delta[n23] * delta_prop[n]) +
       vaccine70[25] * (1 - trans_alpha[n24] * (1 - delta_prop[n]) - trans_delta[n24] * delta_prop[n]) +
       vaccine70[26] * (1 - trans_alpha[n25] * (1 - delta_prop[n]) - trans_delta[n25] * delta_prop[n]) +
       vaccine70[27] * (1 - trans_alpha[n26] * (1 - delta_prop[n]) - trans_delta[n26] * delta_prop[n]) +
       vaccine70[28] * (1 - trans_alpha[n27] * (1 - delta_prop[n]) - trans_delta[n27] * delta_prop[n]) +
       vaccine70[29] * (1 - trans_alpha[n28] * (1 - delta_prop[n]) - trans_delta[n28] * delta_prop[n]) +
       vaccine70[30] * (1 - trans_alpha[n29] * (1 - delta_prop[n]) - trans_delta[n29] * delta_prop[n]) +
       vaccine70[31] * (1 - trans_alpha[n30] * (1 - delta_prop[n]) - trans_delta[n30] * delta_prop[n]) +
       vaccine70[32] * (1 - trans_alpha[n31] * (1 - delta_prop[n]) - trans_delta[n31] * delta_prop[n]) +
       vaccine70[33] * (1 - trans_alpha[n32] * (1 - delta_prop[n]) - trans_delta[n32] * delta_prop[n]) +
       vaccine70[34] * (1 - trans_alpha[n33] * (1 - delta_prop[n]) - trans_delta[n33] * delta_prop[n]) +
       vaccine70[35] * (1 - trans_alpha[n34] * (1 - delta_prop[n]) - trans_delta[n34] * delta_prop[n]) +
       vaccine70[36] * (1 - trans_alpha[n35] * (1 - delta_prop[n]) - trans_delta[n35] * delta_prop[n]) +
       vaccine70[37] * (1 - trans_alpha[n36] * (1 - delta_prop[n]) - trans_delta[n36] * delta_prop[n]) +
       vaccine70[38] * (1 - trans_alpha[n37] * (1 - delta_prop[n]) - trans_delta[n37] * delta_prop[n]) +
       vaccine70[39] * (1 - trans_alpha[n38] * (1 - delta_prop[n]) - trans_delta[n38] * delta_prop[n]) +
       vaccine70[40] * (1 - trans_alpha[n39] * (1 - delta_prop[n]) - trans_delta[n39] * delta_prop[n]) +
       vaccine70[41] * (1 - trans_alpha[n40] * (1 - delta_prop[n]) - trans_delta[n40] * delta_prop[n]) +
       vaccine70[42] * (1 - trans_alpha[n41] * (1 - delta_prop[n]) - trans_delta[n41] * delta_prop[n]) +
       vaccine70[43] * (1 - trans_alpha[n42] * (1 - delta_prop[n]) - trans_delta[n42] * delta_prop[n]) +
       vaccine70[44] * (1 - trans_alpha[n43] * (1 - delta_prop[n]) - trans_delta[n43] * delta_prop[n]) +
       vaccine70[45] * (1 - trans_alpha[n44] * (1 - delta_prop[n]) - trans_delta[n44] * delta_prop[n]) +
       vaccine70[46] * (1 - trans_alpha[n45] * (1 - delta_prop[n]) - trans_delta[n45] * delta_prop[n]) +
       vaccine70[47] * (1 - trans_alpha[n46] * (1 - delta_prop[n]) - trans_delta[n46] * delta_prop[n]) +
       vaccine70[48] * (1 - trans_alpha[n47] * (1 - delta_prop[n]) - trans_delta[n47] * delta_prop[n]) +
       vaccine70[49] * (1 - trans_alpha[n48] * (1 - delta_prop[n]) - trans_delta[n48] * delta_prop[n]) +
       vaccine70[50] * (1 - trans_alpha[n49] * (1 - delta_prop[n]) - trans_delta[n49] * delta_prop[n]) +
       vaccine70[51] * (1 - trans_alpha[n50] * (1 - delta_prop[n]) - trans_delta[n50] * delta_prop[n]) +
       vaccine70[52] * (1 - trans_alpha[n51] * (1 - delta_prop[n]) - trans_delta[n51] * delta_prop[n]) +
       vaccine70[53] * (1 - trans_alpha[n52] * (1 - delta_prop[n]) - trans_delta[n52] * delta_prop[n]) +
       vaccine70[54] * (1 - trans_alpha[n53] * (1 - delta_prop[n]) - trans_delta[n53] * delta_prop[n]) +
       vaccine70[55] * (1 - trans_alpha[n54] * (1 - delta_prop[n]) - trans_delta[n54] * delta_prop[n]) +
       vaccine70[56] * (1 - trans_alpha[n55] * (1 - delta_prop[n]) - trans_delta[n55] * delta_prop[n]) +
       vaccine70[57] * (1 - trans_alpha[n56] * (1 - delta_prop[n]) - trans_delta[n56] * delta_prop[n]) +
       vaccine70[58] * (1 - trans_alpha[n57] * (1 - delta_prop[n]) - trans_delta[n57] * delta_prop[n]) +
       vaccine70[59] * (1 - trans_alpha[n58] * (1 - delta_prop[n]) - trans_delta[n58] * delta_prop[n]) +
       vaccine70[60] * (1 - trans_alpha[n59] * (1 - delta_prop[n]) - trans_delta[n59] * delta_prop[n]) +
       vaccine70[61] * (1 - trans_alpha[n60] * (1 - delta_prop[n]) - trans_delta[n60] * delta_prop[n]) +
       vaccine70[62] * (1 - trans_alpha[n61] * (1 - delta_prop[n]) - trans_delta[n61] * delta_prop[n]) +
       vaccine70[63] * (1 - trans_alpha[n62] * (1 - delta_prop[n]) - trans_delta[n62] * delta_prop[n]) +
       vaccine70[64] * (1 - trans_alpha[n63] * (1 - delta_prop[n]) - trans_delta[n63] * delta_prop[n]) +
       vaccine70[65] * (1 - trans_alpha[n64] * (1 - delta_prop[n]) - trans_delta[n64] * delta_prop[n]) +
       vaccine70[66] * (1 - trans_alpha[n65] * (1 - delta_prop[n]) - trans_delta[n65] * delta_prop[n]) +
       vaccine70[67] * (1 - trans_alpha[n66] * (1 - delta_prop[n]) - trans_delta[n66] * delta_prop[n]) +
       vaccine70[68] * (1 - trans_alpha[n67] * (1 - delta_prop[n]) - trans_delta[n67] * delta_prop[n]) +
       vaccine70[69] * (1 - trans_alpha[n68] * (1 - delta_prop[n]) - trans_delta[n68] * delta_prop[n]) +
       vaccine70[70] * (1 - trans_alpha[n69] * (1 - delta_prop[n]) - trans_delta[n69] * delta_prop[n]) +
       vaccine70[71] * (1 - trans_alpha[n70] * (1 - delta_prop[n]) - trans_delta[n70] * delta_prop[n]) +
       vaccine70[72] * (1 - trans_alpha[n71] * (1 - delta_prop[n]) - trans_delta[n71] * delta_prop[n]) +
       vaccine70[73] * (1 - trans_alpha[n72] * (1 - delta_prop[n]) - trans_delta[n72] * delta_prop[n]) +
       vaccine70[74] * (1 - trans_alpha[n73] * (1 - delta_prop[n]) - trans_delta[n73] * delta_prop[n]) +
       vaccine70[75] * (1 - trans_alpha[n74] * (1 - delta_prop[n]) - trans_delta[n74] * delta_prop[n]) +
       vaccine70[76] * (1 - trans_alpha[n75] * (1 - delta_prop[n]) - trans_delta[n75] * delta_prop[n]) +
       vaccine70[77] * (1 - trans_alpha[n76] * (1 - delta_prop[n]) - trans_delta[n76] * delta_prop[n]) +
       vaccine70[78] * (1 - trans_alpha[n77] * (1 - delta_prop[n]) - trans_delta[n77] * delta_prop[n]) +
       vaccine70[79] * (1 - trans_alpha[n78] * (1 - delta_prop[n]) - trans_delta[n78] * delta_prop[n]) +
       vaccine70[80] * (1 - trans_alpha[n79] * (1 - delta_prop[n]) - trans_delta[n79] * delta_prop[n]) +
       vaccine70[81] * (1 - trans_alpha[n80] * (1 - delta_prop[n]) - trans_delta[n80] * delta_prop[n]) +
       vaccine70[82] * (1 - trans_alpha[n81] * (1 - delta_prop[n]) - trans_delta[n81] * delta_prop[n]) +
       vaccine70[83] * (1 - trans_alpha[n82] * (1 - delta_prop[n]) - trans_delta[n82] * delta_prop[n]) +
       vaccine70[84] * (1 - trans_alpha[n83] * (1 - delta_prop[n]) - trans_delta[n83] * delta_prop[n]) +
       vaccine70[85] * (1 - trans_alpha[n84] * (1 - delta_prop[n]) - trans_delta[n84] * delta_prop[n]) +
       vaccine70[86] * (1 - trans_alpha[n85] * (1 - delta_prop[n]) - trans_delta[n85] * delta_prop[n]) +
       vaccine70[87] * (1 - trans_alpha[n86] * (1 - delta_prop[n]) - trans_delta[n86] * delta_prop[n]) +
       vaccine70[88] * (1 - trans_alpha[n87] * (1 - delta_prop[n]) - trans_delta[n87] * delta_prop[n]) +
       vaccine70[89] * (1 - trans_alpha[n88] * (1 - delta_prop[n]) - trans_delta[n88] * delta_prop[n]) +
       vaccine70[90] * (1 - trans_alpha[n89] * (1 - delta_prop[n]) - trans_delta[n89] * delta_prop[n]) +
       vaccine70[91] * (1 - trans_alpha[n90] * (1 - delta_prop[n]) - trans_delta[n90] * delta_prop[n]) +
       vaccine70[92] * (1 - trans_alpha[n91] * (1 - delta_prop[n]) - trans_delta[n91] * delta_prop[n]) +
       vaccine70[93] * (1 - trans_alpha[n92] * (1 - delta_prop[n]) - trans_delta[n92] * delta_prop[n]) +
       vaccine70[94] * (1 - trans_alpha[n93] * (1 - delta_prop[n]) - trans_delta[n93] * delta_prop[n]) +
       vaccine70[95] * (1 - trans_alpha[n94] * (1 - delta_prop[n]) - trans_delta[n94] * delta_prop[n]) +
       vaccine70[96] * (1 - trans_alpha[n95] * (1 - delta_prop[n]) - trans_delta[n95] * delta_prop[n]) +
       vaccine70[97] * (1 - trans_alpha[n96] * (1 - delta_prop[n]) - trans_delta[n96] * delta_prop[n]) +
       vaccine70[98] * (1 - trans_alpha[n97] * (1 - delta_prop[n]) - trans_delta[n97] * delta_prop[n]) +
       vaccine70[99] * (1 - trans_alpha[n98] * (1 - delta_prop[n]) - trans_delta[n98] * delta_prop[n]) +
       ifelse((1 - cum_vac70[n] - ((sum(new_case70[1:9]) + sum(pred70_sofar[10:n])) / population70)) < 0,
              0,
              1 - cum_vac70[n] - ((sum(new_case70[1:9]) + sum(pred70_sofar[10:n])) / population70)))
  
  poi80 <- pred80_sofar[n] *
    (vaccine80[1] *  (1 - trans_alpha[n00] * (1 - delta_prop[n]) - trans_delta[n00] * delta_prop[n]) +
       vaccine80[2] *  (1 - trans_alpha[n01] * (1 - delta_prop[n]) - trans_delta[n01] * delta_prop[n]) +
       vaccine80[3] *  (1 - trans_alpha[n02] * (1 - delta_prop[n]) - trans_delta[n02] * delta_prop[n]) +
       vaccine80[4] *  (1 - trans_alpha[n03] * (1 - delta_prop[n]) - trans_delta[n03] * delta_prop[n]) +
       vaccine80[5] *  (1 - trans_alpha[n04] * (1 - delta_prop[n]) - trans_delta[n04] * delta_prop[n]) +
       vaccine80[6] *  (1 - trans_alpha[n05] * (1 - delta_prop[n]) - trans_delta[n05] * delta_prop[n]) +
       vaccine80[7] *  (1 - trans_alpha[n06] * (1 - delta_prop[n]) - trans_delta[n06] * delta_prop[n]) +
       vaccine80[8] *  (1 - trans_alpha[n07] * (1 - delta_prop[n]) - trans_delta[n07] * delta_prop[n]) +
       vaccine80[9] *  (1 - trans_alpha[n08] * (1 - delta_prop[n]) - trans_delta[n08] * delta_prop[n]) +
       vaccine80[10] * (1 - trans_alpha[n09] * (1 - delta_prop[n]) - trans_delta[n09] * delta_prop[n]) +
       vaccine80[11] * (1 - trans_alpha[n10] * (1 - delta_prop[n]) - trans_delta[n10] * delta_prop[n]) +
       vaccine80[12] * (1 - trans_alpha[n11] * (1 - delta_prop[n]) - trans_delta[n11] * delta_prop[n]) +
       vaccine80[13] * (1 - trans_alpha[n12] * (1 - delta_prop[n]) - trans_delta[n12] * delta_prop[n]) +
       vaccine80[14] * (1 - trans_alpha[n13] * (1 - delta_prop[n]) - trans_delta[n13] * delta_prop[n]) +
       vaccine80[15] * (1 - trans_alpha[n14] * (1 - delta_prop[n]) - trans_delta[n14] * delta_prop[n]) +
       vaccine80[16] * (1 - trans_alpha[n15] * (1 - delta_prop[n]) - trans_delta[n15] * delta_prop[n]) +
       vaccine80[17] * (1 - trans_alpha[n16] * (1 - delta_prop[n]) - trans_delta[n16] * delta_prop[n]) +
       vaccine80[18] * (1 - trans_alpha[n17] * (1 - delta_prop[n]) - trans_delta[n17] * delta_prop[n]) +
       vaccine80[19] * (1 - trans_alpha[n18] * (1 - delta_prop[n]) - trans_delta[n18] * delta_prop[n]) +
       vaccine80[20] * (1 - trans_alpha[n19] * (1 - delta_prop[n]) - trans_delta[n19] * delta_prop[n]) +
       vaccine80[21] * (1 - trans_alpha[n20] * (1 - delta_prop[n]) - trans_delta[n20] * delta_prop[n]) +
       vaccine80[22] * (1 - trans_alpha[n21] * (1 - delta_prop[n]) - trans_delta[n21] * delta_prop[n]) +
       vaccine80[23] * (1 - trans_alpha[n22] * (1 - delta_prop[n]) - trans_delta[n22] * delta_prop[n]) +
       vaccine80[24] * (1 - trans_alpha[n23] * (1 - delta_prop[n]) - trans_delta[n23] * delta_prop[n]) +
       vaccine80[25] * (1 - trans_alpha[n24] * (1 - delta_prop[n]) - trans_delta[n24] * delta_prop[n]) +
       vaccine80[26] * (1 - trans_alpha[n25] * (1 - delta_prop[n]) - trans_delta[n25] * delta_prop[n]) +
       vaccine80[27] * (1 - trans_alpha[n26] * (1 - delta_prop[n]) - trans_delta[n26] * delta_prop[n]) +
       vaccine80[28] * (1 - trans_alpha[n27] * (1 - delta_prop[n]) - trans_delta[n27] * delta_prop[n]) +
       vaccine80[29] * (1 - trans_alpha[n28] * (1 - delta_prop[n]) - trans_delta[n28] * delta_prop[n]) +
       vaccine80[30] * (1 - trans_alpha[n29] * (1 - delta_prop[n]) - trans_delta[n29] * delta_prop[n]) +
       vaccine80[31] * (1 - trans_alpha[n30] * (1 - delta_prop[n]) - trans_delta[n30] * delta_prop[n]) +
       vaccine80[32] * (1 - trans_alpha[n31] * (1 - delta_prop[n]) - trans_delta[n31] * delta_prop[n]) +
       vaccine80[33] * (1 - trans_alpha[n32] * (1 - delta_prop[n]) - trans_delta[n32] * delta_prop[n]) +
       vaccine80[34] * (1 - trans_alpha[n33] * (1 - delta_prop[n]) - trans_delta[n33] * delta_prop[n]) +
       vaccine80[35] * (1 - trans_alpha[n34] * (1 - delta_prop[n]) - trans_delta[n34] * delta_prop[n]) +
       vaccine80[36] * (1 - trans_alpha[n35] * (1 - delta_prop[n]) - trans_delta[n35] * delta_prop[n]) +
       vaccine80[37] * (1 - trans_alpha[n36] * (1 - delta_prop[n]) - trans_delta[n36] * delta_prop[n]) +
       vaccine80[38] * (1 - trans_alpha[n37] * (1 - delta_prop[n]) - trans_delta[n37] * delta_prop[n]) +
       vaccine80[39] * (1 - trans_alpha[n38] * (1 - delta_prop[n]) - trans_delta[n38] * delta_prop[n]) +
       vaccine80[40] * (1 - trans_alpha[n39] * (1 - delta_prop[n]) - trans_delta[n39] * delta_prop[n]) +
       vaccine80[41] * (1 - trans_alpha[n40] * (1 - delta_prop[n]) - trans_delta[n40] * delta_prop[n]) +
       vaccine80[42] * (1 - trans_alpha[n41] * (1 - delta_prop[n]) - trans_delta[n41] * delta_prop[n]) +
       vaccine80[43] * (1 - trans_alpha[n42] * (1 - delta_prop[n]) - trans_delta[n42] * delta_prop[n]) +
       vaccine80[44] * (1 - trans_alpha[n43] * (1 - delta_prop[n]) - trans_delta[n43] * delta_prop[n]) +
       vaccine80[45] * (1 - trans_alpha[n44] * (1 - delta_prop[n]) - trans_delta[n44] * delta_prop[n]) +
       vaccine80[46] * (1 - trans_alpha[n45] * (1 - delta_prop[n]) - trans_delta[n45] * delta_prop[n]) +
       vaccine80[47] * (1 - trans_alpha[n46] * (1 - delta_prop[n]) - trans_delta[n46] * delta_prop[n]) +
       vaccine80[48] * (1 - trans_alpha[n47] * (1 - delta_prop[n]) - trans_delta[n47] * delta_prop[n]) +
       vaccine80[49] * (1 - trans_alpha[n48] * (1 - delta_prop[n]) - trans_delta[n48] * delta_prop[n]) +
       vaccine80[50] * (1 - trans_alpha[n49] * (1 - delta_prop[n]) - trans_delta[n49] * delta_prop[n]) +
       vaccine80[51] * (1 - trans_alpha[n50] * (1 - delta_prop[n]) - trans_delta[n50] * delta_prop[n]) +
       vaccine80[52] * (1 - trans_alpha[n51] * (1 - delta_prop[n]) - trans_delta[n51] * delta_prop[n]) +
       vaccine80[53] * (1 - trans_alpha[n52] * (1 - delta_prop[n]) - trans_delta[n52] * delta_prop[n]) +
       vaccine80[54] * (1 - trans_alpha[n53] * (1 - delta_prop[n]) - trans_delta[n53] * delta_prop[n]) +
       vaccine80[55] * (1 - trans_alpha[n54] * (1 - delta_prop[n]) - trans_delta[n54] * delta_prop[n]) +
       vaccine80[56] * (1 - trans_alpha[n55] * (1 - delta_prop[n]) - trans_delta[n55] * delta_prop[n]) +
       vaccine80[57] * (1 - trans_alpha[n56] * (1 - delta_prop[n]) - trans_delta[n56] * delta_prop[n]) +
       vaccine80[58] * (1 - trans_alpha[n57] * (1 - delta_prop[n]) - trans_delta[n57] * delta_prop[n]) +
       vaccine80[59] * (1 - trans_alpha[n58] * (1 - delta_prop[n]) - trans_delta[n58] * delta_prop[n]) +
       vaccine80[60] * (1 - trans_alpha[n59] * (1 - delta_prop[n]) - trans_delta[n59] * delta_prop[n]) +
       vaccine80[61] * (1 - trans_alpha[n60] * (1 - delta_prop[n]) - trans_delta[n60] * delta_prop[n]) +
       vaccine80[62] * (1 - trans_alpha[n61] * (1 - delta_prop[n]) - trans_delta[n61] * delta_prop[n]) +
       vaccine80[63] * (1 - trans_alpha[n62] * (1 - delta_prop[n]) - trans_delta[n62] * delta_prop[n]) +
       vaccine80[64] * (1 - trans_alpha[n63] * (1 - delta_prop[n]) - trans_delta[n63] * delta_prop[n]) +
       vaccine80[65] * (1 - trans_alpha[n64] * (1 - delta_prop[n]) - trans_delta[n64] * delta_prop[n]) +
       vaccine80[66] * (1 - trans_alpha[n65] * (1 - delta_prop[n]) - trans_delta[n65] * delta_prop[n]) +
       vaccine80[67] * (1 - trans_alpha[n66] * (1 - delta_prop[n]) - trans_delta[n66] * delta_prop[n]) +
       vaccine80[68] * (1 - trans_alpha[n67] * (1 - delta_prop[n]) - trans_delta[n67] * delta_prop[n]) +
       vaccine80[69] * (1 - trans_alpha[n68] * (1 - delta_prop[n]) - trans_delta[n68] * delta_prop[n]) +
       vaccine80[70] * (1 - trans_alpha[n69] * (1 - delta_prop[n]) - trans_delta[n69] * delta_prop[n]) +
       vaccine80[71] * (1 - trans_alpha[n70] * (1 - delta_prop[n]) - trans_delta[n70] * delta_prop[n]) +
       vaccine80[72] * (1 - trans_alpha[n71] * (1 - delta_prop[n]) - trans_delta[n71] * delta_prop[n]) +
       vaccine80[73] * (1 - trans_alpha[n72] * (1 - delta_prop[n]) - trans_delta[n72] * delta_prop[n]) +
       vaccine80[74] * (1 - trans_alpha[n73] * (1 - delta_prop[n]) - trans_delta[n73] * delta_prop[n]) +
       vaccine80[75] * (1 - trans_alpha[n74] * (1 - delta_prop[n]) - trans_delta[n74] * delta_prop[n]) +
       vaccine80[76] * (1 - trans_alpha[n75] * (1 - delta_prop[n]) - trans_delta[n75] * delta_prop[n]) +
       vaccine80[77] * (1 - trans_alpha[n76] * (1 - delta_prop[n]) - trans_delta[n76] * delta_prop[n]) +
       vaccine80[78] * (1 - trans_alpha[n77] * (1 - delta_prop[n]) - trans_delta[n77] * delta_prop[n]) +
       vaccine80[79] * (1 - trans_alpha[n78] * (1 - delta_prop[n]) - trans_delta[n78] * delta_prop[n]) +
       vaccine80[80] * (1 - trans_alpha[n79] * (1 - delta_prop[n]) - trans_delta[n79] * delta_prop[n]) +
       vaccine80[81] * (1 - trans_alpha[n80] * (1 - delta_prop[n]) - trans_delta[n80] * delta_prop[n]) +
       vaccine80[82] * (1 - trans_alpha[n81] * (1 - delta_prop[n]) - trans_delta[n81] * delta_prop[n]) +
       vaccine80[83] * (1 - trans_alpha[n82] * (1 - delta_prop[n]) - trans_delta[n82] * delta_prop[n]) +
       vaccine80[84] * (1 - trans_alpha[n83] * (1 - delta_prop[n]) - trans_delta[n83] * delta_prop[n]) +
       vaccine80[85] * (1 - trans_alpha[n84] * (1 - delta_prop[n]) - trans_delta[n84] * delta_prop[n]) +
       vaccine80[86] * (1 - trans_alpha[n85] * (1 - delta_prop[n]) - trans_delta[n85] * delta_prop[n]) +
       vaccine80[87] * (1 - trans_alpha[n86] * (1 - delta_prop[n]) - trans_delta[n86] * delta_prop[n]) +
       vaccine80[88] * (1 - trans_alpha[n87] * (1 - delta_prop[n]) - trans_delta[n87] * delta_prop[n]) +
       vaccine80[89] * (1 - trans_alpha[n88] * (1 - delta_prop[n]) - trans_delta[n88] * delta_prop[n]) +
       vaccine80[90] * (1 - trans_alpha[n89] * (1 - delta_prop[n]) - trans_delta[n89] * delta_prop[n]) +
       vaccine80[91] * (1 - trans_alpha[n90] * (1 - delta_prop[n]) - trans_delta[n90] * delta_prop[n]) +
       vaccine80[92] * (1 - trans_alpha[n91] * (1 - delta_prop[n]) - trans_delta[n91] * delta_prop[n]) +
       vaccine80[93] * (1 - trans_alpha[n92] * (1 - delta_prop[n]) - trans_delta[n92] * delta_prop[n]) +
       vaccine80[94] * (1 - trans_alpha[n93] * (1 - delta_prop[n]) - trans_delta[n93] * delta_prop[n]) +
       vaccine80[95] * (1 - trans_alpha[n94] * (1 - delta_prop[n]) - trans_delta[n94] * delta_prop[n]) +
       vaccine80[96] * (1 - trans_alpha[n95] * (1 - delta_prop[n]) - trans_delta[n95] * delta_prop[n]) +
       vaccine80[97] * (1 - trans_alpha[n96] * (1 - delta_prop[n]) - trans_delta[n96] * delta_prop[n]) +
       vaccine80[98] * (1 - trans_alpha[n97] * (1 - delta_prop[n]) - trans_delta[n97] * delta_prop[n]) +
       vaccine80[99] * (1 - trans_alpha[n98] * (1 - delta_prop[n]) - trans_delta[n98] * delta_prop[n]) +
       ifelse((1 - cum_vac80[n] - ((sum(new_case80[1:9]) + sum(pred80_sofar[10:n])) / population80)) < 0,
              0,
              1 - cum_vac80[n] - ((sum(new_case80[1:9]) + sum(pred80_sofar[10:n])) / population80)))
  
  
  poi <- (poi00 + poi10) +
    ((poi20 + poi30) * (((4+4+16+4)/(1+1+1+4)) ^ (7/5))) +
    ((poi40 + poi50) * (((4+4+16+4)/(1+1+1+4)) ^ (7/5))) +
    (poi60 + poi70 + poi80)       
  
  pred0080 <- (rt ^ (7/5)) * poi
  
  new_case0080_n1 <- new_case00[n+1] + new_case10[n+1] + new_case20[n+1] + new_case30[n+1] + new_case40[n+1] + new_case50[n+1] + new_case60[n+1] + new_case70[n+1] + new_case80[n+1]
  
  pred00 <- pred0080 * (new_case00[n+1] / new_case0080_n1)
  pred10 <- pred0080 * (new_case10[n+1] / new_case0080_n1)
  pred20 <- pred0080 * (new_case20[n+1] / new_case0080_n1)
  pred30 <- pred0080 * (new_case30[n+1] / new_case0080_n1)
  pred40 <- pred0080 * (new_case40[n+1] / new_case0080_n1)
  pred50 <- pred0080 * (new_case50[n+1] / new_case0080_n1)
  pred60 <- pred0080 * (new_case60[n+1] / new_case0080_n1)
  pred70 <- pred0080 * (new_case70[n+1] / new_case0080_n1)
  pred80 <- pred0080 * (new_case80[n+1] / new_case0080_n1)
  
  # calculate log-likelihood
  
  loglik <- sum(dpois(as.integer(round(c(new_case00[n+1] + new_case10[n+1] + new_case20[n+1] + new_case30[n+1] + new_case40[n+1] + new_case50[n+1] + new_case60[n+1] + new_case70[n+1] + new_case80[n+1]))),
                      as.integer(round(c(pred00 + pred10 + pred20 + pred30 + pred40 + pred50 + pred60 + pred70 + pred80))),
                      log = TRUE))
  
  #return(-loglik)
  return(+loglik)
}


### function end




























# week by week


nn <- 99

rt_set <- rep(NA, nn)



# cumulative vac

vaccine_data$vaccine_00_cum <- cumsum(vaccine_data$vaccine_00)
vaccine_data$vaccine_10_cum <- cumsum(vaccine_data$vaccine_10)
vaccine_data$vaccine_20_cum <- cumsum(vaccine_data$vaccine_20)
vaccine_data$vaccine_30_cum <- cumsum(vaccine_data$vaccine_30)
vaccine_data$vaccine_40_cum <- cumsum(vaccine_data$vaccine_40)
vaccine_data$vaccine_50_cum <- cumsum(vaccine_data$vaccine_50)
vaccine_data$vaccine_60_cum <- cumsum(vaccine_data$vaccine_60)
vaccine_data$vaccine_70_cum <- cumsum(vaccine_data$vaccine_70)
vaccine_data$vaccine_80_cum <- cumsum(vaccine_data$vaccine_80)





# ### test no vac ###
# 
# vaccine_data$vaccine_00 <- 0
# vaccine_data$vaccine_10 <- 0
# vaccine_data$vaccine_20 <- 0
# vaccine_data$vaccine_30 <- 0
# vaccine_data$vaccine_40 <- 0
# vaccine_data$vaccine_50 <- 0
# vaccine_data$vaccine_60 <- 0
# vaccine_data$vaccine_70 <- 0
# vaccine_data$vaccine_80 <- 0
# 
# 
# 
# 
# vaccine_data$vaccine_00_cum <- 0
# vaccine_data$vaccine_10_cum <- 0
# vaccine_data$vaccine_20_cum <- 0
# vaccine_data$vaccine_30_cum <- 0
# vaccine_data$vaccine_40_cum <- 0
# vaccine_data$vaccine_50_cum <- 0
# vaccine_data$vaccine_60_cum <- 0
# vaccine_data$vaccine_70_cum <- 0
# vaccine_data$vaccine_80_cum <- 0
# 
# ### test no vac ###








### fit the model for loop

# cumulative data empty
vaccine00 <- rep(0, nn)
vaccine10 <- rep(0, nn)
vaccine20 <- rep(0, nn)
vaccine30 <- rep(0, nn)
vaccine40 <- rep(0, nn)
vaccine50 <- rep(0, nn)
vaccine60 <- rep(0, nn)
vaccine70 <- rep(0, nn)
vaccine80 <- rep(0, nn)

vaccine00[1:9] <- vaccine_data$vaccine_00[1:9]
vaccine10[1:9] <- vaccine_data$vaccine_10[1:9]
vaccine20[1:9] <- vaccine_data$vaccine_20[1:9]
vaccine30[1:9] <- vaccine_data$vaccine_30[1:9]
vaccine40[1:9] <- vaccine_data$vaccine_40[1:9]
vaccine50[1:9] <- vaccine_data$vaccine_50[1:9]
vaccine60[1:9] <- vaccine_data$vaccine_60[1:9]
vaccine70[1:9] <- vaccine_data$vaccine_70[1:9]
vaccine80[1:9] <- vaccine_data$vaccine_80[1:9]



cum_vac00 <- rep(0, nn)
cum_vac10 <- rep(0, nn)
cum_vac20 <- rep(0, nn)
cum_vac30 <- rep(0, nn)
cum_vac40 <- rep(0, nn)
cum_vac50 <- rep(0, nn)
cum_vac60 <- rep(0, nn)
cum_vac70 <- rep(0, nn)
cum_vac80 <- rep(0, nn)

cum_vac00[1:9] <- vaccine_data$vaccine_00_cum[1:9]
cum_vac10[1:9] <- vaccine_data$vaccine_10_cum[1:9]
cum_vac20[1:9] <- vaccine_data$vaccine_20_cum[1:9]
cum_vac30[1:9] <- vaccine_data$vaccine_30_cum[1:9]
cum_vac40[1:9] <- vaccine_data$vaccine_40_cum[1:9]
cum_vac50[1:9] <- vaccine_data$vaccine_50_cum[1:9]
cum_vac60[1:9] <- vaccine_data$vaccine_60_cum[1:9]
cum_vac70[1:9] <- vaccine_data$vaccine_70_cum[1:9]
cum_vac80[1:9] <- vaccine_data$vaccine_80_cum[1:9]


### empty pred
pred00_sofar <- rep(NA, nn+1)
pred10_sofar <- rep(NA, nn+1)
pred20_sofar <- rep(NA, nn+1)
pred30_sofar <- rep(NA, nn+1)
pred40_sofar <- rep(NA, nn+1)
pred50_sofar <- rep(NA, nn+1)
pred60_sofar <- rep(NA, nn+1)
pred70_sofar <- rep(NA, nn+1)
pred80_sofar <- rep(NA, nn+1)

pred00_sofar[10] <- all_data$case_00_diff[10]
pred10_sofar[10] <- all_data$case_10_diff[10]
pred20_sofar[10] <- all_data$case_20_diff[10]
pred30_sofar[10] <- all_data$case_30_diff[10]
pred40_sofar[10] <- all_data$case_40_diff[10]
pred50_sofar[10] <- all_data$case_50_diff[10]
pred60_sofar[10] <- all_data$case_60_diff[10]
pred70_sofar[10] <- all_data$case_70_diff[10]
pred80_sofar[10] <- all_data$case_80_diff[10]






### week by week

for (nnn in 10:nn) {
  
  # cumulative data fill out
  vaccine00[nnn] = vaccine_data$vaccine_00[nnn]
  vaccine10[nnn] = vaccine_data$vaccine_10[nnn]
  vaccine20[nnn] = vaccine_data$vaccine_20[nnn]
  vaccine30[nnn] = vaccine_data$vaccine_30[nnn]
  vaccine40[nnn] = vaccine_data$vaccine_40[nnn]
  vaccine50[nnn] = vaccine_data$vaccine_50[nnn]
  vaccine60[nnn] = vaccine_data$vaccine_60[nnn]
  vaccine70[nnn] = vaccine_data$vaccine_70[nnn]
  vaccine80[nnn] = vaccine_data$vaccine_80[nnn]
  
  cum_vac00[nnn] = vaccine_data$vaccine_00_cum[nnn]
  cum_vac10[nnn] = vaccine_data$vaccine_10_cum[nnn]
  cum_vac20[nnn] = vaccine_data$vaccine_20_cum[nnn]
  cum_vac30[nnn] = vaccine_data$vaccine_30_cum[nnn]
  cum_vac40[nnn] = vaccine_data$vaccine_40_cum[nnn]
  cum_vac50[nnn] = vaccine_data$vaccine_50_cum[nnn]
  cum_vac60[nnn] = vaccine_data$vaccine_60_cum[nnn]
  cum_vac70[nnn] = vaccine_data$vaccine_70_cum[nnn]
  cum_vac80[nnn] = vaccine_data$vaccine_80_cum[nnn]
  
  
  
  new_case00 <- all_data$case_00_diff
  new_case10 <- all_data$case_10_diff
  new_case20 <- all_data$case_20_diff
  new_case30 <- all_data$case_30_diff
  new_case40 <- all_data$case_40_diff
  new_case50 <- all_data$case_50_diff
  new_case60 <- all_data$case_60_diff
  new_case70 <- all_data$case_70_diff
  new_case80 <- all_data$case_80_diff
  
  
  
  alpha_inf_young_model <- ve_waning_model$inf_young_alpha_model
  delta_inf_young_model <- ve_waning_model$inf_young_delta_model
  
  alpha_hosp_young_model2 <- ve_waning_model$hosp_young_alpha_model2
  delta_hosp_young_model2 <- ve_waning_model$hosp_young_delta_model2
  
  alpha_inf_old_model <- ve_waning_model$inf_old_alpha_model
  delta_inf_old_model <- ve_waning_model$inf_old_delta_model
  
  alpha_hosp_old_model2 <- ve_waning_model$hosp_old_alpha_model2
  delta_hosp_old_model2 <- ve_waning_model$hosp_old_delta_model2
  
  trans_alpha <- ve_waning_model$trans_alpha
  trans_delta <- ve_waning_model$trans_delta
  
  delta_prop <- variant_data$delta_model
  
  population00 <- population_data$population[age==0]
  population10 <- population_data$population[age==10]
  population20 <- population_data$population[age==20]
  population30 <- population_data$population[age==30]
  population40 <- population_data$population[age==40]
  population50 <- population_data$population[age==50]
  population60 <- population_data$population[age==60]
  population70 <- population_data$population[age==70]
  population80 <- population_data$population[age==80]
  
  fit <- maxLik(rt_loglik,
                start = c(rt=0),
                # fixed = c("duration", "deltaUP"),
                # method = "NR",  #default?
                method = "SANN", #works non-stop?
                # method = "NM", #sometimes works?
                nn = nnn,
                
                new_case00 = new_case00,
                new_case10 = new_case10,
                new_case20 = new_case20,
                new_case30 = new_case30,
                new_case40 = new_case40,
                new_case50 = new_case50,
                new_case60 = new_case60,
                new_case70 = new_case70,
                new_case80 = new_case80,
                
                vaccine00 = vaccine00,
                vaccine10 = vaccine10,
                vaccine20 = vaccine20,
                vaccine30 = vaccine30,
                vaccine40 = vaccine40,
                vaccine50 = vaccine50,
                vaccine60 = vaccine60,
                vaccine70 = vaccine70,
                vaccine80 = vaccine80,
                
                pred00_sofar = pred00_sofar,
                pred10_sofar = pred10_sofar,
                pred20_sofar = pred20_sofar,
                pred30_sofar = pred30_sofar,
                pred40_sofar = pred40_sofar,
                pred50_sofar = pred50_sofar,
                pred60_sofar = pred60_sofar,
                pred70_sofar = pred70_sofar,
                pred80_sofar = pred80_sofar,
                
                cum_vac00 = cum_vac00,
                cum_vac10 = cum_vac10,
                cum_vac20 = cum_vac20,
                cum_vac30 = cum_vac30,
                cum_vac40 = cum_vac40,
                cum_vac50 = cum_vac50,
                cum_vac60 = cum_vac60,
                cum_vac70 = cum_vac70,
                cum_vac80 = cum_vac80,
                
                population00 = population00,
                population10 = population10,
                population20 = population20,
                population30 = population30,
                population40 = population40,
                population50 = population50,
                population60 = population60,
                population70 = population70,
                population80 = population80,
                
                alpha_inf_young_model = alpha_inf_young_model,
                delta_inf_young_model = delta_inf_young_model,
                
                alpha_hosp_young_model2 = alpha_hosp_young_model2,
                delta_hosp_young_model2 = delta_hosp_young_model2,
                
                alpha_inf_old_model = alpha_inf_old_model,
                delta_inf_old_model = delta_inf_old_model,
                
                alpha_hosp_old_model2 = alpha_hosp_old_model2,
                delta_hosp_old_model2 = delta_hosp_old_model2,
                
                trans_alpha = trans_alpha,
                trans_delta = trans_delta,
                
                delta_prop = delta_prop)
  
  
  rt_set[nnn] <- exp(fit$estimate)
  
  
  
  
  
  # fill in new pred_sofar
  n00 <- nnn
  n01 <- ifelse(nnn-1<1, 1, nnn-1)
  n02 <- ifelse(nnn-2<1, 1, nnn-2)
  n03 <- ifelse(nnn-3<1, 1, nnn-3)
  n04 <- ifelse(nnn-4<1, 1, nnn-4)
  n05 <- ifelse(nnn-5<1, 1, nnn-5)
  n06 <- ifelse(nnn-6<1, 1, nnn-6)
  n07 <- ifelse(nnn-7<1, 1, nnn-7)
  n08 <- ifelse(nnn-8<1, 1, nnn-8)
  n09 <- ifelse(nnn-9<1, 1, nnn-9)
  n10 <- ifelse(nnn-10<1, 1, nnn-10)
  n11 <- ifelse(nnn-11<1, 1, nnn-11)
  n12 <- ifelse(nnn-12<1, 1, nnn-12)
  n13 <- ifelse(nnn-13<1, 1, nnn-13)
  n14 <- ifelse(nnn-14<1, 1, nnn-14)
  n15 <- ifelse(nnn-15<1, 1, nnn-15)
  n16 <- ifelse(nnn-16<1, 1, nnn-16)
  n17 <- ifelse(nnn-17<1, 1, nnn-17)
  n18 <- ifelse(nnn-18<1, 1, nnn-18)
  n19 <- ifelse(nnn-19<1, 1, nnn-19)
  n20 <- ifelse(nnn-20<1, 1, nnn-20)
  n21 <- ifelse(nnn-21<1, 1, nnn-21)
  n22 <- ifelse(nnn-22<1, 1, nnn-22)
  n23 <- ifelse(nnn-23<1, 1, nnn-23)
  n24 <- ifelse(nnn-24<1, 1, nnn-24)
  n25 <- ifelse(nnn-25<1, 1, nnn-25)
  n26 <- ifelse(nnn-26<1, 1, nnn-26)
  n27 <- ifelse(nnn-27<1, 1, nnn-27)
  n28 <- ifelse(nnn-28<1, 1, nnn-28)
  n29 <- ifelse(nnn-29<1, 1, nnn-29)
  n30 <- ifelse(nnn-30<1, 1, nnn-30)
  n31 <- ifelse(nnn-31<1, 1, nnn-31)
  n32 <- ifelse(nnn-32<1, 1, nnn-32)
  n33 <- ifelse(nnn-33<1, 1, nnn-33)
  n34 <- ifelse(nnn-34<1, 1, nnn-34)
  n35 <- ifelse(nnn-35<1, 1, nnn-35)
  n36 <- ifelse(nnn-36<1, 1, nnn-36)
  n37 <- ifelse(nnn-37<1, 1, nnn-37)
  n38 <- ifelse(nnn-38<1, 1, nnn-38)
  n39 <- ifelse(nnn-39<1, 1, nnn-39)
  n40 <- ifelse(nnn-40<1, 1, nnn-40)
  n41 <- ifelse(nnn-41<1, 1, nnn-41)
  n42 <- ifelse(nnn-42<1, 1, nnn-42)
  n43 <- ifelse(nnn-43<1, 1, nnn-43)
  n44 <- ifelse(nnn-44<1, 1, nnn-44)
  n45 <- ifelse(nnn-45<1, 1, nnn-45)
  n46 <- ifelse(nnn-46<1, 1, nnn-46)
  n47 <- ifelse(nnn-47<1, 1, nnn-47)
  n48 <- ifelse(nnn-48<1, 1, nnn-48)
  n49 <- ifelse(nnn-49<1, 1, nnn-49)
  n50 <- ifelse(nnn-50<1, 1, nnn-50)
  n51 <- ifelse(nnn-51<1, 1, nnn-51)
  n52 <- ifelse(nnn-52<1, 1, nnn-52)
  n53 <- ifelse(nnn-53<1, 1, nnn-53)
  n54 <- ifelse(nnn-54<1, 1, nnn-54)
  n55 <- ifelse(nnn-55<1, 1, nnn-55)
  n56 <- ifelse(nnn-56<1, 1, nnn-56)
  n57 <- ifelse(nnn-57<1, 1, nnn-57)
  n58 <- ifelse(nnn-58<1, 1, nnn-58)
  n59 <- ifelse(nnn-59<1, 1, nnn-59)
  n60 <- ifelse(nnn-60<1, 1, nnn-60)
  n61 <- ifelse(nnn-61<1, 1, nnn-61)
  n62 <- ifelse(nnn-62<1, 1, nnn-62)
  n63 <- ifelse(nnn-63<1, 1, nnn-63)
  n64 <- ifelse(nnn-64<1, 1, nnn-64)
  n65 <- ifelse(nnn-65<1, 1, nnn-65)
  n66 <- ifelse(nnn-66<1, 1, nnn-66)
  n67 <- ifelse(nnn-67<1, 1, nnn-67)
  n68 <- ifelse(nnn-68<1, 1, nnn-68)
  n69 <- ifelse(nnn-69<1, 1, nnn-69)
  n70 <- ifelse(nnn-70<1, 1, nnn-70)
  n71 <- ifelse(nnn-71<1, 1, nnn-71)
  n72 <- ifelse(nnn-72<1, 1, nnn-72)
  n73 <- ifelse(nnn-73<1, 1, nnn-73)
  n74 <- ifelse(nnn-74<1, 1, nnn-74)
  n75 <- ifelse(nnn-75<1, 1, nnn-75)
  n76 <- ifelse(nnn-76<1, 1, nnn-76)
  n77 <- ifelse(nnn-77<1, 1, nnn-77)
  n78 <- ifelse(nnn-78<1, 1, nnn-78)
  n79 <- ifelse(nnn-79<1, 1, nnn-79)
  n80 <- ifelse(nnn-80<1, 1, nnn-80)
  n81 <- ifelse(nnn-81<1, 1, nnn-81)
  n82 <- ifelse(nnn-82<1, 1, nnn-82)
  n83 <- ifelse(nnn-83<1, 1, nnn-83)
  n84 <- ifelse(nnn-84<1, 1, nnn-84)
  n85 <- ifelse(nnn-85<1, 1, nnn-85)
  n86 <- ifelse(nnn-86<1, 1, nnn-86)
  n87 <- ifelse(nnn-87<1, 1, nnn-87)
  n88 <- ifelse(nnn-88<1, 1, nnn-88)
  n89 <- ifelse(nnn-89<1, 1, nnn-89)
  n90 <- ifelse(nnn-90<1, 1, nnn-90)
  n91 <- ifelse(nnn-91<1, 1, nnn-91)
  n92 <- ifelse(nnn-92<1, 1, nnn-92)
  n93 <- ifelse(nnn-93<1, 1, nnn-93)
  n94 <- ifelse(nnn-94<1, 1, nnn-94)
  n95 <- ifelse(nnn-95<1, 1, nnn-95)
  n96 <- ifelse(nnn-96<1, 1, nnn-96)
  n97 <- ifelse(nnn-97<1, 1, nnn-97)
  n98 <- ifelse(nnn-98<1, 1, nnn-98)
  
  
  poi00 <- pred00_sofar[nnn] *
    (vaccine00[1] *  (1 - trans_alpha[n00] * (1 - delta_prop[nnn]) - trans_delta[n00] * delta_prop[nnn]) +
       vaccine00[2] *  (1 - trans_alpha[n01] * (1 - delta_prop[nnn]) - trans_delta[n01] * delta_prop[nnn]) +
       vaccine00[3] *  (1 - trans_alpha[n02] * (1 - delta_prop[nnn]) - trans_delta[n02] * delta_prop[nnn]) +
       vaccine00[4] *  (1 - trans_alpha[n03] * (1 - delta_prop[nnn]) - trans_delta[n03] * delta_prop[nnn]) +
       vaccine00[5] *  (1 - trans_alpha[n04] * (1 - delta_prop[nnn]) - trans_delta[n04] * delta_prop[nnn]) +
       vaccine00[6] *  (1 - trans_alpha[n05] * (1 - delta_prop[nnn]) - trans_delta[n05] * delta_prop[nnn]) +
       vaccine00[7] *  (1 - trans_alpha[n06] * (1 - delta_prop[nnn]) - trans_delta[n06] * delta_prop[nnn]) +
       vaccine00[8] *  (1 - trans_alpha[n07] * (1 - delta_prop[nnn]) - trans_delta[n07] * delta_prop[nnn]) +
       vaccine00[9] *  (1 - trans_alpha[n08] * (1 - delta_prop[nnn]) - trans_delta[n08] * delta_prop[nnn]) +
       vaccine00[10] * (1 - trans_alpha[n09] * (1 - delta_prop[nnn]) - trans_delta[n09] * delta_prop[nnn]) +
       vaccine00[11] * (1 - trans_alpha[n10] * (1 - delta_prop[nnn]) - trans_delta[n10] * delta_prop[nnn]) +
       vaccine00[12] * (1 - trans_alpha[n11] * (1 - delta_prop[nnn]) - trans_delta[n11] * delta_prop[nnn]) +
       vaccine00[13] * (1 - trans_alpha[n12] * (1 - delta_prop[nnn]) - trans_delta[n12] * delta_prop[nnn]) +
       vaccine00[14] * (1 - trans_alpha[n13] * (1 - delta_prop[nnn]) - trans_delta[n13] * delta_prop[nnn]) +
       vaccine00[15] * (1 - trans_alpha[n14] * (1 - delta_prop[nnn]) - trans_delta[n14] * delta_prop[nnn]) +
       vaccine00[16] * (1 - trans_alpha[n15] * (1 - delta_prop[nnn]) - trans_delta[n15] * delta_prop[nnn]) +
       vaccine00[17] * (1 - trans_alpha[n16] * (1 - delta_prop[nnn]) - trans_delta[n16] * delta_prop[nnn]) +
       vaccine00[18] * (1 - trans_alpha[n17] * (1 - delta_prop[nnn]) - trans_delta[n17] * delta_prop[nnn]) +
       vaccine00[19] * (1 - trans_alpha[n18] * (1 - delta_prop[nnn]) - trans_delta[n18] * delta_prop[nnn]) +
       vaccine00[20] * (1 - trans_alpha[n19] * (1 - delta_prop[nnn]) - trans_delta[n19] * delta_prop[nnn]) +
       vaccine00[21] * (1 - trans_alpha[n20] * (1 - delta_prop[nnn]) - trans_delta[n20] * delta_prop[nnn]) +
       vaccine00[22] * (1 - trans_alpha[n21] * (1 - delta_prop[nnn]) - trans_delta[n21] * delta_prop[nnn]) +
       vaccine00[23] * (1 - trans_alpha[n22] * (1 - delta_prop[nnn]) - trans_delta[n22] * delta_prop[nnn]) +
       vaccine00[24] * (1 - trans_alpha[n23] * (1 - delta_prop[nnn]) - trans_delta[n23] * delta_prop[nnn]) +
       vaccine00[25] * (1 - trans_alpha[n24] * (1 - delta_prop[nnn]) - trans_delta[n24] * delta_prop[nnn]) +
       vaccine00[26] * (1 - trans_alpha[n25] * (1 - delta_prop[nnn]) - trans_delta[n25] * delta_prop[nnn]) +
       vaccine00[27] * (1 - trans_alpha[n26] * (1 - delta_prop[nnn]) - trans_delta[n26] * delta_prop[nnn]) +
       vaccine00[28] * (1 - trans_alpha[n27] * (1 - delta_prop[nnn]) - trans_delta[n27] * delta_prop[nnn]) +
       vaccine00[29] * (1 - trans_alpha[n28] * (1 - delta_prop[nnn]) - trans_delta[n28] * delta_prop[nnn]) +
       vaccine00[30] * (1 - trans_alpha[n29] * (1 - delta_prop[nnn]) - trans_delta[n29] * delta_prop[nnn]) +
       vaccine00[31] * (1 - trans_alpha[n30] * (1 - delta_prop[nnn]) - trans_delta[n30] * delta_prop[nnn]) +
       vaccine00[32] * (1 - trans_alpha[n31] * (1 - delta_prop[nnn]) - trans_delta[n31] * delta_prop[nnn]) +
       vaccine00[33] * (1 - trans_alpha[n32] * (1 - delta_prop[nnn]) - trans_delta[n32] * delta_prop[nnn]) +
       vaccine00[34] * (1 - trans_alpha[n33] * (1 - delta_prop[nnn]) - trans_delta[n33] * delta_prop[nnn]) +
       vaccine00[35] * (1 - trans_alpha[n34] * (1 - delta_prop[nnn]) - trans_delta[n34] * delta_prop[nnn]) +
       vaccine00[36] * (1 - trans_alpha[n35] * (1 - delta_prop[nnn]) - trans_delta[n35] * delta_prop[nnn]) +
       vaccine00[37] * (1 - trans_alpha[n36] * (1 - delta_prop[nnn]) - trans_delta[n36] * delta_prop[nnn]) +
       vaccine00[38] * (1 - trans_alpha[n37] * (1 - delta_prop[nnn]) - trans_delta[n37] * delta_prop[nnn]) +
       vaccine00[39] * (1 - trans_alpha[n38] * (1 - delta_prop[nnn]) - trans_delta[n38] * delta_prop[nnn]) +
       vaccine00[40] * (1 - trans_alpha[n39] * (1 - delta_prop[nnn]) - trans_delta[n39] * delta_prop[nnn]) +
       vaccine00[41] * (1 - trans_alpha[n40] * (1 - delta_prop[nnn]) - trans_delta[n40] * delta_prop[nnn]) +
       vaccine00[42] * (1 - trans_alpha[n41] * (1 - delta_prop[nnn]) - trans_delta[n41] * delta_prop[nnn]) +
       vaccine00[43] * (1 - trans_alpha[n42] * (1 - delta_prop[nnn]) - trans_delta[n42] * delta_prop[nnn]) +
       vaccine00[44] * (1 - trans_alpha[n43] * (1 - delta_prop[nnn]) - trans_delta[n43] * delta_prop[nnn]) +
       vaccine00[45] * (1 - trans_alpha[n44] * (1 - delta_prop[nnn]) - trans_delta[n44] * delta_prop[nnn]) +
       vaccine00[46] * (1 - trans_alpha[n45] * (1 - delta_prop[nnn]) - trans_delta[n45] * delta_prop[nnn]) +
       vaccine00[47] * (1 - trans_alpha[n46] * (1 - delta_prop[nnn]) - trans_delta[n46] * delta_prop[nnn]) +
       vaccine00[48] * (1 - trans_alpha[n47] * (1 - delta_prop[nnn]) - trans_delta[n47] * delta_prop[nnn]) +
       vaccine00[49] * (1 - trans_alpha[n48] * (1 - delta_prop[nnn]) - trans_delta[n48] * delta_prop[nnn]) +
       vaccine00[50] * (1 - trans_alpha[n49] * (1 - delta_prop[nnn]) - trans_delta[n49] * delta_prop[nnn]) +
       vaccine00[51] * (1 - trans_alpha[n50] * (1 - delta_prop[nnn]) - trans_delta[n50] * delta_prop[nnn]) +
       vaccine00[52] * (1 - trans_alpha[n51] * (1 - delta_prop[nnn]) - trans_delta[n51] * delta_prop[nnn]) +
       vaccine00[53] * (1 - trans_alpha[n52] * (1 - delta_prop[nnn]) - trans_delta[n52] * delta_prop[nnn]) +
       vaccine00[54] * (1 - trans_alpha[n53] * (1 - delta_prop[nnn]) - trans_delta[n53] * delta_prop[nnn]) +
       vaccine00[55] * (1 - trans_alpha[n54] * (1 - delta_prop[nnn]) - trans_delta[n54] * delta_prop[nnn]) +
       vaccine00[56] * (1 - trans_alpha[n55] * (1 - delta_prop[nnn]) - trans_delta[n55] * delta_prop[nnn]) +
       vaccine00[57] * (1 - trans_alpha[n56] * (1 - delta_prop[nnn]) - trans_delta[n56] * delta_prop[nnn]) +
       vaccine00[58] * (1 - trans_alpha[n57] * (1 - delta_prop[nnn]) - trans_delta[n57] * delta_prop[nnn]) +
       vaccine00[59] * (1 - trans_alpha[n58] * (1 - delta_prop[nnn]) - trans_delta[n58] * delta_prop[nnn]) +
       vaccine00[60] * (1 - trans_alpha[n59] * (1 - delta_prop[nnn]) - trans_delta[n59] * delta_prop[nnn]) +
       vaccine00[61] * (1 - trans_alpha[n60] * (1 - delta_prop[nnn]) - trans_delta[n60] * delta_prop[nnn]) +
       vaccine00[62] * (1 - trans_alpha[n61] * (1 - delta_prop[nnn]) - trans_delta[n61] * delta_prop[nnn]) +
       vaccine00[63] * (1 - trans_alpha[n62] * (1 - delta_prop[nnn]) - trans_delta[n62] * delta_prop[nnn]) +
       vaccine00[64] * (1 - trans_alpha[n63] * (1 - delta_prop[nnn]) - trans_delta[n63] * delta_prop[nnn]) +
       vaccine00[65] * (1 - trans_alpha[n64] * (1 - delta_prop[nnn]) - trans_delta[n64] * delta_prop[nnn]) +
       vaccine00[66] * (1 - trans_alpha[n65] * (1 - delta_prop[nnn]) - trans_delta[n65] * delta_prop[nnn]) +
       vaccine00[67] * (1 - trans_alpha[n66] * (1 - delta_prop[nnn]) - trans_delta[n66] * delta_prop[nnn]) +
       vaccine00[68] * (1 - trans_alpha[n67] * (1 - delta_prop[nnn]) - trans_delta[n67] * delta_prop[nnn]) +
       vaccine00[69] * (1 - trans_alpha[n68] * (1 - delta_prop[nnn]) - trans_delta[n68] * delta_prop[nnn]) +
       vaccine00[70] * (1 - trans_alpha[n69] * (1 - delta_prop[nnn]) - trans_delta[n69] * delta_prop[nnn]) +
       vaccine00[71] * (1 - trans_alpha[n70] * (1 - delta_prop[nnn]) - trans_delta[n70] * delta_prop[nnn]) +
       vaccine00[72] * (1 - trans_alpha[n71] * (1 - delta_prop[nnn]) - trans_delta[n71] * delta_prop[nnn]) +
       vaccine00[73] * (1 - trans_alpha[n72] * (1 - delta_prop[nnn]) - trans_delta[n72] * delta_prop[nnn]) +
       vaccine00[74] * (1 - trans_alpha[n73] * (1 - delta_prop[nnn]) - trans_delta[n73] * delta_prop[nnn]) +
       vaccine00[75] * (1 - trans_alpha[n74] * (1 - delta_prop[nnn]) - trans_delta[n74] * delta_prop[nnn]) +
       vaccine00[76] * (1 - trans_alpha[n75] * (1 - delta_prop[nnn]) - trans_delta[n75] * delta_prop[nnn]) +
       vaccine00[77] * (1 - trans_alpha[n76] * (1 - delta_prop[nnn]) - trans_delta[n76] * delta_prop[nnn]) +
       vaccine00[78] * (1 - trans_alpha[n77] * (1 - delta_prop[nnn]) - trans_delta[n77] * delta_prop[nnn]) +
       vaccine00[79] * (1 - trans_alpha[n78] * (1 - delta_prop[nnn]) - trans_delta[n78] * delta_prop[nnn]) +
       vaccine00[80] * (1 - trans_alpha[n79] * (1 - delta_prop[nnn]) - trans_delta[n79] * delta_prop[nnn]) +
       vaccine00[81] * (1 - trans_alpha[n80] * (1 - delta_prop[nnn]) - trans_delta[n80] * delta_prop[nnn]) +
       vaccine00[82] * (1 - trans_alpha[n81] * (1 - delta_prop[nnn]) - trans_delta[n81] * delta_prop[nnn]) +
       vaccine00[83] * (1 - trans_alpha[n82] * (1 - delta_prop[nnn]) - trans_delta[n82] * delta_prop[nnn]) +
       vaccine00[84] * (1 - trans_alpha[n83] * (1 - delta_prop[nnn]) - trans_delta[n83] * delta_prop[nnn]) +
       vaccine00[85] * (1 - trans_alpha[n84] * (1 - delta_prop[nnn]) - trans_delta[n84] * delta_prop[nnn]) +
       vaccine00[86] * (1 - trans_alpha[n85] * (1 - delta_prop[nnn]) - trans_delta[n85] * delta_prop[nnn]) +
       vaccine00[87] * (1 - trans_alpha[n86] * (1 - delta_prop[nnn]) - trans_delta[n86] * delta_prop[nnn]) +
       vaccine00[88] * (1 - trans_alpha[n87] * (1 - delta_prop[nnn]) - trans_delta[n87] * delta_prop[nnn]) +
       vaccine00[89] * (1 - trans_alpha[n88] * (1 - delta_prop[nnn]) - trans_delta[n88] * delta_prop[nnn]) +
       vaccine00[90] * (1 - trans_alpha[n89] * (1 - delta_prop[nnn]) - trans_delta[n89] * delta_prop[nnn]) +
       vaccine00[91] * (1 - trans_alpha[n90] * (1 - delta_prop[nnn]) - trans_delta[n90] * delta_prop[nnn]) +
       vaccine00[92] * (1 - trans_alpha[n91] * (1 - delta_prop[nnn]) - trans_delta[n91] * delta_prop[nnn]) +
       vaccine00[93] * (1 - trans_alpha[n92] * (1 - delta_prop[nnn]) - trans_delta[n92] * delta_prop[nnn]) +
       vaccine00[94] * (1 - trans_alpha[n93] * (1 - delta_prop[nnn]) - trans_delta[n93] * delta_prop[nnn]) +
       vaccine00[95] * (1 - trans_alpha[n94] * (1 - delta_prop[nnn]) - trans_delta[n94] * delta_prop[nnn]) +
       vaccine00[96] * (1 - trans_alpha[n95] * (1 - delta_prop[nnn]) - trans_delta[n95] * delta_prop[nnn]) +
       vaccine00[97] * (1 - trans_alpha[n96] * (1 - delta_prop[nnn]) - trans_delta[n96] * delta_prop[nnn]) +
       vaccine00[98] * (1 - trans_alpha[n97] * (1 - delta_prop[nnn]) - trans_delta[n97] * delta_prop[nnn]) +
       vaccine00[99] * (1 - trans_alpha[n98] * (1 - delta_prop[nnn]) - trans_delta[n98] * delta_prop[nnn]) +
       ifelse((1 - cum_vac00[nnn] - ((sum(new_case00[1:9]) + sum(pred00_sofar[10:nnn])) / population00)) < 0,
              0,
              1 - cum_vac00[nnn] - ((sum(new_case00[1:9]) + sum(pred00_sofar[10:nnn])) / population00)))
  
  poi10 <- pred10_sofar[nnn] *
    (vaccine10[1] *  (1 - trans_alpha[n00] * (1 - delta_prop[nnn]) - trans_delta[n00] * delta_prop[nnn]) +
       vaccine10[2] *  (1 - trans_alpha[n01] * (1 - delta_prop[nnn]) - trans_delta[n01] * delta_prop[nnn]) +
       vaccine10[3] *  (1 - trans_alpha[n02] * (1 - delta_prop[nnn]) - trans_delta[n02] * delta_prop[nnn]) +
       vaccine10[4] *  (1 - trans_alpha[n03] * (1 - delta_prop[nnn]) - trans_delta[n03] * delta_prop[nnn]) +
       vaccine10[5] *  (1 - trans_alpha[n04] * (1 - delta_prop[nnn]) - trans_delta[n04] * delta_prop[nnn]) +
       vaccine10[6] *  (1 - trans_alpha[n05] * (1 - delta_prop[nnn]) - trans_delta[n05] * delta_prop[nnn]) +
       vaccine10[7] *  (1 - trans_alpha[n06] * (1 - delta_prop[nnn]) - trans_delta[n06] * delta_prop[nnn]) +
       vaccine10[8] *  (1 - trans_alpha[n07] * (1 - delta_prop[nnn]) - trans_delta[n07] * delta_prop[nnn]) +
       vaccine10[9] *  (1 - trans_alpha[n08] * (1 - delta_prop[nnn]) - trans_delta[n08] * delta_prop[nnn]) +
       vaccine10[10] * (1 - trans_alpha[n09] * (1 - delta_prop[nnn]) - trans_delta[n09] * delta_prop[nnn]) +
       vaccine10[11] * (1 - trans_alpha[n10] * (1 - delta_prop[nnn]) - trans_delta[n10] * delta_prop[nnn]) +
       vaccine10[12] * (1 - trans_alpha[n11] * (1 - delta_prop[nnn]) - trans_delta[n11] * delta_prop[nnn]) +
       vaccine10[13] * (1 - trans_alpha[n12] * (1 - delta_prop[nnn]) - trans_delta[n12] * delta_prop[nnn]) +
       vaccine10[14] * (1 - trans_alpha[n13] * (1 - delta_prop[nnn]) - trans_delta[n13] * delta_prop[nnn]) +
       vaccine10[15] * (1 - trans_alpha[n14] * (1 - delta_prop[nnn]) - trans_delta[n14] * delta_prop[nnn]) +
       vaccine10[16] * (1 - trans_alpha[n15] * (1 - delta_prop[nnn]) - trans_delta[n15] * delta_prop[nnn]) +
       vaccine10[17] * (1 - trans_alpha[n16] * (1 - delta_prop[nnn]) - trans_delta[n16] * delta_prop[nnn]) +
       vaccine10[18] * (1 - trans_alpha[n17] * (1 - delta_prop[nnn]) - trans_delta[n17] * delta_prop[nnn]) +
       vaccine10[19] * (1 - trans_alpha[n18] * (1 - delta_prop[nnn]) - trans_delta[n18] * delta_prop[nnn]) +
       vaccine10[20] * (1 - trans_alpha[n19] * (1 - delta_prop[nnn]) - trans_delta[n19] * delta_prop[nnn]) +
       vaccine10[21] * (1 - trans_alpha[n20] * (1 - delta_prop[nnn]) - trans_delta[n20] * delta_prop[nnn]) +
       vaccine10[22] * (1 - trans_alpha[n21] * (1 - delta_prop[nnn]) - trans_delta[n21] * delta_prop[nnn]) +
       vaccine10[23] * (1 - trans_alpha[n22] * (1 - delta_prop[nnn]) - trans_delta[n22] * delta_prop[nnn]) +
       vaccine10[24] * (1 - trans_alpha[n23] * (1 - delta_prop[nnn]) - trans_delta[n23] * delta_prop[nnn]) +
       vaccine10[25] * (1 - trans_alpha[n24] * (1 - delta_prop[nnn]) - trans_delta[n24] * delta_prop[nnn]) +
       vaccine10[26] * (1 - trans_alpha[n25] * (1 - delta_prop[nnn]) - trans_delta[n25] * delta_prop[nnn]) +
       vaccine10[27] * (1 - trans_alpha[n26] * (1 - delta_prop[nnn]) - trans_delta[n26] * delta_prop[nnn]) +
       vaccine10[28] * (1 - trans_alpha[n27] * (1 - delta_prop[nnn]) - trans_delta[n27] * delta_prop[nnn]) +
       vaccine10[29] * (1 - trans_alpha[n28] * (1 - delta_prop[nnn]) - trans_delta[n28] * delta_prop[nnn]) +
       vaccine10[30] * (1 - trans_alpha[n29] * (1 - delta_prop[nnn]) - trans_delta[n29] * delta_prop[nnn]) +
       vaccine10[31] * (1 - trans_alpha[n30] * (1 - delta_prop[nnn]) - trans_delta[n30] * delta_prop[nnn]) +
       vaccine10[32] * (1 - trans_alpha[n31] * (1 - delta_prop[nnn]) - trans_delta[n31] * delta_prop[nnn]) +
       vaccine10[33] * (1 - trans_alpha[n32] * (1 - delta_prop[nnn]) - trans_delta[n32] * delta_prop[nnn]) +
       vaccine10[34] * (1 - trans_alpha[n33] * (1 - delta_prop[nnn]) - trans_delta[n33] * delta_prop[nnn]) +
       vaccine10[35] * (1 - trans_alpha[n34] * (1 - delta_prop[nnn]) - trans_delta[n34] * delta_prop[nnn]) +
       vaccine10[36] * (1 - trans_alpha[n35] * (1 - delta_prop[nnn]) - trans_delta[n35] * delta_prop[nnn]) +
       vaccine10[37] * (1 - trans_alpha[n36] * (1 - delta_prop[nnn]) - trans_delta[n36] * delta_prop[nnn]) +
       vaccine10[38] * (1 - trans_alpha[n37] * (1 - delta_prop[nnn]) - trans_delta[n37] * delta_prop[nnn]) +
       vaccine10[39] * (1 - trans_alpha[n38] * (1 - delta_prop[nnn]) - trans_delta[n38] * delta_prop[nnn]) +
       vaccine10[40] * (1 - trans_alpha[n39] * (1 - delta_prop[nnn]) - trans_delta[n39] * delta_prop[nnn]) +
       vaccine10[41] * (1 - trans_alpha[n40] * (1 - delta_prop[nnn]) - trans_delta[n40] * delta_prop[nnn]) +
       vaccine10[42] * (1 - trans_alpha[n41] * (1 - delta_prop[nnn]) - trans_delta[n41] * delta_prop[nnn]) +
       vaccine10[43] * (1 - trans_alpha[n42] * (1 - delta_prop[nnn]) - trans_delta[n42] * delta_prop[nnn]) +
       vaccine10[44] * (1 - trans_alpha[n43] * (1 - delta_prop[nnn]) - trans_delta[n43] * delta_prop[nnn]) +
       vaccine10[45] * (1 - trans_alpha[n44] * (1 - delta_prop[nnn]) - trans_delta[n44] * delta_prop[nnn]) +
       vaccine10[46] * (1 - trans_alpha[n45] * (1 - delta_prop[nnn]) - trans_delta[n45] * delta_prop[nnn]) +
       vaccine10[47] * (1 - trans_alpha[n46] * (1 - delta_prop[nnn]) - trans_delta[n46] * delta_prop[nnn]) +
       vaccine10[48] * (1 - trans_alpha[n47] * (1 - delta_prop[nnn]) - trans_delta[n47] * delta_prop[nnn]) +
       vaccine10[49] * (1 - trans_alpha[n48] * (1 - delta_prop[nnn]) - trans_delta[n48] * delta_prop[nnn]) +
       vaccine10[50] * (1 - trans_alpha[n49] * (1 - delta_prop[nnn]) - trans_delta[n49] * delta_prop[nnn]) +
       vaccine10[51] * (1 - trans_alpha[n50] * (1 - delta_prop[nnn]) - trans_delta[n50] * delta_prop[nnn]) +
       vaccine10[52] * (1 - trans_alpha[n51] * (1 - delta_prop[nnn]) - trans_delta[n51] * delta_prop[nnn]) +
       vaccine10[53] * (1 - trans_alpha[n52] * (1 - delta_prop[nnn]) - trans_delta[n52] * delta_prop[nnn]) +
       vaccine10[54] * (1 - trans_alpha[n53] * (1 - delta_prop[nnn]) - trans_delta[n53] * delta_prop[nnn]) +
       vaccine10[55] * (1 - trans_alpha[n54] * (1 - delta_prop[nnn]) - trans_delta[n54] * delta_prop[nnn]) +
       vaccine10[56] * (1 - trans_alpha[n55] * (1 - delta_prop[nnn]) - trans_delta[n55] * delta_prop[nnn]) +
       vaccine10[57] * (1 - trans_alpha[n56] * (1 - delta_prop[nnn]) - trans_delta[n56] * delta_prop[nnn]) +
       vaccine10[58] * (1 - trans_alpha[n57] * (1 - delta_prop[nnn]) - trans_delta[n57] * delta_prop[nnn]) +
       vaccine10[59] * (1 - trans_alpha[n58] * (1 - delta_prop[nnn]) - trans_delta[n58] * delta_prop[nnn]) +
       vaccine10[60] * (1 - trans_alpha[n59] * (1 - delta_prop[nnn]) - trans_delta[n59] * delta_prop[nnn]) +
       vaccine10[61] * (1 - trans_alpha[n60] * (1 - delta_prop[nnn]) - trans_delta[n60] * delta_prop[nnn]) +
       vaccine10[62] * (1 - trans_alpha[n61] * (1 - delta_prop[nnn]) - trans_delta[n61] * delta_prop[nnn]) +
       vaccine10[63] * (1 - trans_alpha[n62] * (1 - delta_prop[nnn]) - trans_delta[n62] * delta_prop[nnn]) +
       vaccine10[64] * (1 - trans_alpha[n63] * (1 - delta_prop[nnn]) - trans_delta[n63] * delta_prop[nnn]) +
       vaccine10[65] * (1 - trans_alpha[n64] * (1 - delta_prop[nnn]) - trans_delta[n64] * delta_prop[nnn]) +
       vaccine10[66] * (1 - trans_alpha[n65] * (1 - delta_prop[nnn]) - trans_delta[n65] * delta_prop[nnn]) +
       vaccine10[67] * (1 - trans_alpha[n66] * (1 - delta_prop[nnn]) - trans_delta[n66] * delta_prop[nnn]) +
       vaccine10[68] * (1 - trans_alpha[n67] * (1 - delta_prop[nnn]) - trans_delta[n67] * delta_prop[nnn]) +
       vaccine10[69] * (1 - trans_alpha[n68] * (1 - delta_prop[nnn]) - trans_delta[n68] * delta_prop[nnn]) +
       vaccine10[70] * (1 - trans_alpha[n69] * (1 - delta_prop[nnn]) - trans_delta[n69] * delta_prop[nnn]) +
       vaccine10[71] * (1 - trans_alpha[n70] * (1 - delta_prop[nnn]) - trans_delta[n70] * delta_prop[nnn]) +
       vaccine10[72] * (1 - trans_alpha[n71] * (1 - delta_prop[nnn]) - trans_delta[n71] * delta_prop[nnn]) +
       vaccine10[73] * (1 - trans_alpha[n72] * (1 - delta_prop[nnn]) - trans_delta[n72] * delta_prop[nnn]) +
       vaccine10[74] * (1 - trans_alpha[n73] * (1 - delta_prop[nnn]) - trans_delta[n73] * delta_prop[nnn]) +
       vaccine10[75] * (1 - trans_alpha[n74] * (1 - delta_prop[nnn]) - trans_delta[n74] * delta_prop[nnn]) +
       vaccine10[76] * (1 - trans_alpha[n75] * (1 - delta_prop[nnn]) - trans_delta[n75] * delta_prop[nnn]) +
       vaccine10[77] * (1 - trans_alpha[n76] * (1 - delta_prop[nnn]) - trans_delta[n76] * delta_prop[nnn]) +
       vaccine10[78] * (1 - trans_alpha[n77] * (1 - delta_prop[nnn]) - trans_delta[n77] * delta_prop[nnn]) +
       vaccine10[79] * (1 - trans_alpha[n78] * (1 - delta_prop[nnn]) - trans_delta[n78] * delta_prop[nnn]) +
       vaccine10[80] * (1 - trans_alpha[n79] * (1 - delta_prop[nnn]) - trans_delta[n79] * delta_prop[nnn]) +
       vaccine10[81] * (1 - trans_alpha[n80] * (1 - delta_prop[nnn]) - trans_delta[n80] * delta_prop[nnn]) +
       vaccine10[82] * (1 - trans_alpha[n81] * (1 - delta_prop[nnn]) - trans_delta[n81] * delta_prop[nnn]) +
       vaccine10[83] * (1 - trans_alpha[n82] * (1 - delta_prop[nnn]) - trans_delta[n82] * delta_prop[nnn]) +
       vaccine10[84] * (1 - trans_alpha[n83] * (1 - delta_prop[nnn]) - trans_delta[n83] * delta_prop[nnn]) +
       vaccine10[85] * (1 - trans_alpha[n84] * (1 - delta_prop[nnn]) - trans_delta[n84] * delta_prop[nnn]) +
       vaccine10[86] * (1 - trans_alpha[n85] * (1 - delta_prop[nnn]) - trans_delta[n85] * delta_prop[nnn]) +
       vaccine10[87] * (1 - trans_alpha[n86] * (1 - delta_prop[nnn]) - trans_delta[n86] * delta_prop[nnn]) +
       vaccine10[88] * (1 - trans_alpha[n87] * (1 - delta_prop[nnn]) - trans_delta[n87] * delta_prop[nnn]) +
       vaccine10[89] * (1 - trans_alpha[n88] * (1 - delta_prop[nnn]) - trans_delta[n88] * delta_prop[nnn]) +
       vaccine10[90] * (1 - trans_alpha[n89] * (1 - delta_prop[nnn]) - trans_delta[n89] * delta_prop[nnn]) +
       vaccine10[91] * (1 - trans_alpha[n90] * (1 - delta_prop[nnn]) - trans_delta[n90] * delta_prop[nnn]) +
       vaccine10[92] * (1 - trans_alpha[n91] * (1 - delta_prop[nnn]) - trans_delta[n91] * delta_prop[nnn]) +
       vaccine10[93] * (1 - trans_alpha[n92] * (1 - delta_prop[nnn]) - trans_delta[n92] * delta_prop[nnn]) +
       vaccine10[94] * (1 - trans_alpha[n93] * (1 - delta_prop[nnn]) - trans_delta[n93] * delta_prop[nnn]) +
       vaccine10[95] * (1 - trans_alpha[n94] * (1 - delta_prop[nnn]) - trans_delta[n94] * delta_prop[nnn]) +
       vaccine10[96] * (1 - trans_alpha[n95] * (1 - delta_prop[nnn]) - trans_delta[n95] * delta_prop[nnn]) +
       vaccine10[97] * (1 - trans_alpha[n96] * (1 - delta_prop[nnn]) - trans_delta[n96] * delta_prop[nnn]) +
       vaccine10[98] * (1 - trans_alpha[n97] * (1 - delta_prop[nnn]) - trans_delta[n97] * delta_prop[nnn]) +
       vaccine10[99] * (1 - trans_alpha[n98] * (1 - delta_prop[nnn]) - trans_delta[n98] * delta_prop[nnn]) +
       ifelse((1 - cum_vac10[nnn] - ((sum(new_case10[1:9]) + sum(pred10_sofar[10:nnn])) / population10)) < 0,
              0,
              1 - cum_vac10[nnn] - ((sum(new_case10[1:9]) + sum(pred10_sofar[10:nnn])) / population10)))
  
  
  poi20 <- pred20_sofar[nnn] *
    (vaccine20[1] *  (1 - trans_alpha[n00] * (1 - delta_prop[nnn]) - trans_delta[n00] * delta_prop[nnn]) +
       vaccine20[2] *  (1 - trans_alpha[n01] * (1 - delta_prop[nnn]) - trans_delta[n01] * delta_prop[nnn]) +
       vaccine20[3] *  (1 - trans_alpha[n02] * (1 - delta_prop[nnn]) - trans_delta[n02] * delta_prop[nnn]) +
       vaccine20[4] *  (1 - trans_alpha[n03] * (1 - delta_prop[nnn]) - trans_delta[n03] * delta_prop[nnn]) +
       vaccine20[5] *  (1 - trans_alpha[n04] * (1 - delta_prop[nnn]) - trans_delta[n04] * delta_prop[nnn]) +
       vaccine20[6] *  (1 - trans_alpha[n05] * (1 - delta_prop[nnn]) - trans_delta[n05] * delta_prop[nnn]) +
       vaccine20[7] *  (1 - trans_alpha[n06] * (1 - delta_prop[nnn]) - trans_delta[n06] * delta_prop[nnn]) +
       vaccine20[8] *  (1 - trans_alpha[n07] * (1 - delta_prop[nnn]) - trans_delta[n07] * delta_prop[nnn]) +
       vaccine20[9] *  (1 - trans_alpha[n08] * (1 - delta_prop[nnn]) - trans_delta[n08] * delta_prop[nnn]) +
       vaccine20[10] * (1 - trans_alpha[n09] * (1 - delta_prop[nnn]) - trans_delta[n09] * delta_prop[nnn]) +
       vaccine20[11] * (1 - trans_alpha[n10] * (1 - delta_prop[nnn]) - trans_delta[n10] * delta_prop[nnn]) +
       vaccine20[12] * (1 - trans_alpha[n11] * (1 - delta_prop[nnn]) - trans_delta[n11] * delta_prop[nnn]) +
       vaccine20[13] * (1 - trans_alpha[n12] * (1 - delta_prop[nnn]) - trans_delta[n12] * delta_prop[nnn]) +
       vaccine20[14] * (1 - trans_alpha[n13] * (1 - delta_prop[nnn]) - trans_delta[n13] * delta_prop[nnn]) +
       vaccine20[15] * (1 - trans_alpha[n14] * (1 - delta_prop[nnn]) - trans_delta[n14] * delta_prop[nnn]) +
       vaccine20[16] * (1 - trans_alpha[n15] * (1 - delta_prop[nnn]) - trans_delta[n15] * delta_prop[nnn]) +
       vaccine20[17] * (1 - trans_alpha[n16] * (1 - delta_prop[nnn]) - trans_delta[n16] * delta_prop[nnn]) +
       vaccine20[18] * (1 - trans_alpha[n17] * (1 - delta_prop[nnn]) - trans_delta[n17] * delta_prop[nnn]) +
       vaccine20[19] * (1 - trans_alpha[n18] * (1 - delta_prop[nnn]) - trans_delta[n18] * delta_prop[nnn]) +
       vaccine20[20] * (1 - trans_alpha[n19] * (1 - delta_prop[nnn]) - trans_delta[n19] * delta_prop[nnn]) +
       vaccine20[21] * (1 - trans_alpha[n20] * (1 - delta_prop[nnn]) - trans_delta[n20] * delta_prop[nnn]) +
       vaccine20[22] * (1 - trans_alpha[n21] * (1 - delta_prop[nnn]) - trans_delta[n21] * delta_prop[nnn]) +
       vaccine20[23] * (1 - trans_alpha[n22] * (1 - delta_prop[nnn]) - trans_delta[n22] * delta_prop[nnn]) +
       vaccine20[24] * (1 - trans_alpha[n23] * (1 - delta_prop[nnn]) - trans_delta[n23] * delta_prop[nnn]) +
       vaccine20[25] * (1 - trans_alpha[n24] * (1 - delta_prop[nnn]) - trans_delta[n24] * delta_prop[nnn]) +
       vaccine20[26] * (1 - trans_alpha[n25] * (1 - delta_prop[nnn]) - trans_delta[n25] * delta_prop[nnn]) +
       vaccine20[27] * (1 - trans_alpha[n26] * (1 - delta_prop[nnn]) - trans_delta[n26] * delta_prop[nnn]) +
       vaccine20[28] * (1 - trans_alpha[n27] * (1 - delta_prop[nnn]) - trans_delta[n27] * delta_prop[nnn]) +
       vaccine20[29] * (1 - trans_alpha[n28] * (1 - delta_prop[nnn]) - trans_delta[n28] * delta_prop[nnn]) +
       vaccine20[30] * (1 - trans_alpha[n29] * (1 - delta_prop[nnn]) - trans_delta[n29] * delta_prop[nnn]) +
       vaccine20[31] * (1 - trans_alpha[n30] * (1 - delta_prop[nnn]) - trans_delta[n30] * delta_prop[nnn]) +
       vaccine20[32] * (1 - trans_alpha[n31] * (1 - delta_prop[nnn]) - trans_delta[n31] * delta_prop[nnn]) +
       vaccine20[33] * (1 - trans_alpha[n32] * (1 - delta_prop[nnn]) - trans_delta[n32] * delta_prop[nnn]) +
       vaccine20[34] * (1 - trans_alpha[n33] * (1 - delta_prop[nnn]) - trans_delta[n33] * delta_prop[nnn]) +
       vaccine20[35] * (1 - trans_alpha[n34] * (1 - delta_prop[nnn]) - trans_delta[n34] * delta_prop[nnn]) +
       vaccine20[36] * (1 - trans_alpha[n35] * (1 - delta_prop[nnn]) - trans_delta[n35] * delta_prop[nnn]) +
       vaccine20[37] * (1 - trans_alpha[n36] * (1 - delta_prop[nnn]) - trans_delta[n36] * delta_prop[nnn]) +
       vaccine20[38] * (1 - trans_alpha[n37] * (1 - delta_prop[nnn]) - trans_delta[n37] * delta_prop[nnn]) +
       vaccine20[39] * (1 - trans_alpha[n38] * (1 - delta_prop[nnn]) - trans_delta[n38] * delta_prop[nnn]) +
       vaccine20[40] * (1 - trans_alpha[n39] * (1 - delta_prop[nnn]) - trans_delta[n39] * delta_prop[nnn]) +
       vaccine20[41] * (1 - trans_alpha[n40] * (1 - delta_prop[nnn]) - trans_delta[n40] * delta_prop[nnn]) +
       vaccine20[42] * (1 - trans_alpha[n41] * (1 - delta_prop[nnn]) - trans_delta[n41] * delta_prop[nnn]) +
       vaccine20[43] * (1 - trans_alpha[n42] * (1 - delta_prop[nnn]) - trans_delta[n42] * delta_prop[nnn]) +
       vaccine20[44] * (1 - trans_alpha[n43] * (1 - delta_prop[nnn]) - trans_delta[n43] * delta_prop[nnn]) +
       vaccine20[45] * (1 - trans_alpha[n44] * (1 - delta_prop[nnn]) - trans_delta[n44] * delta_prop[nnn]) +
       vaccine20[46] * (1 - trans_alpha[n45] * (1 - delta_prop[nnn]) - trans_delta[n45] * delta_prop[nnn]) +
       vaccine20[47] * (1 - trans_alpha[n46] * (1 - delta_prop[nnn]) - trans_delta[n46] * delta_prop[nnn]) +
       vaccine20[48] * (1 - trans_alpha[n47] * (1 - delta_prop[nnn]) - trans_delta[n47] * delta_prop[nnn]) +
       vaccine20[49] * (1 - trans_alpha[n48] * (1 - delta_prop[nnn]) - trans_delta[n48] * delta_prop[nnn]) +
       vaccine20[50] * (1 - trans_alpha[n49] * (1 - delta_prop[nnn]) - trans_delta[n49] * delta_prop[nnn]) +
       vaccine20[51] * (1 - trans_alpha[n50] * (1 - delta_prop[nnn]) - trans_delta[n50] * delta_prop[nnn]) +
       vaccine20[52] * (1 - trans_alpha[n51] * (1 - delta_prop[nnn]) - trans_delta[n51] * delta_prop[nnn]) +
       vaccine20[53] * (1 - trans_alpha[n52] * (1 - delta_prop[nnn]) - trans_delta[n52] * delta_prop[nnn]) +
       vaccine20[54] * (1 - trans_alpha[n53] * (1 - delta_prop[nnn]) - trans_delta[n53] * delta_prop[nnn]) +
       vaccine20[55] * (1 - trans_alpha[n54] * (1 - delta_prop[nnn]) - trans_delta[n54] * delta_prop[nnn]) +
       vaccine20[56] * (1 - trans_alpha[n55] * (1 - delta_prop[nnn]) - trans_delta[n55] * delta_prop[nnn]) +
       vaccine20[57] * (1 - trans_alpha[n56] * (1 - delta_prop[nnn]) - trans_delta[n56] * delta_prop[nnn]) +
       vaccine20[58] * (1 - trans_alpha[n57] * (1 - delta_prop[nnn]) - trans_delta[n57] * delta_prop[nnn]) +
       vaccine20[59] * (1 - trans_alpha[n58] * (1 - delta_prop[nnn]) - trans_delta[n58] * delta_prop[nnn]) +
       vaccine20[60] * (1 - trans_alpha[n59] * (1 - delta_prop[nnn]) - trans_delta[n59] * delta_prop[nnn]) +
       vaccine20[61] * (1 - trans_alpha[n60] * (1 - delta_prop[nnn]) - trans_delta[n60] * delta_prop[nnn]) +
       vaccine20[62] * (1 - trans_alpha[n61] * (1 - delta_prop[nnn]) - trans_delta[n61] * delta_prop[nnn]) +
       vaccine20[63] * (1 - trans_alpha[n62] * (1 - delta_prop[nnn]) - trans_delta[n62] * delta_prop[nnn]) +
       vaccine20[64] * (1 - trans_alpha[n63] * (1 - delta_prop[nnn]) - trans_delta[n63] * delta_prop[nnn]) +
       vaccine20[65] * (1 - trans_alpha[n64] * (1 - delta_prop[nnn]) - trans_delta[n64] * delta_prop[nnn]) +
       vaccine20[66] * (1 - trans_alpha[n65] * (1 - delta_prop[nnn]) - trans_delta[n65] * delta_prop[nnn]) +
       vaccine20[67] * (1 - trans_alpha[n66] * (1 - delta_prop[nnn]) - trans_delta[n66] * delta_prop[nnn]) +
       vaccine20[68] * (1 - trans_alpha[n67] * (1 - delta_prop[nnn]) - trans_delta[n67] * delta_prop[nnn]) +
       vaccine20[69] * (1 - trans_alpha[n68] * (1 - delta_prop[nnn]) - trans_delta[n68] * delta_prop[nnn]) +
       vaccine20[70] * (1 - trans_alpha[n69] * (1 - delta_prop[nnn]) - trans_delta[n69] * delta_prop[nnn]) +
       vaccine20[71] * (1 - trans_alpha[n70] * (1 - delta_prop[nnn]) - trans_delta[n70] * delta_prop[nnn]) +
       vaccine20[72] * (1 - trans_alpha[n71] * (1 - delta_prop[nnn]) - trans_delta[n71] * delta_prop[nnn]) +
       vaccine20[73] * (1 - trans_alpha[n72] * (1 - delta_prop[nnn]) - trans_delta[n72] * delta_prop[nnn]) +
       vaccine20[74] * (1 - trans_alpha[n73] * (1 - delta_prop[nnn]) - trans_delta[n73] * delta_prop[nnn]) +
       vaccine20[75] * (1 - trans_alpha[n74] * (1 - delta_prop[nnn]) - trans_delta[n74] * delta_prop[nnn]) +
       vaccine20[76] * (1 - trans_alpha[n75] * (1 - delta_prop[nnn]) - trans_delta[n75] * delta_prop[nnn]) +
       vaccine20[77] * (1 - trans_alpha[n76] * (1 - delta_prop[nnn]) - trans_delta[n76] * delta_prop[nnn]) +
       vaccine20[78] * (1 - trans_alpha[n77] * (1 - delta_prop[nnn]) - trans_delta[n77] * delta_prop[nnn]) +
       vaccine20[79] * (1 - trans_alpha[n78] * (1 - delta_prop[nnn]) - trans_delta[n78] * delta_prop[nnn]) +
       vaccine20[80] * (1 - trans_alpha[n79] * (1 - delta_prop[nnn]) - trans_delta[n79] * delta_prop[nnn]) +
       vaccine20[81] * (1 - trans_alpha[n80] * (1 - delta_prop[nnn]) - trans_delta[n80] * delta_prop[nnn]) +
       vaccine20[82] * (1 - trans_alpha[n81] * (1 - delta_prop[nnn]) - trans_delta[n81] * delta_prop[nnn]) +
       vaccine20[83] * (1 - trans_alpha[n82] * (1 - delta_prop[nnn]) - trans_delta[n82] * delta_prop[nnn]) +
       vaccine20[84] * (1 - trans_alpha[n83] * (1 - delta_prop[nnn]) - trans_delta[n83] * delta_prop[nnn]) +
       vaccine20[85] * (1 - trans_alpha[n84] * (1 - delta_prop[nnn]) - trans_delta[n84] * delta_prop[nnn]) +
       vaccine20[86] * (1 - trans_alpha[n85] * (1 - delta_prop[nnn]) - trans_delta[n85] * delta_prop[nnn]) +
       vaccine20[87] * (1 - trans_alpha[n86] * (1 - delta_prop[nnn]) - trans_delta[n86] * delta_prop[nnn]) +
       vaccine20[88] * (1 - trans_alpha[n87] * (1 - delta_prop[nnn]) - trans_delta[n87] * delta_prop[nnn]) +
       vaccine20[89] * (1 - trans_alpha[n88] * (1 - delta_prop[nnn]) - trans_delta[n88] * delta_prop[nnn]) +
       vaccine20[90] * (1 - trans_alpha[n89] * (1 - delta_prop[nnn]) - trans_delta[n89] * delta_prop[nnn]) +
       vaccine20[91] * (1 - trans_alpha[n90] * (1 - delta_prop[nnn]) - trans_delta[n90] * delta_prop[nnn]) +
       vaccine20[92] * (1 - trans_alpha[n91] * (1 - delta_prop[nnn]) - trans_delta[n91] * delta_prop[nnn]) +
       vaccine20[93] * (1 - trans_alpha[n92] * (1 - delta_prop[nnn]) - trans_delta[n92] * delta_prop[nnn]) +
       vaccine20[94] * (1 - trans_alpha[n93] * (1 - delta_prop[nnn]) - trans_delta[n93] * delta_prop[nnn]) +
       vaccine20[95] * (1 - trans_alpha[n94] * (1 - delta_prop[nnn]) - trans_delta[n94] * delta_prop[nnn]) +
       vaccine20[96] * (1 - trans_alpha[n95] * (1 - delta_prop[nnn]) - trans_delta[n95] * delta_prop[nnn]) +
       vaccine20[97] * (1 - trans_alpha[n96] * (1 - delta_prop[nnn]) - trans_delta[n96] * delta_prop[nnn]) +
       vaccine20[98] * (1 - trans_alpha[n97] * (1 - delta_prop[nnn]) - trans_delta[n97] * delta_prop[nnn]) +
       vaccine20[99] * (1 - trans_alpha[n98] * (1 - delta_prop[nnn]) - trans_delta[n98] * delta_prop[nnn]) +
       ifelse((1 - cum_vac20[nnn] - ((sum(new_case20[1:9]) + sum(pred20_sofar[10:nnn])) / population20)) < 0,
              0,
              1 - cum_vac20[nnn] - ((sum(new_case20[1:9]) + sum(pred20_sofar[10:nnn])) / population20)))
  
  poi30 <- pred30_sofar[nnn] *
    (vaccine30[1] *  (1 - trans_alpha[n00] * (1 - delta_prop[nnn]) - trans_delta[n00] * delta_prop[nnn]) +
       vaccine30[2] *  (1 - trans_alpha[n01] * (1 - delta_prop[nnn]) - trans_delta[n01] * delta_prop[nnn]) +
       vaccine30[3] *  (1 - trans_alpha[n02] * (1 - delta_prop[nnn]) - trans_delta[n02] * delta_prop[nnn]) +
       vaccine30[4] *  (1 - trans_alpha[n03] * (1 - delta_prop[nnn]) - trans_delta[n03] * delta_prop[nnn]) +
       vaccine30[5] *  (1 - trans_alpha[n04] * (1 - delta_prop[nnn]) - trans_delta[n04] * delta_prop[nnn]) +
       vaccine30[6] *  (1 - trans_alpha[n05] * (1 - delta_prop[nnn]) - trans_delta[n05] * delta_prop[nnn]) +
       vaccine30[7] *  (1 - trans_alpha[n06] * (1 - delta_prop[nnn]) - trans_delta[n06] * delta_prop[nnn]) +
       vaccine30[8] *  (1 - trans_alpha[n07] * (1 - delta_prop[nnn]) - trans_delta[n07] * delta_prop[nnn]) +
       vaccine30[9] *  (1 - trans_alpha[n08] * (1 - delta_prop[nnn]) - trans_delta[n08] * delta_prop[nnn]) +
       vaccine30[10] * (1 - trans_alpha[n09] * (1 - delta_prop[nnn]) - trans_delta[n09] * delta_prop[nnn]) +
       vaccine30[11] * (1 - trans_alpha[n10] * (1 - delta_prop[nnn]) - trans_delta[n10] * delta_prop[nnn]) +
       vaccine30[12] * (1 - trans_alpha[n11] * (1 - delta_prop[nnn]) - trans_delta[n11] * delta_prop[nnn]) +
       vaccine30[13] * (1 - trans_alpha[n12] * (1 - delta_prop[nnn]) - trans_delta[n12] * delta_prop[nnn]) +
       vaccine30[14] * (1 - trans_alpha[n13] * (1 - delta_prop[nnn]) - trans_delta[n13] * delta_prop[nnn]) +
       vaccine30[15] * (1 - trans_alpha[n14] * (1 - delta_prop[nnn]) - trans_delta[n14] * delta_prop[nnn]) +
       vaccine30[16] * (1 - trans_alpha[n15] * (1 - delta_prop[nnn]) - trans_delta[n15] * delta_prop[nnn]) +
       vaccine30[17] * (1 - trans_alpha[n16] * (1 - delta_prop[nnn]) - trans_delta[n16] * delta_prop[nnn]) +
       vaccine30[18] * (1 - trans_alpha[n17] * (1 - delta_prop[nnn]) - trans_delta[n17] * delta_prop[nnn]) +
       vaccine30[19] * (1 - trans_alpha[n18] * (1 - delta_prop[nnn]) - trans_delta[n18] * delta_prop[nnn]) +
       vaccine30[20] * (1 - trans_alpha[n19] * (1 - delta_prop[nnn]) - trans_delta[n19] * delta_prop[nnn]) +
       vaccine30[21] * (1 - trans_alpha[n20] * (1 - delta_prop[nnn]) - trans_delta[n20] * delta_prop[nnn]) +
       vaccine30[22] * (1 - trans_alpha[n21] * (1 - delta_prop[nnn]) - trans_delta[n21] * delta_prop[nnn]) +
       vaccine30[23] * (1 - trans_alpha[n22] * (1 - delta_prop[nnn]) - trans_delta[n22] * delta_prop[nnn]) +
       vaccine30[24] * (1 - trans_alpha[n23] * (1 - delta_prop[nnn]) - trans_delta[n23] * delta_prop[nnn]) +
       vaccine30[25] * (1 - trans_alpha[n24] * (1 - delta_prop[nnn]) - trans_delta[n24] * delta_prop[nnn]) +
       vaccine30[26] * (1 - trans_alpha[n25] * (1 - delta_prop[nnn]) - trans_delta[n25] * delta_prop[nnn]) +
       vaccine30[27] * (1 - trans_alpha[n26] * (1 - delta_prop[nnn]) - trans_delta[n26] * delta_prop[nnn]) +
       vaccine30[28] * (1 - trans_alpha[n27] * (1 - delta_prop[nnn]) - trans_delta[n27] * delta_prop[nnn]) +
       vaccine30[29] * (1 - trans_alpha[n28] * (1 - delta_prop[nnn]) - trans_delta[n28] * delta_prop[nnn]) +
       vaccine30[30] * (1 - trans_alpha[n29] * (1 - delta_prop[nnn]) - trans_delta[n29] * delta_prop[nnn]) +
       vaccine30[31] * (1 - trans_alpha[n30] * (1 - delta_prop[nnn]) - trans_delta[n30] * delta_prop[nnn]) +
       vaccine30[32] * (1 - trans_alpha[n31] * (1 - delta_prop[nnn]) - trans_delta[n31] * delta_prop[nnn]) +
       vaccine30[33] * (1 - trans_alpha[n32] * (1 - delta_prop[nnn]) - trans_delta[n32] * delta_prop[nnn]) +
       vaccine30[34] * (1 - trans_alpha[n33] * (1 - delta_prop[nnn]) - trans_delta[n33] * delta_prop[nnn]) +
       vaccine30[35] * (1 - trans_alpha[n34] * (1 - delta_prop[nnn]) - trans_delta[n34] * delta_prop[nnn]) +
       vaccine30[36] * (1 - trans_alpha[n35] * (1 - delta_prop[nnn]) - trans_delta[n35] * delta_prop[nnn]) +
       vaccine30[37] * (1 - trans_alpha[n36] * (1 - delta_prop[nnn]) - trans_delta[n36] * delta_prop[nnn]) +
       vaccine30[38] * (1 - trans_alpha[n37] * (1 - delta_prop[nnn]) - trans_delta[n37] * delta_prop[nnn]) +
       vaccine30[39] * (1 - trans_alpha[n38] * (1 - delta_prop[nnn]) - trans_delta[n38] * delta_prop[nnn]) +
       vaccine30[40] * (1 - trans_alpha[n39] * (1 - delta_prop[nnn]) - trans_delta[n39] * delta_prop[nnn]) +
       vaccine30[41] * (1 - trans_alpha[n40] * (1 - delta_prop[nnn]) - trans_delta[n40] * delta_prop[nnn]) +
       vaccine30[42] * (1 - trans_alpha[n41] * (1 - delta_prop[nnn]) - trans_delta[n41] * delta_prop[nnn]) +
       vaccine30[43] * (1 - trans_alpha[n42] * (1 - delta_prop[nnn]) - trans_delta[n42] * delta_prop[nnn]) +
       vaccine30[44] * (1 - trans_alpha[n43] * (1 - delta_prop[nnn]) - trans_delta[n43] * delta_prop[nnn]) +
       vaccine30[45] * (1 - trans_alpha[n44] * (1 - delta_prop[nnn]) - trans_delta[n44] * delta_prop[nnn]) +
       vaccine30[46] * (1 - trans_alpha[n45] * (1 - delta_prop[nnn]) - trans_delta[n45] * delta_prop[nnn]) +
       vaccine30[47] * (1 - trans_alpha[n46] * (1 - delta_prop[nnn]) - trans_delta[n46] * delta_prop[nnn]) +
       vaccine30[48] * (1 - trans_alpha[n47] * (1 - delta_prop[nnn]) - trans_delta[n47] * delta_prop[nnn]) +
       vaccine30[49] * (1 - trans_alpha[n48] * (1 - delta_prop[nnn]) - trans_delta[n48] * delta_prop[nnn]) +
       vaccine30[50] * (1 - trans_alpha[n49] * (1 - delta_prop[nnn]) - trans_delta[n49] * delta_prop[nnn]) +
       vaccine30[51] * (1 - trans_alpha[n50] * (1 - delta_prop[nnn]) - trans_delta[n50] * delta_prop[nnn]) +
       vaccine30[52] * (1 - trans_alpha[n51] * (1 - delta_prop[nnn]) - trans_delta[n51] * delta_prop[nnn]) +
       vaccine30[53] * (1 - trans_alpha[n52] * (1 - delta_prop[nnn]) - trans_delta[n52] * delta_prop[nnn]) +
       vaccine30[54] * (1 - trans_alpha[n53] * (1 - delta_prop[nnn]) - trans_delta[n53] * delta_prop[nnn]) +
       vaccine30[55] * (1 - trans_alpha[n54] * (1 - delta_prop[nnn]) - trans_delta[n54] * delta_prop[nnn]) +
       vaccine30[56] * (1 - trans_alpha[n55] * (1 - delta_prop[nnn]) - trans_delta[n55] * delta_prop[nnn]) +
       vaccine30[57] * (1 - trans_alpha[n56] * (1 - delta_prop[nnn]) - trans_delta[n56] * delta_prop[nnn]) +
       vaccine30[58] * (1 - trans_alpha[n57] * (1 - delta_prop[nnn]) - trans_delta[n57] * delta_prop[nnn]) +
       vaccine30[59] * (1 - trans_alpha[n58] * (1 - delta_prop[nnn]) - trans_delta[n58] * delta_prop[nnn]) +
       vaccine30[60] * (1 - trans_alpha[n59] * (1 - delta_prop[nnn]) - trans_delta[n59] * delta_prop[nnn]) +
       vaccine30[61] * (1 - trans_alpha[n60] * (1 - delta_prop[nnn]) - trans_delta[n60] * delta_prop[nnn]) +
       vaccine30[62] * (1 - trans_alpha[n61] * (1 - delta_prop[nnn]) - trans_delta[n61] * delta_prop[nnn]) +
       vaccine30[63] * (1 - trans_alpha[n62] * (1 - delta_prop[nnn]) - trans_delta[n62] * delta_prop[nnn]) +
       vaccine30[64] * (1 - trans_alpha[n63] * (1 - delta_prop[nnn]) - trans_delta[n63] * delta_prop[nnn]) +
       vaccine30[65] * (1 - trans_alpha[n64] * (1 - delta_prop[nnn]) - trans_delta[n64] * delta_prop[nnn]) +
       vaccine30[66] * (1 - trans_alpha[n65] * (1 - delta_prop[nnn]) - trans_delta[n65] * delta_prop[nnn]) +
       vaccine30[67] * (1 - trans_alpha[n66] * (1 - delta_prop[nnn]) - trans_delta[n66] * delta_prop[nnn]) +
       vaccine30[68] * (1 - trans_alpha[n67] * (1 - delta_prop[nnn]) - trans_delta[n67] * delta_prop[nnn]) +
       vaccine30[69] * (1 - trans_alpha[n68] * (1 - delta_prop[nnn]) - trans_delta[n68] * delta_prop[nnn]) +
       vaccine30[70] * (1 - trans_alpha[n69] * (1 - delta_prop[nnn]) - trans_delta[n69] * delta_prop[nnn]) +
       vaccine30[71] * (1 - trans_alpha[n70] * (1 - delta_prop[nnn]) - trans_delta[n70] * delta_prop[nnn]) +
       vaccine30[72] * (1 - trans_alpha[n71] * (1 - delta_prop[nnn]) - trans_delta[n71] * delta_prop[nnn]) +
       vaccine30[73] * (1 - trans_alpha[n72] * (1 - delta_prop[nnn]) - trans_delta[n72] * delta_prop[nnn]) +
       vaccine30[74] * (1 - trans_alpha[n73] * (1 - delta_prop[nnn]) - trans_delta[n73] * delta_prop[nnn]) +
       vaccine30[75] * (1 - trans_alpha[n74] * (1 - delta_prop[nnn]) - trans_delta[n74] * delta_prop[nnn]) +
       vaccine30[76] * (1 - trans_alpha[n75] * (1 - delta_prop[nnn]) - trans_delta[n75] * delta_prop[nnn]) +
       vaccine30[77] * (1 - trans_alpha[n76] * (1 - delta_prop[nnn]) - trans_delta[n76] * delta_prop[nnn]) +
       vaccine30[78] * (1 - trans_alpha[n77] * (1 - delta_prop[nnn]) - trans_delta[n77] * delta_prop[nnn]) +
       vaccine30[79] * (1 - trans_alpha[n78] * (1 - delta_prop[nnn]) - trans_delta[n78] * delta_prop[nnn]) +
       vaccine30[80] * (1 - trans_alpha[n79] * (1 - delta_prop[nnn]) - trans_delta[n79] * delta_prop[nnn]) +
       vaccine30[81] * (1 - trans_alpha[n80] * (1 - delta_prop[nnn]) - trans_delta[n80] * delta_prop[nnn]) +
       vaccine30[82] * (1 - trans_alpha[n81] * (1 - delta_prop[nnn]) - trans_delta[n81] * delta_prop[nnn]) +
       vaccine30[83] * (1 - trans_alpha[n82] * (1 - delta_prop[nnn]) - trans_delta[n82] * delta_prop[nnn]) +
       vaccine30[84] * (1 - trans_alpha[n83] * (1 - delta_prop[nnn]) - trans_delta[n83] * delta_prop[nnn]) +
       vaccine30[85] * (1 - trans_alpha[n84] * (1 - delta_prop[nnn]) - trans_delta[n84] * delta_prop[nnn]) +
       vaccine30[86] * (1 - trans_alpha[n85] * (1 - delta_prop[nnn]) - trans_delta[n85] * delta_prop[nnn]) +
       vaccine30[87] * (1 - trans_alpha[n86] * (1 - delta_prop[nnn]) - trans_delta[n86] * delta_prop[nnn]) +
       vaccine30[88] * (1 - trans_alpha[n87] * (1 - delta_prop[nnn]) - trans_delta[n87] * delta_prop[nnn]) +
       vaccine30[89] * (1 - trans_alpha[n88] * (1 - delta_prop[nnn]) - trans_delta[n88] * delta_prop[nnn]) +
       vaccine30[90] * (1 - trans_alpha[n89] * (1 - delta_prop[nnn]) - trans_delta[n89] * delta_prop[nnn]) +
       vaccine30[91] * (1 - trans_alpha[n90] * (1 - delta_prop[nnn]) - trans_delta[n90] * delta_prop[nnn]) +
       vaccine30[92] * (1 - trans_alpha[n91] * (1 - delta_prop[nnn]) - trans_delta[n91] * delta_prop[nnn]) +
       vaccine30[93] * (1 - trans_alpha[n92] * (1 - delta_prop[nnn]) - trans_delta[n92] * delta_prop[nnn]) +
       vaccine30[94] * (1 - trans_alpha[n93] * (1 - delta_prop[nnn]) - trans_delta[n93] * delta_prop[nnn]) +
       vaccine30[95] * (1 - trans_alpha[n94] * (1 - delta_prop[nnn]) - trans_delta[n94] * delta_prop[nnn]) +
       vaccine30[96] * (1 - trans_alpha[n95] * (1 - delta_prop[nnn]) - trans_delta[n95] * delta_prop[nnn]) +
       vaccine30[97] * (1 - trans_alpha[n96] * (1 - delta_prop[nnn]) - trans_delta[n96] * delta_prop[nnn]) +
       vaccine30[98] * (1 - trans_alpha[n97] * (1 - delta_prop[nnn]) - trans_delta[n97] * delta_prop[nnn]) +
       vaccine30[99] * (1 - trans_alpha[n98] * (1 - delta_prop[nnn]) - trans_delta[n98] * delta_prop[nnn]) +
       ifelse((1 - cum_vac30[nnn] - ((sum(new_case30[1:9]) + sum(pred30_sofar[10:nnn])) / population30)) < 0,
              0,
              1 - cum_vac30[nnn] - ((sum(new_case30[1:9]) + sum(pred30_sofar[10:nnn])) / population30)))
  
  poi40 <- pred40_sofar[nnn] *
    (vaccine40[1] *  (1 - trans_alpha[n00] * (1 - delta_prop[nnn]) - trans_delta[n00] * delta_prop[nnn]) +
       vaccine40[2] *  (1 - trans_alpha[n01] * (1 - delta_prop[nnn]) - trans_delta[n01] * delta_prop[nnn]) +
       vaccine40[3] *  (1 - trans_alpha[n02] * (1 - delta_prop[nnn]) - trans_delta[n02] * delta_prop[nnn]) +
       vaccine40[4] *  (1 - trans_alpha[n03] * (1 - delta_prop[nnn]) - trans_delta[n03] * delta_prop[nnn]) +
       vaccine40[5] *  (1 - trans_alpha[n04] * (1 - delta_prop[nnn]) - trans_delta[n04] * delta_prop[nnn]) +
       vaccine40[6] *  (1 - trans_alpha[n05] * (1 - delta_prop[nnn]) - trans_delta[n05] * delta_prop[nnn]) +
       vaccine40[7] *  (1 - trans_alpha[n06] * (1 - delta_prop[nnn]) - trans_delta[n06] * delta_prop[nnn]) +
       vaccine40[8] *  (1 - trans_alpha[n07] * (1 - delta_prop[nnn]) - trans_delta[n07] * delta_prop[nnn]) +
       vaccine40[9] *  (1 - trans_alpha[n08] * (1 - delta_prop[nnn]) - trans_delta[n08] * delta_prop[nnn]) +
       vaccine40[10] * (1 - trans_alpha[n09] * (1 - delta_prop[nnn]) - trans_delta[n09] * delta_prop[nnn]) +
       vaccine40[11] * (1 - trans_alpha[n10] * (1 - delta_prop[nnn]) - trans_delta[n10] * delta_prop[nnn]) +
       vaccine40[12] * (1 - trans_alpha[n11] * (1 - delta_prop[nnn]) - trans_delta[n11] * delta_prop[nnn]) +
       vaccine40[13] * (1 - trans_alpha[n12] * (1 - delta_prop[nnn]) - trans_delta[n12] * delta_prop[nnn]) +
       vaccine40[14] * (1 - trans_alpha[n13] * (1 - delta_prop[nnn]) - trans_delta[n13] * delta_prop[nnn]) +
       vaccine40[15] * (1 - trans_alpha[n14] * (1 - delta_prop[nnn]) - trans_delta[n14] * delta_prop[nnn]) +
       vaccine40[16] * (1 - trans_alpha[n15] * (1 - delta_prop[nnn]) - trans_delta[n15] * delta_prop[nnn]) +
       vaccine40[17] * (1 - trans_alpha[n16] * (1 - delta_prop[nnn]) - trans_delta[n16] * delta_prop[nnn]) +
       vaccine40[18] * (1 - trans_alpha[n17] * (1 - delta_prop[nnn]) - trans_delta[n17] * delta_prop[nnn]) +
       vaccine40[19] * (1 - trans_alpha[n18] * (1 - delta_prop[nnn]) - trans_delta[n18] * delta_prop[nnn]) +
       vaccine40[20] * (1 - trans_alpha[n19] * (1 - delta_prop[nnn]) - trans_delta[n19] * delta_prop[nnn]) +
       vaccine40[21] * (1 - trans_alpha[n20] * (1 - delta_prop[nnn]) - trans_delta[n20] * delta_prop[nnn]) +
       vaccine40[22] * (1 - trans_alpha[n21] * (1 - delta_prop[nnn]) - trans_delta[n21] * delta_prop[nnn]) +
       vaccine40[23] * (1 - trans_alpha[n22] * (1 - delta_prop[nnn]) - trans_delta[n22] * delta_prop[nnn]) +
       vaccine40[24] * (1 - trans_alpha[n23] * (1 - delta_prop[nnn]) - trans_delta[n23] * delta_prop[nnn]) +
       vaccine40[25] * (1 - trans_alpha[n24] * (1 - delta_prop[nnn]) - trans_delta[n24] * delta_prop[nnn]) +
       vaccine40[26] * (1 - trans_alpha[n25] * (1 - delta_prop[nnn]) - trans_delta[n25] * delta_prop[nnn]) +
       vaccine40[27] * (1 - trans_alpha[n26] * (1 - delta_prop[nnn]) - trans_delta[n26] * delta_prop[nnn]) +
       vaccine40[28] * (1 - trans_alpha[n27] * (1 - delta_prop[nnn]) - trans_delta[n27] * delta_prop[nnn]) +
       vaccine40[29] * (1 - trans_alpha[n28] * (1 - delta_prop[nnn]) - trans_delta[n28] * delta_prop[nnn]) +
       vaccine40[30] * (1 - trans_alpha[n29] * (1 - delta_prop[nnn]) - trans_delta[n29] * delta_prop[nnn]) +
       vaccine40[31] * (1 - trans_alpha[n30] * (1 - delta_prop[nnn]) - trans_delta[n30] * delta_prop[nnn]) +
       vaccine40[32] * (1 - trans_alpha[n31] * (1 - delta_prop[nnn]) - trans_delta[n31] * delta_prop[nnn]) +
       vaccine40[33] * (1 - trans_alpha[n32] * (1 - delta_prop[nnn]) - trans_delta[n32] * delta_prop[nnn]) +
       vaccine40[34] * (1 - trans_alpha[n33] * (1 - delta_prop[nnn]) - trans_delta[n33] * delta_prop[nnn]) +
       vaccine40[35] * (1 - trans_alpha[n34] * (1 - delta_prop[nnn]) - trans_delta[n34] * delta_prop[nnn]) +
       vaccine40[36] * (1 - trans_alpha[n35] * (1 - delta_prop[nnn]) - trans_delta[n35] * delta_prop[nnn]) +
       vaccine40[37] * (1 - trans_alpha[n36] * (1 - delta_prop[nnn]) - trans_delta[n36] * delta_prop[nnn]) +
       vaccine40[38] * (1 - trans_alpha[n37] * (1 - delta_prop[nnn]) - trans_delta[n37] * delta_prop[nnn]) +
       vaccine40[39] * (1 - trans_alpha[n38] * (1 - delta_prop[nnn]) - trans_delta[n38] * delta_prop[nnn]) +
       vaccine40[40] * (1 - trans_alpha[n39] * (1 - delta_prop[nnn]) - trans_delta[n39] * delta_prop[nnn]) +
       vaccine40[41] * (1 - trans_alpha[n40] * (1 - delta_prop[nnn]) - trans_delta[n40] * delta_prop[nnn]) +
       vaccine40[42] * (1 - trans_alpha[n41] * (1 - delta_prop[nnn]) - trans_delta[n41] * delta_prop[nnn]) +
       vaccine40[43] * (1 - trans_alpha[n42] * (1 - delta_prop[nnn]) - trans_delta[n42] * delta_prop[nnn]) +
       vaccine40[44] * (1 - trans_alpha[n43] * (1 - delta_prop[nnn]) - trans_delta[n43] * delta_prop[nnn]) +
       vaccine40[45] * (1 - trans_alpha[n44] * (1 - delta_prop[nnn]) - trans_delta[n44] * delta_prop[nnn]) +
       vaccine40[46] * (1 - trans_alpha[n45] * (1 - delta_prop[nnn]) - trans_delta[n45] * delta_prop[nnn]) +
       vaccine40[47] * (1 - trans_alpha[n46] * (1 - delta_prop[nnn]) - trans_delta[n46] * delta_prop[nnn]) +
       vaccine40[48] * (1 - trans_alpha[n47] * (1 - delta_prop[nnn]) - trans_delta[n47] * delta_prop[nnn]) +
       vaccine40[49] * (1 - trans_alpha[n48] * (1 - delta_prop[nnn]) - trans_delta[n48] * delta_prop[nnn]) +
       vaccine40[50] * (1 - trans_alpha[n49] * (1 - delta_prop[nnn]) - trans_delta[n49] * delta_prop[nnn]) +
       vaccine40[51] * (1 - trans_alpha[n50] * (1 - delta_prop[nnn]) - trans_delta[n50] * delta_prop[nnn]) +
       vaccine40[52] * (1 - trans_alpha[n51] * (1 - delta_prop[nnn]) - trans_delta[n51] * delta_prop[nnn]) +
       vaccine40[53] * (1 - trans_alpha[n52] * (1 - delta_prop[nnn]) - trans_delta[n52] * delta_prop[nnn]) +
       vaccine40[54] * (1 - trans_alpha[n53] * (1 - delta_prop[nnn]) - trans_delta[n53] * delta_prop[nnn]) +
       vaccine40[55] * (1 - trans_alpha[n54] * (1 - delta_prop[nnn]) - trans_delta[n54] * delta_prop[nnn]) +
       vaccine40[56] * (1 - trans_alpha[n55] * (1 - delta_prop[nnn]) - trans_delta[n55] * delta_prop[nnn]) +
       vaccine40[57] * (1 - trans_alpha[n56] * (1 - delta_prop[nnn]) - trans_delta[n56] * delta_prop[nnn]) +
       vaccine40[58] * (1 - trans_alpha[n57] * (1 - delta_prop[nnn]) - trans_delta[n57] * delta_prop[nnn]) +
       vaccine40[59] * (1 - trans_alpha[n58] * (1 - delta_prop[nnn]) - trans_delta[n58] * delta_prop[nnn]) +
       vaccine40[60] * (1 - trans_alpha[n59] * (1 - delta_prop[nnn]) - trans_delta[n59] * delta_prop[nnn]) +
       vaccine40[61] * (1 - trans_alpha[n60] * (1 - delta_prop[nnn]) - trans_delta[n60] * delta_prop[nnn]) +
       vaccine40[62] * (1 - trans_alpha[n61] * (1 - delta_prop[nnn]) - trans_delta[n61] * delta_prop[nnn]) +
       vaccine40[63] * (1 - trans_alpha[n62] * (1 - delta_prop[nnn]) - trans_delta[n62] * delta_prop[nnn]) +
       vaccine40[64] * (1 - trans_alpha[n63] * (1 - delta_prop[nnn]) - trans_delta[n63] * delta_prop[nnn]) +
       vaccine40[65] * (1 - trans_alpha[n64] * (1 - delta_prop[nnn]) - trans_delta[n64] * delta_prop[nnn]) +
       vaccine40[66] * (1 - trans_alpha[n65] * (1 - delta_prop[nnn]) - trans_delta[n65] * delta_prop[nnn]) +
       vaccine40[67] * (1 - trans_alpha[n66] * (1 - delta_prop[nnn]) - trans_delta[n66] * delta_prop[nnn]) +
       vaccine40[68] * (1 - trans_alpha[n67] * (1 - delta_prop[nnn]) - trans_delta[n67] * delta_prop[nnn]) +
       vaccine40[69] * (1 - trans_alpha[n68] * (1 - delta_prop[nnn]) - trans_delta[n68] * delta_prop[nnn]) +
       vaccine40[70] * (1 - trans_alpha[n69] * (1 - delta_prop[nnn]) - trans_delta[n69] * delta_prop[nnn]) +
       vaccine40[71] * (1 - trans_alpha[n70] * (1 - delta_prop[nnn]) - trans_delta[n70] * delta_prop[nnn]) +
       vaccine40[72] * (1 - trans_alpha[n71] * (1 - delta_prop[nnn]) - trans_delta[n71] * delta_prop[nnn]) +
       vaccine40[73] * (1 - trans_alpha[n72] * (1 - delta_prop[nnn]) - trans_delta[n72] * delta_prop[nnn]) +
       vaccine40[74] * (1 - trans_alpha[n73] * (1 - delta_prop[nnn]) - trans_delta[n73] * delta_prop[nnn]) +
       vaccine40[75] * (1 - trans_alpha[n74] * (1 - delta_prop[nnn]) - trans_delta[n74] * delta_prop[nnn]) +
       vaccine40[76] * (1 - trans_alpha[n75] * (1 - delta_prop[nnn]) - trans_delta[n75] * delta_prop[nnn]) +
       vaccine40[77] * (1 - trans_alpha[n76] * (1 - delta_prop[nnn]) - trans_delta[n76] * delta_prop[nnn]) +
       vaccine40[78] * (1 - trans_alpha[n77] * (1 - delta_prop[nnn]) - trans_delta[n77] * delta_prop[nnn]) +
       vaccine40[79] * (1 - trans_alpha[n78] * (1 - delta_prop[nnn]) - trans_delta[n78] * delta_prop[nnn]) +
       vaccine40[80] * (1 - trans_alpha[n79] * (1 - delta_prop[nnn]) - trans_delta[n79] * delta_prop[nnn]) +
       vaccine40[81] * (1 - trans_alpha[n80] * (1 - delta_prop[nnn]) - trans_delta[n80] * delta_prop[nnn]) +
       vaccine40[82] * (1 - trans_alpha[n81] * (1 - delta_prop[nnn]) - trans_delta[n81] * delta_prop[nnn]) +
       vaccine40[83] * (1 - trans_alpha[n82] * (1 - delta_prop[nnn]) - trans_delta[n82] * delta_prop[nnn]) +
       vaccine40[84] * (1 - trans_alpha[n83] * (1 - delta_prop[nnn]) - trans_delta[n83] * delta_prop[nnn]) +
       vaccine40[85] * (1 - trans_alpha[n84] * (1 - delta_prop[nnn]) - trans_delta[n84] * delta_prop[nnn]) +
       vaccine40[86] * (1 - trans_alpha[n85] * (1 - delta_prop[nnn]) - trans_delta[n85] * delta_prop[nnn]) +
       vaccine40[87] * (1 - trans_alpha[n86] * (1 - delta_prop[nnn]) - trans_delta[n86] * delta_prop[nnn]) +
       vaccine40[88] * (1 - trans_alpha[n87] * (1 - delta_prop[nnn]) - trans_delta[n87] * delta_prop[nnn]) +
       vaccine40[89] * (1 - trans_alpha[n88] * (1 - delta_prop[nnn]) - trans_delta[n88] * delta_prop[nnn]) +
       vaccine40[90] * (1 - trans_alpha[n89] * (1 - delta_prop[nnn]) - trans_delta[n89] * delta_prop[nnn]) +
       vaccine40[91] * (1 - trans_alpha[n90] * (1 - delta_prop[nnn]) - trans_delta[n90] * delta_prop[nnn]) +
       vaccine40[92] * (1 - trans_alpha[n91] * (1 - delta_prop[nnn]) - trans_delta[n91] * delta_prop[nnn]) +
       vaccine40[93] * (1 - trans_alpha[n92] * (1 - delta_prop[nnn]) - trans_delta[n92] * delta_prop[nnn]) +
       vaccine40[94] * (1 - trans_alpha[n93] * (1 - delta_prop[nnn]) - trans_delta[n93] * delta_prop[nnn]) +
       vaccine40[95] * (1 - trans_alpha[n94] * (1 - delta_prop[nnn]) - trans_delta[n94] * delta_prop[nnn]) +
       vaccine40[96] * (1 - trans_alpha[n95] * (1 - delta_prop[nnn]) - trans_delta[n95] * delta_prop[nnn]) +
       vaccine40[97] * (1 - trans_alpha[n96] * (1 - delta_prop[nnn]) - trans_delta[n96] * delta_prop[nnn]) +
       vaccine40[98] * (1 - trans_alpha[n97] * (1 - delta_prop[nnn]) - trans_delta[n97] * delta_prop[nnn]) +
       vaccine40[99] * (1 - trans_alpha[n98] * (1 - delta_prop[nnn]) - trans_delta[n98] * delta_prop[nnn]) +
       ifelse((1 - cum_vac40[nnn] - ((sum(new_case40[1:9]) + sum(pred40_sofar[10:nnn])) / population40)) < 0,
              0,
              1 - cum_vac40[nnn] - ((sum(new_case40[1:9]) + sum(pred40_sofar[10:nnn])) / population40)))
  
  poi50 <- pred50_sofar[nnn] *
    (vaccine50[1] *  (1 - trans_alpha[n00] * (1 - delta_prop[nnn]) - trans_delta[n00] * delta_prop[nnn]) +
       vaccine50[2] *  (1 - trans_alpha[n01] * (1 - delta_prop[nnn]) - trans_delta[n01] * delta_prop[nnn]) +
       vaccine50[3] *  (1 - trans_alpha[n02] * (1 - delta_prop[nnn]) - trans_delta[n02] * delta_prop[nnn]) +
       vaccine50[4] *  (1 - trans_alpha[n03] * (1 - delta_prop[nnn]) - trans_delta[n03] * delta_prop[nnn]) +
       vaccine50[5] *  (1 - trans_alpha[n04] * (1 - delta_prop[nnn]) - trans_delta[n04] * delta_prop[nnn]) +
       vaccine50[6] *  (1 - trans_alpha[n05] * (1 - delta_prop[nnn]) - trans_delta[n05] * delta_prop[nnn]) +
       vaccine50[7] *  (1 - trans_alpha[n06] * (1 - delta_prop[nnn]) - trans_delta[n06] * delta_prop[nnn]) +
       vaccine50[8] *  (1 - trans_alpha[n07] * (1 - delta_prop[nnn]) - trans_delta[n07] * delta_prop[nnn]) +
       vaccine50[9] *  (1 - trans_alpha[n08] * (1 - delta_prop[nnn]) - trans_delta[n08] * delta_prop[nnn]) +
       vaccine50[10] * (1 - trans_alpha[n09] * (1 - delta_prop[nnn]) - trans_delta[n09] * delta_prop[nnn]) +
       vaccine50[11] * (1 - trans_alpha[n10] * (1 - delta_prop[nnn]) - trans_delta[n10] * delta_prop[nnn]) +
       vaccine50[12] * (1 - trans_alpha[n11] * (1 - delta_prop[nnn]) - trans_delta[n11] * delta_prop[nnn]) +
       vaccine50[13] * (1 - trans_alpha[n12] * (1 - delta_prop[nnn]) - trans_delta[n12] * delta_prop[nnn]) +
       vaccine50[14] * (1 - trans_alpha[n13] * (1 - delta_prop[nnn]) - trans_delta[n13] * delta_prop[nnn]) +
       vaccine50[15] * (1 - trans_alpha[n14] * (1 - delta_prop[nnn]) - trans_delta[n14] * delta_prop[nnn]) +
       vaccine50[16] * (1 - trans_alpha[n15] * (1 - delta_prop[nnn]) - trans_delta[n15] * delta_prop[nnn]) +
       vaccine50[17] * (1 - trans_alpha[n16] * (1 - delta_prop[nnn]) - trans_delta[n16] * delta_prop[nnn]) +
       vaccine50[18] * (1 - trans_alpha[n17] * (1 - delta_prop[nnn]) - trans_delta[n17] * delta_prop[nnn]) +
       vaccine50[19] * (1 - trans_alpha[n18] * (1 - delta_prop[nnn]) - trans_delta[n18] * delta_prop[nnn]) +
       vaccine50[20] * (1 - trans_alpha[n19] * (1 - delta_prop[nnn]) - trans_delta[n19] * delta_prop[nnn]) +
       vaccine50[21] * (1 - trans_alpha[n20] * (1 - delta_prop[nnn]) - trans_delta[n20] * delta_prop[nnn]) +
       vaccine50[22] * (1 - trans_alpha[n21] * (1 - delta_prop[nnn]) - trans_delta[n21] * delta_prop[nnn]) +
       vaccine50[23] * (1 - trans_alpha[n22] * (1 - delta_prop[nnn]) - trans_delta[n22] * delta_prop[nnn]) +
       vaccine50[24] * (1 - trans_alpha[n23] * (1 - delta_prop[nnn]) - trans_delta[n23] * delta_prop[nnn]) +
       vaccine50[25] * (1 - trans_alpha[n24] * (1 - delta_prop[nnn]) - trans_delta[n24] * delta_prop[nnn]) +
       vaccine50[26] * (1 - trans_alpha[n25] * (1 - delta_prop[nnn]) - trans_delta[n25] * delta_prop[nnn]) +
       vaccine50[27] * (1 - trans_alpha[n26] * (1 - delta_prop[nnn]) - trans_delta[n26] * delta_prop[nnn]) +
       vaccine50[28] * (1 - trans_alpha[n27] * (1 - delta_prop[nnn]) - trans_delta[n27] * delta_prop[nnn]) +
       vaccine50[29] * (1 - trans_alpha[n28] * (1 - delta_prop[nnn]) - trans_delta[n28] * delta_prop[nnn]) +
       vaccine50[30] * (1 - trans_alpha[n29] * (1 - delta_prop[nnn]) - trans_delta[n29] * delta_prop[nnn]) +
       vaccine50[31] * (1 - trans_alpha[n30] * (1 - delta_prop[nnn]) - trans_delta[n30] * delta_prop[nnn]) +
       vaccine50[32] * (1 - trans_alpha[n31] * (1 - delta_prop[nnn]) - trans_delta[n31] * delta_prop[nnn]) +
       vaccine50[33] * (1 - trans_alpha[n32] * (1 - delta_prop[nnn]) - trans_delta[n32] * delta_prop[nnn]) +
       vaccine50[34] * (1 - trans_alpha[n33] * (1 - delta_prop[nnn]) - trans_delta[n33] * delta_prop[nnn]) +
       vaccine50[35] * (1 - trans_alpha[n34] * (1 - delta_prop[nnn]) - trans_delta[n34] * delta_prop[nnn]) +
       vaccine50[36] * (1 - trans_alpha[n35] * (1 - delta_prop[nnn]) - trans_delta[n35] * delta_prop[nnn]) +
       vaccine50[37] * (1 - trans_alpha[n36] * (1 - delta_prop[nnn]) - trans_delta[n36] * delta_prop[nnn]) +
       vaccine50[38] * (1 - trans_alpha[n37] * (1 - delta_prop[nnn]) - trans_delta[n37] * delta_prop[nnn]) +
       vaccine50[39] * (1 - trans_alpha[n38] * (1 - delta_prop[nnn]) - trans_delta[n38] * delta_prop[nnn]) +
       vaccine50[40] * (1 - trans_alpha[n39] * (1 - delta_prop[nnn]) - trans_delta[n39] * delta_prop[nnn]) +
       vaccine50[41] * (1 - trans_alpha[n40] * (1 - delta_prop[nnn]) - trans_delta[n40] * delta_prop[nnn]) +
       vaccine50[42] * (1 - trans_alpha[n41] * (1 - delta_prop[nnn]) - trans_delta[n41] * delta_prop[nnn]) +
       vaccine50[43] * (1 - trans_alpha[n42] * (1 - delta_prop[nnn]) - trans_delta[n42] * delta_prop[nnn]) +
       vaccine50[44] * (1 - trans_alpha[n43] * (1 - delta_prop[nnn]) - trans_delta[n43] * delta_prop[nnn]) +
       vaccine50[45] * (1 - trans_alpha[n44] * (1 - delta_prop[nnn]) - trans_delta[n44] * delta_prop[nnn]) +
       vaccine50[46] * (1 - trans_alpha[n45] * (1 - delta_prop[nnn]) - trans_delta[n45] * delta_prop[nnn]) +
       vaccine50[47] * (1 - trans_alpha[n46] * (1 - delta_prop[nnn]) - trans_delta[n46] * delta_prop[nnn]) +
       vaccine50[48] * (1 - trans_alpha[n47] * (1 - delta_prop[nnn]) - trans_delta[n47] * delta_prop[nnn]) +
       vaccine50[49] * (1 - trans_alpha[n48] * (1 - delta_prop[nnn]) - trans_delta[n48] * delta_prop[nnn]) +
       vaccine50[50] * (1 - trans_alpha[n49] * (1 - delta_prop[nnn]) - trans_delta[n49] * delta_prop[nnn]) +
       vaccine50[51] * (1 - trans_alpha[n50] * (1 - delta_prop[nnn]) - trans_delta[n50] * delta_prop[nnn]) +
       vaccine50[52] * (1 - trans_alpha[n51] * (1 - delta_prop[nnn]) - trans_delta[n51] * delta_prop[nnn]) +
       vaccine50[53] * (1 - trans_alpha[n52] * (1 - delta_prop[nnn]) - trans_delta[n52] * delta_prop[nnn]) +
       vaccine50[54] * (1 - trans_alpha[n53] * (1 - delta_prop[nnn]) - trans_delta[n53] * delta_prop[nnn]) +
       vaccine50[55] * (1 - trans_alpha[n54] * (1 - delta_prop[nnn]) - trans_delta[n54] * delta_prop[nnn]) +
       vaccine50[56] * (1 - trans_alpha[n55] * (1 - delta_prop[nnn]) - trans_delta[n55] * delta_prop[nnn]) +
       vaccine50[57] * (1 - trans_alpha[n56] * (1 - delta_prop[nnn]) - trans_delta[n56] * delta_prop[nnn]) +
       vaccine50[58] * (1 - trans_alpha[n57] * (1 - delta_prop[nnn]) - trans_delta[n57] * delta_prop[nnn]) +
       vaccine50[59] * (1 - trans_alpha[n58] * (1 - delta_prop[nnn]) - trans_delta[n58] * delta_prop[nnn]) +
       vaccine50[60] * (1 - trans_alpha[n59] * (1 - delta_prop[nnn]) - trans_delta[n59] * delta_prop[nnn]) +
       vaccine50[61] * (1 - trans_alpha[n60] * (1 - delta_prop[nnn]) - trans_delta[n60] * delta_prop[nnn]) +
       vaccine50[62] * (1 - trans_alpha[n61] * (1 - delta_prop[nnn]) - trans_delta[n61] * delta_prop[nnn]) +
       vaccine50[63] * (1 - trans_alpha[n62] * (1 - delta_prop[nnn]) - trans_delta[n62] * delta_prop[nnn]) +
       vaccine50[64] * (1 - trans_alpha[n63] * (1 - delta_prop[nnn]) - trans_delta[n63] * delta_prop[nnn]) +
       vaccine50[65] * (1 - trans_alpha[n64] * (1 - delta_prop[nnn]) - trans_delta[n64] * delta_prop[nnn]) +
       vaccine50[66] * (1 - trans_alpha[n65] * (1 - delta_prop[nnn]) - trans_delta[n65] * delta_prop[nnn]) +
       vaccine50[67] * (1 - trans_alpha[n66] * (1 - delta_prop[nnn]) - trans_delta[n66] * delta_prop[nnn]) +
       vaccine50[68] * (1 - trans_alpha[n67] * (1 - delta_prop[nnn]) - trans_delta[n67] * delta_prop[nnn]) +
       vaccine50[69] * (1 - trans_alpha[n68] * (1 - delta_prop[nnn]) - trans_delta[n68] * delta_prop[nnn]) +
       vaccine50[70] * (1 - trans_alpha[n69] * (1 - delta_prop[nnn]) - trans_delta[n69] * delta_prop[nnn]) +
       vaccine50[71] * (1 - trans_alpha[n70] * (1 - delta_prop[nnn]) - trans_delta[n70] * delta_prop[nnn]) +
       vaccine50[72] * (1 - trans_alpha[n71] * (1 - delta_prop[nnn]) - trans_delta[n71] * delta_prop[nnn]) +
       vaccine50[73] * (1 - trans_alpha[n72] * (1 - delta_prop[nnn]) - trans_delta[n72] * delta_prop[nnn]) +
       vaccine50[74] * (1 - trans_alpha[n73] * (1 - delta_prop[nnn]) - trans_delta[n73] * delta_prop[nnn]) +
       vaccine50[75] * (1 - trans_alpha[n74] * (1 - delta_prop[nnn]) - trans_delta[n74] * delta_prop[nnn]) +
       vaccine50[76] * (1 - trans_alpha[n75] * (1 - delta_prop[nnn]) - trans_delta[n75] * delta_prop[nnn]) +
       vaccine50[77] * (1 - trans_alpha[n76] * (1 - delta_prop[nnn]) - trans_delta[n76] * delta_prop[nnn]) +
       vaccine50[78] * (1 - trans_alpha[n77] * (1 - delta_prop[nnn]) - trans_delta[n77] * delta_prop[nnn]) +
       vaccine50[79] * (1 - trans_alpha[n78] * (1 - delta_prop[nnn]) - trans_delta[n78] * delta_prop[nnn]) +
       vaccine50[80] * (1 - trans_alpha[n79] * (1 - delta_prop[nnn]) - trans_delta[n79] * delta_prop[nnn]) +
       vaccine50[81] * (1 - trans_alpha[n80] * (1 - delta_prop[nnn]) - trans_delta[n80] * delta_prop[nnn]) +
       vaccine50[82] * (1 - trans_alpha[n81] * (1 - delta_prop[nnn]) - trans_delta[n81] * delta_prop[nnn]) +
       vaccine50[83] * (1 - trans_alpha[n82] * (1 - delta_prop[nnn]) - trans_delta[n82] * delta_prop[nnn]) +
       vaccine50[84] * (1 - trans_alpha[n83] * (1 - delta_prop[nnn]) - trans_delta[n83] * delta_prop[nnn]) +
       vaccine50[85] * (1 - trans_alpha[n84] * (1 - delta_prop[nnn]) - trans_delta[n84] * delta_prop[nnn]) +
       vaccine50[86] * (1 - trans_alpha[n85] * (1 - delta_prop[nnn]) - trans_delta[n85] * delta_prop[nnn]) +
       vaccine50[87] * (1 - trans_alpha[n86] * (1 - delta_prop[nnn]) - trans_delta[n86] * delta_prop[nnn]) +
       vaccine50[88] * (1 - trans_alpha[n87] * (1 - delta_prop[nnn]) - trans_delta[n87] * delta_prop[nnn]) +
       vaccine50[89] * (1 - trans_alpha[n88] * (1 - delta_prop[nnn]) - trans_delta[n88] * delta_prop[nnn]) +
       vaccine50[90] * (1 - trans_alpha[n89] * (1 - delta_prop[nnn]) - trans_delta[n89] * delta_prop[nnn]) +
       vaccine50[91] * (1 - trans_alpha[n90] * (1 - delta_prop[nnn]) - trans_delta[n90] * delta_prop[nnn]) +
       vaccine50[92] * (1 - trans_alpha[n91] * (1 - delta_prop[nnn]) - trans_delta[n91] * delta_prop[nnn]) +
       vaccine50[93] * (1 - trans_alpha[n92] * (1 - delta_prop[nnn]) - trans_delta[n92] * delta_prop[nnn]) +
       vaccine50[94] * (1 - trans_alpha[n93] * (1 - delta_prop[nnn]) - trans_delta[n93] * delta_prop[nnn]) +
       vaccine50[95] * (1 - trans_alpha[n94] * (1 - delta_prop[nnn]) - trans_delta[n94] * delta_prop[nnn]) +
       vaccine50[96] * (1 - trans_alpha[n95] * (1 - delta_prop[nnn]) - trans_delta[n95] * delta_prop[nnn]) +
       vaccine50[97] * (1 - trans_alpha[n96] * (1 - delta_prop[nnn]) - trans_delta[n96] * delta_prop[nnn]) +
       vaccine50[98] * (1 - trans_alpha[n97] * (1 - delta_prop[nnn]) - trans_delta[n97] * delta_prop[nnn]) +
       vaccine50[99] * (1 - trans_alpha[n98] * (1 - delta_prop[nnn]) - trans_delta[n98] * delta_prop[nnn]) +
       ifelse((1 - cum_vac50[nnn] - ((sum(new_case50[1:9]) + sum(pred50_sofar[10:nnn])) / population50)) < 0,
              0,
              1 - cum_vac50[nnn] - ((sum(new_case50[1:9]) + sum(pred50_sofar[10:nnn])) / population50)))
  
  poi60 <- pred60_sofar[nnn] *
    (vaccine60[1] *  (1 - trans_alpha[n00] * (1 - delta_prop[nnn]) - trans_delta[n00] * delta_prop[nnn]) +
       vaccine60[2] *  (1 - trans_alpha[n01] * (1 - delta_prop[nnn]) - trans_delta[n01] * delta_prop[nnn]) +
       vaccine60[3] *  (1 - trans_alpha[n02] * (1 - delta_prop[nnn]) - trans_delta[n02] * delta_prop[nnn]) +
       vaccine60[4] *  (1 - trans_alpha[n03] * (1 - delta_prop[nnn]) - trans_delta[n03] * delta_prop[nnn]) +
       vaccine60[5] *  (1 - trans_alpha[n04] * (1 - delta_prop[nnn]) - trans_delta[n04] * delta_prop[nnn]) +
       vaccine60[6] *  (1 - trans_alpha[n05] * (1 - delta_prop[nnn]) - trans_delta[n05] * delta_prop[nnn]) +
       vaccine60[7] *  (1 - trans_alpha[n06] * (1 - delta_prop[nnn]) - trans_delta[n06] * delta_prop[nnn]) +
       vaccine60[8] *  (1 - trans_alpha[n07] * (1 - delta_prop[nnn]) - trans_delta[n07] * delta_prop[nnn]) +
       vaccine60[9] *  (1 - trans_alpha[n08] * (1 - delta_prop[nnn]) - trans_delta[n08] * delta_prop[nnn]) +
       vaccine60[10] * (1 - trans_alpha[n09] * (1 - delta_prop[nnn]) - trans_delta[n09] * delta_prop[nnn]) +
       vaccine60[11] * (1 - trans_alpha[n10] * (1 - delta_prop[nnn]) - trans_delta[n10] * delta_prop[nnn]) +
       vaccine60[12] * (1 - trans_alpha[n11] * (1 - delta_prop[nnn]) - trans_delta[n11] * delta_prop[nnn]) +
       vaccine60[13] * (1 - trans_alpha[n12] * (1 - delta_prop[nnn]) - trans_delta[n12] * delta_prop[nnn]) +
       vaccine60[14] * (1 - trans_alpha[n13] * (1 - delta_prop[nnn]) - trans_delta[n13] * delta_prop[nnn]) +
       vaccine60[15] * (1 - trans_alpha[n14] * (1 - delta_prop[nnn]) - trans_delta[n14] * delta_prop[nnn]) +
       vaccine60[16] * (1 - trans_alpha[n15] * (1 - delta_prop[nnn]) - trans_delta[n15] * delta_prop[nnn]) +
       vaccine60[17] * (1 - trans_alpha[n16] * (1 - delta_prop[nnn]) - trans_delta[n16] * delta_prop[nnn]) +
       vaccine60[18] * (1 - trans_alpha[n17] * (1 - delta_prop[nnn]) - trans_delta[n17] * delta_prop[nnn]) +
       vaccine60[19] * (1 - trans_alpha[n18] * (1 - delta_prop[nnn]) - trans_delta[n18] * delta_prop[nnn]) +
       vaccine60[20] * (1 - trans_alpha[n19] * (1 - delta_prop[nnn]) - trans_delta[n19] * delta_prop[nnn]) +
       vaccine60[21] * (1 - trans_alpha[n20] * (1 - delta_prop[nnn]) - trans_delta[n20] * delta_prop[nnn]) +
       vaccine60[22] * (1 - trans_alpha[n21] * (1 - delta_prop[nnn]) - trans_delta[n21] * delta_prop[nnn]) +
       vaccine60[23] * (1 - trans_alpha[n22] * (1 - delta_prop[nnn]) - trans_delta[n22] * delta_prop[nnn]) +
       vaccine60[24] * (1 - trans_alpha[n23] * (1 - delta_prop[nnn]) - trans_delta[n23] * delta_prop[nnn]) +
       vaccine60[25] * (1 - trans_alpha[n24] * (1 - delta_prop[nnn]) - trans_delta[n24] * delta_prop[nnn]) +
       vaccine60[26] * (1 - trans_alpha[n25] * (1 - delta_prop[nnn]) - trans_delta[n25] * delta_prop[nnn]) +
       vaccine60[27] * (1 - trans_alpha[n26] * (1 - delta_prop[nnn]) - trans_delta[n26] * delta_prop[nnn]) +
       vaccine60[28] * (1 - trans_alpha[n27] * (1 - delta_prop[nnn]) - trans_delta[n27] * delta_prop[nnn]) +
       vaccine60[29] * (1 - trans_alpha[n28] * (1 - delta_prop[nnn]) - trans_delta[n28] * delta_prop[nnn]) +
       vaccine60[30] * (1 - trans_alpha[n29] * (1 - delta_prop[nnn]) - trans_delta[n29] * delta_prop[nnn]) +
       vaccine60[31] * (1 - trans_alpha[n30] * (1 - delta_prop[nnn]) - trans_delta[n30] * delta_prop[nnn]) +
       vaccine60[32] * (1 - trans_alpha[n31] * (1 - delta_prop[nnn]) - trans_delta[n31] * delta_prop[nnn]) +
       vaccine60[33] * (1 - trans_alpha[n32] * (1 - delta_prop[nnn]) - trans_delta[n32] * delta_prop[nnn]) +
       vaccine60[34] * (1 - trans_alpha[n33] * (1 - delta_prop[nnn]) - trans_delta[n33] * delta_prop[nnn]) +
       vaccine60[35] * (1 - trans_alpha[n34] * (1 - delta_prop[nnn]) - trans_delta[n34] * delta_prop[nnn]) +
       vaccine60[36] * (1 - trans_alpha[n35] * (1 - delta_prop[nnn]) - trans_delta[n35] * delta_prop[nnn]) +
       vaccine60[37] * (1 - trans_alpha[n36] * (1 - delta_prop[nnn]) - trans_delta[n36] * delta_prop[nnn]) +
       vaccine60[38] * (1 - trans_alpha[n37] * (1 - delta_prop[nnn]) - trans_delta[n37] * delta_prop[nnn]) +
       vaccine60[39] * (1 - trans_alpha[n38] * (1 - delta_prop[nnn]) - trans_delta[n38] * delta_prop[nnn]) +
       vaccine60[40] * (1 - trans_alpha[n39] * (1 - delta_prop[nnn]) - trans_delta[n39] * delta_prop[nnn]) +
       vaccine60[41] * (1 - trans_alpha[n40] * (1 - delta_prop[nnn]) - trans_delta[n40] * delta_prop[nnn]) +
       vaccine60[42] * (1 - trans_alpha[n41] * (1 - delta_prop[nnn]) - trans_delta[n41] * delta_prop[nnn]) +
       vaccine60[43] * (1 - trans_alpha[n42] * (1 - delta_prop[nnn]) - trans_delta[n42] * delta_prop[nnn]) +
       vaccine60[44] * (1 - trans_alpha[n43] * (1 - delta_prop[nnn]) - trans_delta[n43] * delta_prop[nnn]) +
       vaccine60[45] * (1 - trans_alpha[n44] * (1 - delta_prop[nnn]) - trans_delta[n44] * delta_prop[nnn]) +
       vaccine60[46] * (1 - trans_alpha[n45] * (1 - delta_prop[nnn]) - trans_delta[n45] * delta_prop[nnn]) +
       vaccine60[47] * (1 - trans_alpha[n46] * (1 - delta_prop[nnn]) - trans_delta[n46] * delta_prop[nnn]) +
       vaccine60[48] * (1 - trans_alpha[n47] * (1 - delta_prop[nnn]) - trans_delta[n47] * delta_prop[nnn]) +
       vaccine60[49] * (1 - trans_alpha[n48] * (1 - delta_prop[nnn]) - trans_delta[n48] * delta_prop[nnn]) +
       vaccine60[50] * (1 - trans_alpha[n49] * (1 - delta_prop[nnn]) - trans_delta[n49] * delta_prop[nnn]) +
       vaccine60[51] * (1 - trans_alpha[n50] * (1 - delta_prop[nnn]) - trans_delta[n50] * delta_prop[nnn]) +
       vaccine60[52] * (1 - trans_alpha[n51] * (1 - delta_prop[nnn]) - trans_delta[n51] * delta_prop[nnn]) +
       vaccine60[53] * (1 - trans_alpha[n52] * (1 - delta_prop[nnn]) - trans_delta[n52] * delta_prop[nnn]) +
       vaccine60[54] * (1 - trans_alpha[n53] * (1 - delta_prop[nnn]) - trans_delta[n53] * delta_prop[nnn]) +
       vaccine60[55] * (1 - trans_alpha[n54] * (1 - delta_prop[nnn]) - trans_delta[n54] * delta_prop[nnn]) +
       vaccine60[56] * (1 - trans_alpha[n55] * (1 - delta_prop[nnn]) - trans_delta[n55] * delta_prop[nnn]) +
       vaccine60[57] * (1 - trans_alpha[n56] * (1 - delta_prop[nnn]) - trans_delta[n56] * delta_prop[nnn]) +
       vaccine60[58] * (1 - trans_alpha[n57] * (1 - delta_prop[nnn]) - trans_delta[n57] * delta_prop[nnn]) +
       vaccine60[59] * (1 - trans_alpha[n58] * (1 - delta_prop[nnn]) - trans_delta[n58] * delta_prop[nnn]) +
       vaccine60[60] * (1 - trans_alpha[n59] * (1 - delta_prop[nnn]) - trans_delta[n59] * delta_prop[nnn]) +
       vaccine60[61] * (1 - trans_alpha[n60] * (1 - delta_prop[nnn]) - trans_delta[n60] * delta_prop[nnn]) +
       vaccine60[62] * (1 - trans_alpha[n61] * (1 - delta_prop[nnn]) - trans_delta[n61] * delta_prop[nnn]) +
       vaccine60[63] * (1 - trans_alpha[n62] * (1 - delta_prop[nnn]) - trans_delta[n62] * delta_prop[nnn]) +
       vaccine60[64] * (1 - trans_alpha[n63] * (1 - delta_prop[nnn]) - trans_delta[n63] * delta_prop[nnn]) +
       vaccine60[65] * (1 - trans_alpha[n64] * (1 - delta_prop[nnn]) - trans_delta[n64] * delta_prop[nnn]) +
       vaccine60[66] * (1 - trans_alpha[n65] * (1 - delta_prop[nnn]) - trans_delta[n65] * delta_prop[nnn]) +
       vaccine60[67] * (1 - trans_alpha[n66] * (1 - delta_prop[nnn]) - trans_delta[n66] * delta_prop[nnn]) +
       vaccine60[68] * (1 - trans_alpha[n67] * (1 - delta_prop[nnn]) - trans_delta[n67] * delta_prop[nnn]) +
       vaccine60[69] * (1 - trans_alpha[n68] * (1 - delta_prop[nnn]) - trans_delta[n68] * delta_prop[nnn]) +
       vaccine60[70] * (1 - trans_alpha[n69] * (1 - delta_prop[nnn]) - trans_delta[n69] * delta_prop[nnn]) +
       vaccine60[71] * (1 - trans_alpha[n70] * (1 - delta_prop[nnn]) - trans_delta[n70] * delta_prop[nnn]) +
       vaccine60[72] * (1 - trans_alpha[n71] * (1 - delta_prop[nnn]) - trans_delta[n71] * delta_prop[nnn]) +
       vaccine60[73] * (1 - trans_alpha[n72] * (1 - delta_prop[nnn]) - trans_delta[n72] * delta_prop[nnn]) +
       vaccine60[74] * (1 - trans_alpha[n73] * (1 - delta_prop[nnn]) - trans_delta[n73] * delta_prop[nnn]) +
       vaccine60[75] * (1 - trans_alpha[n74] * (1 - delta_prop[nnn]) - trans_delta[n74] * delta_prop[nnn]) +
       vaccine60[76] * (1 - trans_alpha[n75] * (1 - delta_prop[nnn]) - trans_delta[n75] * delta_prop[nnn]) +
       vaccine60[77] * (1 - trans_alpha[n76] * (1 - delta_prop[nnn]) - trans_delta[n76] * delta_prop[nnn]) +
       vaccine60[78] * (1 - trans_alpha[n77] * (1 - delta_prop[nnn]) - trans_delta[n77] * delta_prop[nnn]) +
       vaccine60[79] * (1 - trans_alpha[n78] * (1 - delta_prop[nnn]) - trans_delta[n78] * delta_prop[nnn]) +
       vaccine60[80] * (1 - trans_alpha[n79] * (1 - delta_prop[nnn]) - trans_delta[n79] * delta_prop[nnn]) +
       vaccine60[81] * (1 - trans_alpha[n80] * (1 - delta_prop[nnn]) - trans_delta[n80] * delta_prop[nnn]) +
       vaccine60[82] * (1 - trans_alpha[n81] * (1 - delta_prop[nnn]) - trans_delta[n81] * delta_prop[nnn]) +
       vaccine60[83] * (1 - trans_alpha[n82] * (1 - delta_prop[nnn]) - trans_delta[n82] * delta_prop[nnn]) +
       vaccine60[84] * (1 - trans_alpha[n83] * (1 - delta_prop[nnn]) - trans_delta[n83] * delta_prop[nnn]) +
       vaccine60[85] * (1 - trans_alpha[n84] * (1 - delta_prop[nnn]) - trans_delta[n84] * delta_prop[nnn]) +
       vaccine60[86] * (1 - trans_alpha[n85] * (1 - delta_prop[nnn]) - trans_delta[n85] * delta_prop[nnn]) +
       vaccine60[87] * (1 - trans_alpha[n86] * (1 - delta_prop[nnn]) - trans_delta[n86] * delta_prop[nnn]) +
       vaccine60[88] * (1 - trans_alpha[n87] * (1 - delta_prop[nnn]) - trans_delta[n87] * delta_prop[nnn]) +
       vaccine60[89] * (1 - trans_alpha[n88] * (1 - delta_prop[nnn]) - trans_delta[n88] * delta_prop[nnn]) +
       vaccine60[90] * (1 - trans_alpha[n89] * (1 - delta_prop[nnn]) - trans_delta[n89] * delta_prop[nnn]) +
       vaccine60[91] * (1 - trans_alpha[n90] * (1 - delta_prop[nnn]) - trans_delta[n90] * delta_prop[nnn]) +
       vaccine60[92] * (1 - trans_alpha[n91] * (1 - delta_prop[nnn]) - trans_delta[n91] * delta_prop[nnn]) +
       vaccine60[93] * (1 - trans_alpha[n92] * (1 - delta_prop[nnn]) - trans_delta[n92] * delta_prop[nnn]) +
       vaccine60[94] * (1 - trans_alpha[n93] * (1 - delta_prop[nnn]) - trans_delta[n93] * delta_prop[nnn]) +
       vaccine60[95] * (1 - trans_alpha[n94] * (1 - delta_prop[nnn]) - trans_delta[n94] * delta_prop[nnn]) +
       vaccine60[96] * (1 - trans_alpha[n95] * (1 - delta_prop[nnn]) - trans_delta[n95] * delta_prop[nnn]) +
       vaccine60[97] * (1 - trans_alpha[n96] * (1 - delta_prop[nnn]) - trans_delta[n96] * delta_prop[nnn]) +
       vaccine60[98] * (1 - trans_alpha[n97] * (1 - delta_prop[nnn]) - trans_delta[n97] * delta_prop[nnn]) +
       vaccine60[99] * (1 - trans_alpha[n98] * (1 - delta_prop[nnn]) - trans_delta[n98] * delta_prop[nnn]) +
       ifelse((1 - cum_vac60[nnn] - ((sum(new_case60[1:9]) + sum(pred60_sofar[10:nnn])) / population60)) < 0,
              0,
              1 - cum_vac60[nnn] - ((sum(new_case60[1:9]) + sum(pred60_sofar[10:nnn])) / population60)))
  
  poi70 <- pred70_sofar[nnn] *
    (vaccine70[1] *  (1 - trans_alpha[n00] * (1 - delta_prop[nnn]) - trans_delta[n00] * delta_prop[nnn]) +
       vaccine70[2] *  (1 - trans_alpha[n01] * (1 - delta_prop[nnn]) - trans_delta[n01] * delta_prop[nnn]) +
       vaccine70[3] *  (1 - trans_alpha[n02] * (1 - delta_prop[nnn]) - trans_delta[n02] * delta_prop[nnn]) +
       vaccine70[4] *  (1 - trans_alpha[n03] * (1 - delta_prop[nnn]) - trans_delta[n03] * delta_prop[nnn]) +
       vaccine70[5] *  (1 - trans_alpha[n04] * (1 - delta_prop[nnn]) - trans_delta[n04] * delta_prop[nnn]) +
       vaccine70[6] *  (1 - trans_alpha[n05] * (1 - delta_prop[nnn]) - trans_delta[n05] * delta_prop[nnn]) +
       vaccine70[7] *  (1 - trans_alpha[n06] * (1 - delta_prop[nnn]) - trans_delta[n06] * delta_prop[nnn]) +
       vaccine70[8] *  (1 - trans_alpha[n07] * (1 - delta_prop[nnn]) - trans_delta[n07] * delta_prop[nnn]) +
       vaccine70[9] *  (1 - trans_alpha[n08] * (1 - delta_prop[nnn]) - trans_delta[n08] * delta_prop[nnn]) +
       vaccine70[10] * (1 - trans_alpha[n09] * (1 - delta_prop[nnn]) - trans_delta[n09] * delta_prop[nnn]) +
       vaccine70[11] * (1 - trans_alpha[n10] * (1 - delta_prop[nnn]) - trans_delta[n10] * delta_prop[nnn]) +
       vaccine70[12] * (1 - trans_alpha[n11] * (1 - delta_prop[nnn]) - trans_delta[n11] * delta_prop[nnn]) +
       vaccine70[13] * (1 - trans_alpha[n12] * (1 - delta_prop[nnn]) - trans_delta[n12] * delta_prop[nnn]) +
       vaccine70[14] * (1 - trans_alpha[n13] * (1 - delta_prop[nnn]) - trans_delta[n13] * delta_prop[nnn]) +
       vaccine70[15] * (1 - trans_alpha[n14] * (1 - delta_prop[nnn]) - trans_delta[n14] * delta_prop[nnn]) +
       vaccine70[16] * (1 - trans_alpha[n15] * (1 - delta_prop[nnn]) - trans_delta[n15] * delta_prop[nnn]) +
       vaccine70[17] * (1 - trans_alpha[n16] * (1 - delta_prop[nnn]) - trans_delta[n16] * delta_prop[nnn]) +
       vaccine70[18] * (1 - trans_alpha[n17] * (1 - delta_prop[nnn]) - trans_delta[n17] * delta_prop[nnn]) +
       vaccine70[19] * (1 - trans_alpha[n18] * (1 - delta_prop[nnn]) - trans_delta[n18] * delta_prop[nnn]) +
       vaccine70[20] * (1 - trans_alpha[n19] * (1 - delta_prop[nnn]) - trans_delta[n19] * delta_prop[nnn]) +
       vaccine70[21] * (1 - trans_alpha[n20] * (1 - delta_prop[nnn]) - trans_delta[n20] * delta_prop[nnn]) +
       vaccine70[22] * (1 - trans_alpha[n21] * (1 - delta_prop[nnn]) - trans_delta[n21] * delta_prop[nnn]) +
       vaccine70[23] * (1 - trans_alpha[n22] * (1 - delta_prop[nnn]) - trans_delta[n22] * delta_prop[nnn]) +
       vaccine70[24] * (1 - trans_alpha[n23] * (1 - delta_prop[nnn]) - trans_delta[n23] * delta_prop[nnn]) +
       vaccine70[25] * (1 - trans_alpha[n24] * (1 - delta_prop[nnn]) - trans_delta[n24] * delta_prop[nnn]) +
       vaccine70[26] * (1 - trans_alpha[n25] * (1 - delta_prop[nnn]) - trans_delta[n25] * delta_prop[nnn]) +
       vaccine70[27] * (1 - trans_alpha[n26] * (1 - delta_prop[nnn]) - trans_delta[n26] * delta_prop[nnn]) +
       vaccine70[28] * (1 - trans_alpha[n27] * (1 - delta_prop[nnn]) - trans_delta[n27] * delta_prop[nnn]) +
       vaccine70[29] * (1 - trans_alpha[n28] * (1 - delta_prop[nnn]) - trans_delta[n28] * delta_prop[nnn]) +
       vaccine70[30] * (1 - trans_alpha[n29] * (1 - delta_prop[nnn]) - trans_delta[n29] * delta_prop[nnn]) +
       vaccine70[31] * (1 - trans_alpha[n30] * (1 - delta_prop[nnn]) - trans_delta[n30] * delta_prop[nnn]) +
       vaccine70[32] * (1 - trans_alpha[n31] * (1 - delta_prop[nnn]) - trans_delta[n31] * delta_prop[nnn]) +
       vaccine70[33] * (1 - trans_alpha[n32] * (1 - delta_prop[nnn]) - trans_delta[n32] * delta_prop[nnn]) +
       vaccine70[34] * (1 - trans_alpha[n33] * (1 - delta_prop[nnn]) - trans_delta[n33] * delta_prop[nnn]) +
       vaccine70[35] * (1 - trans_alpha[n34] * (1 - delta_prop[nnn]) - trans_delta[n34] * delta_prop[nnn]) +
       vaccine70[36] * (1 - trans_alpha[n35] * (1 - delta_prop[nnn]) - trans_delta[n35] * delta_prop[nnn]) +
       vaccine70[37] * (1 - trans_alpha[n36] * (1 - delta_prop[nnn]) - trans_delta[n36] * delta_prop[nnn]) +
       vaccine70[38] * (1 - trans_alpha[n37] * (1 - delta_prop[nnn]) - trans_delta[n37] * delta_prop[nnn]) +
       vaccine70[39] * (1 - trans_alpha[n38] * (1 - delta_prop[nnn]) - trans_delta[n38] * delta_prop[nnn]) +
       vaccine70[40] * (1 - trans_alpha[n39] * (1 - delta_prop[nnn]) - trans_delta[n39] * delta_prop[nnn]) +
       vaccine70[41] * (1 - trans_alpha[n40] * (1 - delta_prop[nnn]) - trans_delta[n40] * delta_prop[nnn]) +
       vaccine70[42] * (1 - trans_alpha[n41] * (1 - delta_prop[nnn]) - trans_delta[n41] * delta_prop[nnn]) +
       vaccine70[43] * (1 - trans_alpha[n42] * (1 - delta_prop[nnn]) - trans_delta[n42] * delta_prop[nnn]) +
       vaccine70[44] * (1 - trans_alpha[n43] * (1 - delta_prop[nnn]) - trans_delta[n43] * delta_prop[nnn]) +
       vaccine70[45] * (1 - trans_alpha[n44] * (1 - delta_prop[nnn]) - trans_delta[n44] * delta_prop[nnn]) +
       vaccine70[46] * (1 - trans_alpha[n45] * (1 - delta_prop[nnn]) - trans_delta[n45] * delta_prop[nnn]) +
       vaccine70[47] * (1 - trans_alpha[n46] * (1 - delta_prop[nnn]) - trans_delta[n46] * delta_prop[nnn]) +
       vaccine70[48] * (1 - trans_alpha[n47] * (1 - delta_prop[nnn]) - trans_delta[n47] * delta_prop[nnn]) +
       vaccine70[49] * (1 - trans_alpha[n48] * (1 - delta_prop[nnn]) - trans_delta[n48] * delta_prop[nnn]) +
       vaccine70[50] * (1 - trans_alpha[n49] * (1 - delta_prop[nnn]) - trans_delta[n49] * delta_prop[nnn]) +
       vaccine70[51] * (1 - trans_alpha[n50] * (1 - delta_prop[nnn]) - trans_delta[n50] * delta_prop[nnn]) +
       vaccine70[52] * (1 - trans_alpha[n51] * (1 - delta_prop[nnn]) - trans_delta[n51] * delta_prop[nnn]) +
       vaccine70[53] * (1 - trans_alpha[n52] * (1 - delta_prop[nnn]) - trans_delta[n52] * delta_prop[nnn]) +
       vaccine70[54] * (1 - trans_alpha[n53] * (1 - delta_prop[nnn]) - trans_delta[n53] * delta_prop[nnn]) +
       vaccine70[55] * (1 - trans_alpha[n54] * (1 - delta_prop[nnn]) - trans_delta[n54] * delta_prop[nnn]) +
       vaccine70[56] * (1 - trans_alpha[n55] * (1 - delta_prop[nnn]) - trans_delta[n55] * delta_prop[nnn]) +
       vaccine70[57] * (1 - trans_alpha[n56] * (1 - delta_prop[nnn]) - trans_delta[n56] * delta_prop[nnn]) +
       vaccine70[58] * (1 - trans_alpha[n57] * (1 - delta_prop[nnn]) - trans_delta[n57] * delta_prop[nnn]) +
       vaccine70[59] * (1 - trans_alpha[n58] * (1 - delta_prop[nnn]) - trans_delta[n58] * delta_prop[nnn]) +
       vaccine70[60] * (1 - trans_alpha[n59] * (1 - delta_prop[nnn]) - trans_delta[n59] * delta_prop[nnn]) +
       vaccine70[61] * (1 - trans_alpha[n60] * (1 - delta_prop[nnn]) - trans_delta[n60] * delta_prop[nnn]) +
       vaccine70[62] * (1 - trans_alpha[n61] * (1 - delta_prop[nnn]) - trans_delta[n61] * delta_prop[nnn]) +
       vaccine70[63] * (1 - trans_alpha[n62] * (1 - delta_prop[nnn]) - trans_delta[n62] * delta_prop[nnn]) +
       vaccine70[64] * (1 - trans_alpha[n63] * (1 - delta_prop[nnn]) - trans_delta[n63] * delta_prop[nnn]) +
       vaccine70[65] * (1 - trans_alpha[n64] * (1 - delta_prop[nnn]) - trans_delta[n64] * delta_prop[nnn]) +
       vaccine70[66] * (1 - trans_alpha[n65] * (1 - delta_prop[nnn]) - trans_delta[n65] * delta_prop[nnn]) +
       vaccine70[67] * (1 - trans_alpha[n66] * (1 - delta_prop[nnn]) - trans_delta[n66] * delta_prop[nnn]) +
       vaccine70[68] * (1 - trans_alpha[n67] * (1 - delta_prop[nnn]) - trans_delta[n67] * delta_prop[nnn]) +
       vaccine70[69] * (1 - trans_alpha[n68] * (1 - delta_prop[nnn]) - trans_delta[n68] * delta_prop[nnn]) +
       vaccine70[70] * (1 - trans_alpha[n69] * (1 - delta_prop[nnn]) - trans_delta[n69] * delta_prop[nnn]) +
       vaccine70[71] * (1 - trans_alpha[n70] * (1 - delta_prop[nnn]) - trans_delta[n70] * delta_prop[nnn]) +
       vaccine70[72] * (1 - trans_alpha[n71] * (1 - delta_prop[nnn]) - trans_delta[n71] * delta_prop[nnn]) +
       vaccine70[73] * (1 - trans_alpha[n72] * (1 - delta_prop[nnn]) - trans_delta[n72] * delta_prop[nnn]) +
       vaccine70[74] * (1 - trans_alpha[n73] * (1 - delta_prop[nnn]) - trans_delta[n73] * delta_prop[nnn]) +
       vaccine70[75] * (1 - trans_alpha[n74] * (1 - delta_prop[nnn]) - trans_delta[n74] * delta_prop[nnn]) +
       vaccine70[76] * (1 - trans_alpha[n75] * (1 - delta_prop[nnn]) - trans_delta[n75] * delta_prop[nnn]) +
       vaccine70[77] * (1 - trans_alpha[n76] * (1 - delta_prop[nnn]) - trans_delta[n76] * delta_prop[nnn]) +
       vaccine70[78] * (1 - trans_alpha[n77] * (1 - delta_prop[nnn]) - trans_delta[n77] * delta_prop[nnn]) +
       vaccine70[79] * (1 - trans_alpha[n78] * (1 - delta_prop[nnn]) - trans_delta[n78] * delta_prop[nnn]) +
       vaccine70[80] * (1 - trans_alpha[n79] * (1 - delta_prop[nnn]) - trans_delta[n79] * delta_prop[nnn]) +
       vaccine70[81] * (1 - trans_alpha[n80] * (1 - delta_prop[nnn]) - trans_delta[n80] * delta_prop[nnn]) +
       vaccine70[82] * (1 - trans_alpha[n81] * (1 - delta_prop[nnn]) - trans_delta[n81] * delta_prop[nnn]) +
       vaccine70[83] * (1 - trans_alpha[n82] * (1 - delta_prop[nnn]) - trans_delta[n82] * delta_prop[nnn]) +
       vaccine70[84] * (1 - trans_alpha[n83] * (1 - delta_prop[nnn]) - trans_delta[n83] * delta_prop[nnn]) +
       vaccine70[85] * (1 - trans_alpha[n84] * (1 - delta_prop[nnn]) - trans_delta[n84] * delta_prop[nnn]) +
       vaccine70[86] * (1 - trans_alpha[n85] * (1 - delta_prop[nnn]) - trans_delta[n85] * delta_prop[nnn]) +
       vaccine70[87] * (1 - trans_alpha[n86] * (1 - delta_prop[nnn]) - trans_delta[n86] * delta_prop[nnn]) +
       vaccine70[88] * (1 - trans_alpha[n87] * (1 - delta_prop[nnn]) - trans_delta[n87] * delta_prop[nnn]) +
       vaccine70[89] * (1 - trans_alpha[n88] * (1 - delta_prop[nnn]) - trans_delta[n88] * delta_prop[nnn]) +
       vaccine70[90] * (1 - trans_alpha[n89] * (1 - delta_prop[nnn]) - trans_delta[n89] * delta_prop[nnn]) +
       vaccine70[91] * (1 - trans_alpha[n90] * (1 - delta_prop[nnn]) - trans_delta[n90] * delta_prop[nnn]) +
       vaccine70[92] * (1 - trans_alpha[n91] * (1 - delta_prop[nnn]) - trans_delta[n91] * delta_prop[nnn]) +
       vaccine70[93] * (1 - trans_alpha[n92] * (1 - delta_prop[nnn]) - trans_delta[n92] * delta_prop[nnn]) +
       vaccine70[94] * (1 - trans_alpha[n93] * (1 - delta_prop[nnn]) - trans_delta[n93] * delta_prop[nnn]) +
       vaccine70[95] * (1 - trans_alpha[n94] * (1 - delta_prop[nnn]) - trans_delta[n94] * delta_prop[nnn]) +
       vaccine70[96] * (1 - trans_alpha[n95] * (1 - delta_prop[nnn]) - trans_delta[n95] * delta_prop[nnn]) +
       vaccine70[97] * (1 - trans_alpha[n96] * (1 - delta_prop[nnn]) - trans_delta[n96] * delta_prop[nnn]) +
       vaccine70[98] * (1 - trans_alpha[n97] * (1 - delta_prop[nnn]) - trans_delta[n97] * delta_prop[nnn]) +
       vaccine70[99] * (1 - trans_alpha[n98] * (1 - delta_prop[nnn]) - trans_delta[n98] * delta_prop[nnn]) +
       ifelse((1 - cum_vac70[nnn] - ((sum(new_case70[1:9]) + sum(pred70_sofar[10:nnn])) / population70)) < 0,
              0,
              1 - cum_vac70[nnn] - ((sum(new_case70[1:9]) + sum(pred70_sofar[10:nnn])) / population70)))
  
  poi80 <- pred80_sofar[nnn] *
    (vaccine80[1] *  (1 - trans_alpha[n00] * (1 - delta_prop[nnn]) - trans_delta[n00] * delta_prop[nnn]) +
       vaccine80[2] *  (1 - trans_alpha[n01] * (1 - delta_prop[nnn]) - trans_delta[n01] * delta_prop[nnn]) +
       vaccine80[3] *  (1 - trans_alpha[n02] * (1 - delta_prop[nnn]) - trans_delta[n02] * delta_prop[nnn]) +
       vaccine80[4] *  (1 - trans_alpha[n03] * (1 - delta_prop[nnn]) - trans_delta[n03] * delta_prop[nnn]) +
       vaccine80[5] *  (1 - trans_alpha[n04] * (1 - delta_prop[nnn]) - trans_delta[n04] * delta_prop[nnn]) +
       vaccine80[6] *  (1 - trans_alpha[n05] * (1 - delta_prop[nnn]) - trans_delta[n05] * delta_prop[nnn]) +
       vaccine80[7] *  (1 - trans_alpha[n06] * (1 - delta_prop[nnn]) - trans_delta[n06] * delta_prop[nnn]) +
       vaccine80[8] *  (1 - trans_alpha[n07] * (1 - delta_prop[nnn]) - trans_delta[n07] * delta_prop[nnn]) +
       vaccine80[9] *  (1 - trans_alpha[n08] * (1 - delta_prop[nnn]) - trans_delta[n08] * delta_prop[nnn]) +
       vaccine80[10] * (1 - trans_alpha[n09] * (1 - delta_prop[nnn]) - trans_delta[n09] * delta_prop[nnn]) +
       vaccine80[11] * (1 - trans_alpha[n10] * (1 - delta_prop[nnn]) - trans_delta[n10] * delta_prop[nnn]) +
       vaccine80[12] * (1 - trans_alpha[n11] * (1 - delta_prop[nnn]) - trans_delta[n11] * delta_prop[nnn]) +
       vaccine80[13] * (1 - trans_alpha[n12] * (1 - delta_prop[nnn]) - trans_delta[n12] * delta_prop[nnn]) +
       vaccine80[14] * (1 - trans_alpha[n13] * (1 - delta_prop[nnn]) - trans_delta[n13] * delta_prop[nnn]) +
       vaccine80[15] * (1 - trans_alpha[n14] * (1 - delta_prop[nnn]) - trans_delta[n14] * delta_prop[nnn]) +
       vaccine80[16] * (1 - trans_alpha[n15] * (1 - delta_prop[nnn]) - trans_delta[n15] * delta_prop[nnn]) +
       vaccine80[17] * (1 - trans_alpha[n16] * (1 - delta_prop[nnn]) - trans_delta[n16] * delta_prop[nnn]) +
       vaccine80[18] * (1 - trans_alpha[n17] * (1 - delta_prop[nnn]) - trans_delta[n17] * delta_prop[nnn]) +
       vaccine80[19] * (1 - trans_alpha[n18] * (1 - delta_prop[nnn]) - trans_delta[n18] * delta_prop[nnn]) +
       vaccine80[20] * (1 - trans_alpha[n19] * (1 - delta_prop[nnn]) - trans_delta[n19] * delta_prop[nnn]) +
       vaccine80[21] * (1 - trans_alpha[n20] * (1 - delta_prop[nnn]) - trans_delta[n20] * delta_prop[nnn]) +
       vaccine80[22] * (1 - trans_alpha[n21] * (1 - delta_prop[nnn]) - trans_delta[n21] * delta_prop[nnn]) +
       vaccine80[23] * (1 - trans_alpha[n22] * (1 - delta_prop[nnn]) - trans_delta[n22] * delta_prop[nnn]) +
       vaccine80[24] * (1 - trans_alpha[n23] * (1 - delta_prop[nnn]) - trans_delta[n23] * delta_prop[nnn]) +
       vaccine80[25] * (1 - trans_alpha[n24] * (1 - delta_prop[nnn]) - trans_delta[n24] * delta_prop[nnn]) +
       vaccine80[26] * (1 - trans_alpha[n25] * (1 - delta_prop[nnn]) - trans_delta[n25] * delta_prop[nnn]) +
       vaccine80[27] * (1 - trans_alpha[n26] * (1 - delta_prop[nnn]) - trans_delta[n26] * delta_prop[nnn]) +
       vaccine80[28] * (1 - trans_alpha[n27] * (1 - delta_prop[nnn]) - trans_delta[n27] * delta_prop[nnn]) +
       vaccine80[29] * (1 - trans_alpha[n28] * (1 - delta_prop[nnn]) - trans_delta[n28] * delta_prop[nnn]) +
       vaccine80[30] * (1 - trans_alpha[n29] * (1 - delta_prop[nnn]) - trans_delta[n29] * delta_prop[nnn]) +
       vaccine80[31] * (1 - trans_alpha[n30] * (1 - delta_prop[nnn]) - trans_delta[n30] * delta_prop[nnn]) +
       vaccine80[32] * (1 - trans_alpha[n31] * (1 - delta_prop[nnn]) - trans_delta[n31] * delta_prop[nnn]) +
       vaccine80[33] * (1 - trans_alpha[n32] * (1 - delta_prop[nnn]) - trans_delta[n32] * delta_prop[nnn]) +
       vaccine80[34] * (1 - trans_alpha[n33] * (1 - delta_prop[nnn]) - trans_delta[n33] * delta_prop[nnn]) +
       vaccine80[35] * (1 - trans_alpha[n34] * (1 - delta_prop[nnn]) - trans_delta[n34] * delta_prop[nnn]) +
       vaccine80[36] * (1 - trans_alpha[n35] * (1 - delta_prop[nnn]) - trans_delta[n35] * delta_prop[nnn]) +
       vaccine80[37] * (1 - trans_alpha[n36] * (1 - delta_prop[nnn]) - trans_delta[n36] * delta_prop[nnn]) +
       vaccine80[38] * (1 - trans_alpha[n37] * (1 - delta_prop[nnn]) - trans_delta[n37] * delta_prop[nnn]) +
       vaccine80[39] * (1 - trans_alpha[n38] * (1 - delta_prop[nnn]) - trans_delta[n38] * delta_prop[nnn]) +
       vaccine80[40] * (1 - trans_alpha[n39] * (1 - delta_prop[nnn]) - trans_delta[n39] * delta_prop[nnn]) +
       vaccine80[41] * (1 - trans_alpha[n40] * (1 - delta_prop[nnn]) - trans_delta[n40] * delta_prop[nnn]) +
       vaccine80[42] * (1 - trans_alpha[n41] * (1 - delta_prop[nnn]) - trans_delta[n41] * delta_prop[nnn]) +
       vaccine80[43] * (1 - trans_alpha[n42] * (1 - delta_prop[nnn]) - trans_delta[n42] * delta_prop[nnn]) +
       vaccine80[44] * (1 - trans_alpha[n43] * (1 - delta_prop[nnn]) - trans_delta[n43] * delta_prop[nnn]) +
       vaccine80[45] * (1 - trans_alpha[n44] * (1 - delta_prop[nnn]) - trans_delta[n44] * delta_prop[nnn]) +
       vaccine80[46] * (1 - trans_alpha[n45] * (1 - delta_prop[nnn]) - trans_delta[n45] * delta_prop[nnn]) +
       vaccine80[47] * (1 - trans_alpha[n46] * (1 - delta_prop[nnn]) - trans_delta[n46] * delta_prop[nnn]) +
       vaccine80[48] * (1 - trans_alpha[n47] * (1 - delta_prop[nnn]) - trans_delta[n47] * delta_prop[nnn]) +
       vaccine80[49] * (1 - trans_alpha[n48] * (1 - delta_prop[nnn]) - trans_delta[n48] * delta_prop[nnn]) +
       vaccine80[50] * (1 - trans_alpha[n49] * (1 - delta_prop[nnn]) - trans_delta[n49] * delta_prop[nnn]) +
       vaccine80[51] * (1 - trans_alpha[n50] * (1 - delta_prop[nnn]) - trans_delta[n50] * delta_prop[nnn]) +
       vaccine80[52] * (1 - trans_alpha[n51] * (1 - delta_prop[nnn]) - trans_delta[n51] * delta_prop[nnn]) +
       vaccine80[53] * (1 - trans_alpha[n52] * (1 - delta_prop[nnn]) - trans_delta[n52] * delta_prop[nnn]) +
       vaccine80[54] * (1 - trans_alpha[n53] * (1 - delta_prop[nnn]) - trans_delta[n53] * delta_prop[nnn]) +
       vaccine80[55] * (1 - trans_alpha[n54] * (1 - delta_prop[nnn]) - trans_delta[n54] * delta_prop[nnn]) +
       vaccine80[56] * (1 - trans_alpha[n55] * (1 - delta_prop[nnn]) - trans_delta[n55] * delta_prop[nnn]) +
       vaccine80[57] * (1 - trans_alpha[n56] * (1 - delta_prop[nnn]) - trans_delta[n56] * delta_prop[nnn]) +
       vaccine80[58] * (1 - trans_alpha[n57] * (1 - delta_prop[nnn]) - trans_delta[n57] * delta_prop[nnn]) +
       vaccine80[59] * (1 - trans_alpha[n58] * (1 - delta_prop[nnn]) - trans_delta[n58] * delta_prop[nnn]) +
       vaccine80[60] * (1 - trans_alpha[n59] * (1 - delta_prop[nnn]) - trans_delta[n59] * delta_prop[nnn]) +
       vaccine80[61] * (1 - trans_alpha[n60] * (1 - delta_prop[nnn]) - trans_delta[n60] * delta_prop[nnn]) +
       vaccine80[62] * (1 - trans_alpha[n61] * (1 - delta_prop[nnn]) - trans_delta[n61] * delta_prop[nnn]) +
       vaccine80[63] * (1 - trans_alpha[n62] * (1 - delta_prop[nnn]) - trans_delta[n62] * delta_prop[nnn]) +
       vaccine80[64] * (1 - trans_alpha[n63] * (1 - delta_prop[nnn]) - trans_delta[n63] * delta_prop[nnn]) +
       vaccine80[65] * (1 - trans_alpha[n64] * (1 - delta_prop[nnn]) - trans_delta[n64] * delta_prop[nnn]) +
       vaccine80[66] * (1 - trans_alpha[n65] * (1 - delta_prop[nnn]) - trans_delta[n65] * delta_prop[nnn]) +
       vaccine80[67] * (1 - trans_alpha[n66] * (1 - delta_prop[nnn]) - trans_delta[n66] * delta_prop[nnn]) +
       vaccine80[68] * (1 - trans_alpha[n67] * (1 - delta_prop[nnn]) - trans_delta[n67] * delta_prop[nnn]) +
       vaccine80[69] * (1 - trans_alpha[n68] * (1 - delta_prop[nnn]) - trans_delta[n68] * delta_prop[nnn]) +
       vaccine80[70] * (1 - trans_alpha[n69] * (1 - delta_prop[nnn]) - trans_delta[n69] * delta_prop[nnn]) +
       vaccine80[71] * (1 - trans_alpha[n70] * (1 - delta_prop[nnn]) - trans_delta[n70] * delta_prop[nnn]) +
       vaccine80[72] * (1 - trans_alpha[n71] * (1 - delta_prop[nnn]) - trans_delta[n71] * delta_prop[nnn]) +
       vaccine80[73] * (1 - trans_alpha[n72] * (1 - delta_prop[nnn]) - trans_delta[n72] * delta_prop[nnn]) +
       vaccine80[74] * (1 - trans_alpha[n73] * (1 - delta_prop[nnn]) - trans_delta[n73] * delta_prop[nnn]) +
       vaccine80[75] * (1 - trans_alpha[n74] * (1 - delta_prop[nnn]) - trans_delta[n74] * delta_prop[nnn]) +
       vaccine80[76] * (1 - trans_alpha[n75] * (1 - delta_prop[nnn]) - trans_delta[n75] * delta_prop[nnn]) +
       vaccine80[77] * (1 - trans_alpha[n76] * (1 - delta_prop[nnn]) - trans_delta[n76] * delta_prop[nnn]) +
       vaccine80[78] * (1 - trans_alpha[n77] * (1 - delta_prop[nnn]) - trans_delta[n77] * delta_prop[nnn]) +
       vaccine80[79] * (1 - trans_alpha[n78] * (1 - delta_prop[nnn]) - trans_delta[n78] * delta_prop[nnn]) +
       vaccine80[80] * (1 - trans_alpha[n79] * (1 - delta_prop[nnn]) - trans_delta[n79] * delta_prop[nnn]) +
       vaccine80[81] * (1 - trans_alpha[n80] * (1 - delta_prop[nnn]) - trans_delta[n80] * delta_prop[nnn]) +
       vaccine80[82] * (1 - trans_alpha[n81] * (1 - delta_prop[nnn]) - trans_delta[n81] * delta_prop[nnn]) +
       vaccine80[83] * (1 - trans_alpha[n82] * (1 - delta_prop[nnn]) - trans_delta[n82] * delta_prop[nnn]) +
       vaccine80[84] * (1 - trans_alpha[n83] * (1 - delta_prop[nnn]) - trans_delta[n83] * delta_prop[nnn]) +
       vaccine80[85] * (1 - trans_alpha[n84] * (1 - delta_prop[nnn]) - trans_delta[n84] * delta_prop[nnn]) +
       vaccine80[86] * (1 - trans_alpha[n85] * (1 - delta_prop[nnn]) - trans_delta[n85] * delta_prop[nnn]) +
       vaccine80[87] * (1 - trans_alpha[n86] * (1 - delta_prop[nnn]) - trans_delta[n86] * delta_prop[nnn]) +
       vaccine80[88] * (1 - trans_alpha[n87] * (1 - delta_prop[nnn]) - trans_delta[n87] * delta_prop[nnn]) +
       vaccine80[89] * (1 - trans_alpha[n88] * (1 - delta_prop[nnn]) - trans_delta[n88] * delta_prop[nnn]) +
       vaccine80[90] * (1 - trans_alpha[n89] * (1 - delta_prop[nnn]) - trans_delta[n89] * delta_prop[nnn]) +
       vaccine80[91] * (1 - trans_alpha[n90] * (1 - delta_prop[nnn]) - trans_delta[n90] * delta_prop[nnn]) +
       vaccine80[92] * (1 - trans_alpha[n91] * (1 - delta_prop[nnn]) - trans_delta[n91] * delta_prop[nnn]) +
       vaccine80[93] * (1 - trans_alpha[n92] * (1 - delta_prop[nnn]) - trans_delta[n92] * delta_prop[nnn]) +
       vaccine80[94] * (1 - trans_alpha[n93] * (1 - delta_prop[nnn]) - trans_delta[n93] * delta_prop[nnn]) +
       vaccine80[95] * (1 - trans_alpha[n94] * (1 - delta_prop[nnn]) - trans_delta[n94] * delta_prop[nnn]) +
       vaccine80[96] * (1 - trans_alpha[n95] * (1 - delta_prop[nnn]) - trans_delta[n95] * delta_prop[nnn]) +
       vaccine80[97] * (1 - trans_alpha[n96] * (1 - delta_prop[nnn]) - trans_delta[n96] * delta_prop[nnn]) +
       vaccine80[98] * (1 - trans_alpha[n97] * (1 - delta_prop[nnn]) - trans_delta[n97] * delta_prop[nnn]) +
       vaccine80[99] * (1 - trans_alpha[n98] * (1 - delta_prop[nnn]) - trans_delta[n98] * delta_prop[nnn]) +
       ifelse((1 - cum_vac80[nnn] - ((sum(new_case80[1:9]) + sum(pred80_sofar[10:nnn])) / population80)) < 0,
              0,
              1 - cum_vac80[nnn] - ((sum(new_case80[1:9]) + sum(pred80_sofar[10:nnn])) / population80)))
  
  
  poi <- (poi00 + poi10) +
    ((poi20 + poi30) * (((4+4+16+4)/(1+1+1+4)) ^ (7/5))) +
    ((poi40 + poi50) * (((4+4+16+4)/(1+1+1+4)) ^ (7/5))) +
    (poi60 + poi70 + poi80)       
  
  pred0080 <- (rt_set[nnn] ^ (7/5)) * poi
  #pred0080 <- rpois(1,(rt_set[n] ^ (7/5)) * poi)
  
  new_case0080_n1 <- new_case00[nnn+1] + new_case10[nnn+1] + new_case20[nnn+1] + new_case30[nnn+1] + new_case40[nnn+1] + new_case50[nnn+1] + new_case60[nnn+1] + new_case70[nnn+1] + new_case80[nnn+1]
  
  pred00_sofar[nnn+1] <- pred0080 * (new_case00[nnn+1] / new_case0080_n1)
  pred10_sofar[nnn+1] <- pred0080 * (new_case10[nnn+1] / new_case0080_n1)
  pred20_sofar[nnn+1] <- pred0080 * (new_case20[nnn+1] / new_case0080_n1)
  pred30_sofar[nnn+1] <- pred0080 * (new_case30[nnn+1] / new_case0080_n1)
  pred40_sofar[nnn+1] <- pred0080 * (new_case40[nnn+1] / new_case0080_n1)
  pred50_sofar[nnn+1] <- pred0080 * (new_case50[nnn+1] / new_case0080_n1)
  pred60_sofar[nnn+1] <- pred0080 * (new_case60[nnn+1] / new_case0080_n1)
  pred70_sofar[nnn+1] <- pred0080 * (new_case70[nnn+1] / new_case0080_n1)
  pred80_sofar[nnn+1] <- pred0080 * (new_case80[nnn+1] / new_case0080_n1)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # pred_sofar fill end
  
  
} # week by week end


### fit end


# Save results (all environment)
# "after_60.5_environment_rev.RData"

# save rt_set
save(rt_set, file = "rt_set_60.5_rev3.RData")






load("rt_set_60.5_rev3.RData")




























### Rt check


# 
# # Calculate Rt directly from case number
# rt_direct <- (all_data$case_total_diff[2:100] / all_data$case_total_diff[1:99]) ^ (5/7)
# 
# # Convert Rt to real scale
# rt_set_convert <- rt_set * (max(rt_direct[20:99]) / max(rt_set[20:99]))
# 
# 
# 
# # compare Rt model and Rt direct
# date <- all_data$date[1:99]
# rt <- c(rt_set_convert, rt_direct)
# class <- c(rep("model", 99), rep("direct", 99))
# 
# graph_data <- data.frame(date, rt, class)
# 
# #plot rt on y, date on x
# ggplot(graph_data, aes(x=date)) +
#   geom_line(aes(y=rt, color=class, linetype = class)) +
#   scale_linetype_manual(values = c("solid", "dashed")) +
#   theme_minimal() +
#   labs(title = "Rt model and Rt direct",
#        x = "Date",
#        y = "Rt") +
#   # add horizontal line at 1
#   geom_hline(yintercept = 1, linetype="dashed", color = "black")
# 
# 
# 
# 
# 
# 
# 






























# simulation for fit check (using observed case number)




# ### test no vac ###
# 
# vaccine_data$vaccine_00 <- 0
# vaccine_data$vaccine_10 <- 0
# vaccine_data$vaccine_20 <- 0
# vaccine_data$vaccine_30 <- 0
# vaccine_data$vaccine_40 <- 0
# vaccine_data$vaccine_50 <- 0
# vaccine_data$vaccine_60 <- 0
# vaccine_data$vaccine_70 <- 0
# vaccine_data$vaccine_80 <- 0
# 

# vaccine_data$vaccine_00_cum <- 0
# vaccine_data$vaccine_10_cum <- 0
# vaccine_data$vaccine_20_cum <- 0
# vaccine_data$vaccine_30_cum <- 0
# vaccine_data$vaccine_40_cum <- 0
# vaccine_data$vaccine_50_cum <- 0
# vaccine_data$vaccine_60_cum <- 0
# vaccine_data$vaccine_70_cum <- 0
# vaccine_data$vaccine_80_cum <- 0
# 
# ### test no vac ###









# check fit

pred0080 <- pred00_sofar + pred10_sofar + pred20_sofar + pred30_sofar + pred40_sofar + pred50_sofar + pred60_sofar + pred70_sofar + pred80_sofar








# plot observed and simulated case number

date <- rep(all_data$date[1:100], 2)
case <- c(all_data$case_total_diff[1:100], pred0080)
class <- c(rep("Observed", 100),rep("Model", 100))

df_graph2 <- data.frame(date, case, class)
df_graph2$class <- factor(df_graph2$class, levels = c("Observed", "Model"))

ggplot(df_graph2) +
  geom_line(aes(x=date, y=case, group= class, color=class, linetype = class), size = 1) +
  scale_color_manual(values = c("black", "red")) +
  scale_linetype_manual(values = c("solid", "dashed"))  # limit x from 2021-01-01 to 2021-12-31
scale_x_date(limits = as.Date(c("2021-01-01", "2021-12-31")), date_labels = "%Y-%m-%d") +
  # limit y from 0 to 200000
  scale_y_continuous(limits = c(0, 200000)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(title="Case number", x="", y="") +
  # increase font size for x label, y label, x title, y title, title
  theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16),
        plot.title = element_text(size = 40)) +
  # increase font size for legend
  theme(legend.title = element_text(size = 28), legend.text = element_text(size = 28)) +
  theme(legend.title = element_blank())
# position legend at left bottom
# theme(legend.position = c(0.3, 0.3))













