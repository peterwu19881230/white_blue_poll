# input data
n1=2046
n2=1149
n3=1112
n4=1112
n5=1082
n6=1484

rate_1_1A=0.483
rate_1_1B=0.392
rate_1_2A=0.461
rate_1_2B=0.416

rate_2_1A=0.41
rate_2_1B=0.35
rate_2_2A=0.42
rate_2_2B=0.36

rate_3_1A=0.466
rate_3_1B=0.331
rate_3_2A=0.465
rate_3_2B=0.349

rate_4_1A=0.4601
rate_4_1B=0.3222
rate_4_2A=0.4082
rate_4_2B=0.3586

rate_5_1A=0.44
rate_5_1B=0.32
rate_5_2A=0.39
rate_5_2B=0.33

rate_6_1A=0.388
rate_6_1B=0.293
rate_6_2A=0.382
rate_6_2B=0.306

calculate_support_and_SE=function(n,rate_1A,rate_1B,rate_2A,rate_2B){
  # Calculate the standard errors for the differences in support for each strategy
  # The distributions are binomial
  SE_diff_strategy1 = sqrt((rate_1A * (1 - rate_1A) / n) +
                             (rate_1B * (1 - rate_1B) / n))
  
  SE_diff_strategy2 = sqrt((rate_2A * (1 - rate_2A) / n) +
                             (rate_2B * (1 - rate_2B) / n))
  
  # Calculate the difference in the differences in support
  diff_in_diff_support = abs((rate_1A - rate_1B) -
                               (rate_2A - rate_2B))
  
  # Calculate the combined standard error of the difference between the strategies
  SE_diff_combined = sqrt(SE_diff_strategy1^2 + SE_diff_strategy2^2)
  
  
  return(c(diff_in_diff_support=diff_in_diff_support*100,SE_diff_combined=SE_diff_combined*100))
}
  
# part I: validating the mean difference & standard error calculation
calculate_support_and_SE(n1,rate_1_1A,rate_1_1B,rate_1_2A,rate_1_2B)

# part II: What if all polls are combined into one big poll?
N=n1+n2+n3+n4+n5+n6
Rate_1A=(rate_1_1A*n1+rate_2_1A*n2+rate_3_1A*n3+rate_4_1A*n4+rate_5_1A*n5+rate_6_1A*n6)/N
Rate_1B=(rate_1_1B*n1+rate_2_1B*n2+rate_3_1B*n3+rate_4_1B*n4+rate_5_1B*n5+rate_6_1B*n6)/N
Rate_2A=(rate_1_2A*n1+rate_2_2A*n2+rate_3_2A*n3+rate_4_2A*n4+rate_5_2A*n5+rate_6_2A*n6)/N
Rate_2B=(rate_1_2B*n1+rate_2_2B*n2+rate_3_2B*n3+rate_4_2B*n4+rate_5_2B*n5+rate_6_2B*n6)/N  
  
calculate_support_and_SE(N,Rate_1A,Rate_1B,Rate_2A,Rate_2B) # All 6 polls came from the same population. Therefore, it makes most sense to combine rather than voting separately. Combining will reduce variance.
##  = in part II TPP wins even if it's 2 standard errors away =


# part III: What if 6 polls are separated into 12 smaller polls?
calculate_support_and_SE(n1/2,rate_1_1A,rate_1_1B,rate_1_2A,rate_1_2B) #if more splits are done, standard error will increase
calculate_support_and_SE(n2/2,rate_2_1A,rate_2_1B,rate_2_2A,rate_2_2B)
calculate_support_and_SE(n3/2,rate_3_1A,rate_3_1B,rate_3_2A,rate_3_2B)
calculate_support_and_SE(n4/2,rate_3_1A,rate_3_1B,rate_3_2A,rate_3_2B)
calculate_support_and_SE(n5/2,rate_3_1A,rate_3_1B,rate_3_2A,rate_3_2B)
calculate_support_and_SE(n6/2,rate_3_1A,rate_3_1B,rate_3_2A,rate_3_2B)
# = in part III, where polls are split into even more separated polls, KMT always wins. = 







