#calculate modeling outcomes 
require(ggplot2)

#load probability distributions
source("distributions.R")

#calculations
calculate = function(c, beta, tau, p, theta_h, theta_l){
  ### Function that does all the calculations required for the model 
  totalP = function(tau, pf, GF){
    #calculate total probability of passing or failing for G or F given tau
    if (pf == "p" && GF == "G") {
      total = (1-G_H(tau))*p+(1-G_L(tau))*(1-p)
      return(total)
    } else if (pf == "f" && GF == "G") {
      total = (G_H(tau))*p+(G_L(tau))*(1-p)
      return(total)
    } else if (pf == "f" && GF == "F") {
      total = (F_H(tau))*p+(F_L(tau))*(1-p)
      return(total)
    } else if (pf == "p" && GF == "F") {
      total = (1-F_H(tau))*p+(1-F_L(tau))*(1-p)
      return(total)
    }
  }
  post_theta = function(tau, pf, GF){
    #calculate posterior theta after passing or failing for G or F given tau
    if (pf == "p" && GF == "G") {
      P_H_p_G = (1-G_H(tau))*p/totalP(tau, "p", "G")
      post = P_H_p_G*theta_h + (1-P_H_p_G)*theta_l
      return(post)
    } else if (pf == "f" && GF == "G") {
      P_H_f_G = G_H(tau)*p/totalP(tau, "f", "G")
      post = P_H_f_G*theta_h + (1-P_H_f_G)*theta_l
      return(post)
    } else if (pf == "f" && GF == "F") {
      P_H_f_F = F_H(tau)*p/totalP(tau, "f", "F")
      post = P_H_f_F*theta_h + (1-P_H_f_F)*theta_l
      return(post)
    } else if (pf == "p" && GF == "F") {
      P_H_p_F = (1-F_H(tau))*p/totalP(tau, "p", "F")
      post = P_H_p_F*theta_h + (1-P_H_p_F)*theta_l
      return(post)
    }
  }
  
  action = function(post_theta){
    #criterion to test whether action is undertaken
    if (post_theta >= c/beta){
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
  
  post_theta_p_G = post_theta(tau, "p", "G")
  post_theta_f_G = post_theta(tau, "f", "G")
  post_theta_p_F = post_theta(tau, "p", "F")
  post_theta_f_F = post_theta(tau, "f", "F")
  
  #create table of decisions for each combination of p/f and F/G for given tau
  post_thetas = c(post_theta_p_G,post_theta_f_G,post_theta_p_F,post_theta_f_F)
  df = data.frame(post_thetas)
  df$action = apply(df, MARGIN=1, FUN=function(x2) action(x2))
  rownames(df) = c("p | G", "f | G", "p | F", "f | F")
  
  outcome = function(df){
    #calculate expected utility for each outcome
    if (df[1,2] == TRUE && df[2,2] == TRUE){
      #do for G in both cases
      utility_G = totalP(tau, "p", "G")*(df[1,1]-c) + totalP(tau, "f", "G")*(df[2,1]-c)
    }else if (df[1,2] == TRUE && df[2,2] == FALSE){
      #do for G only after pass
      utility_G = totalP(tau, "p", "G")*(df[1,1]-c)
    }else if (df[1,2] == FALSE && df[2,2] == TRUE){
      #do for G only after fail
      utility_G = totalP(tau, "f", "G")*(df[2,1]-c)
    }else {
      utility_G = 0
    }
    if (df[3,2] == TRUE && df[4,2] == TRUE){
      #do for G in both cases
      utility_F = totalP(tau, "p", "F")*(df[3,1]-c) + totalP(tau, "f", "F")*(df[4,1]-c)
    }else if (df[3,2] == TRUE && df[4,2] == FALSE){
      #do for G only after pass
      utility_F = totalP(tau, "p", "F")*(df[3,1]-c)
    }else if (df[3,2] == FALSE && df[4,2] == TRUE){
      #do for G only after fail
      utility_F = totalP(tau, "f", "F")*(df[4,1]-c)
    }else if (df[3,2] == FALSE && df[4,2] == FALSE){
      utility_F = 0
    }
    utility = c(utility_G, utility_F, utility_G-utility_F)
    result = data.frame(utility)
    rownames(result) = c("L", "NL", "Diff")
    return(result)
  }
  
  values = outcome(df)
  return(values)
}


#single test - set values
c = 0.4
beta = 0.7
q = 0.6
theta_h = 0.8
theta_l = 0.3

#define set of x (tau) values - 0 wont work because of division error
X = seq(0.001, 0.999, length.out = 1000)
#dataframe with values
plot.frame = data.frame(X)
plot.frame$df = apply(plot.frame, MARGIN=1, FUN=function(x2) calculate(c, beta, x2, q, theta_h, theta_l))
#extract avlues
test = plot.frame
test$diff = 0
test$NL = 0
test$L = 0
for (i in 1:1000){
  test$diff[i] = unlist(test$df[i])[3]
  test$NL[i] = unlist(test$df[i])[2]
  test$L[i] = unlist(test$df[i])[1]
}
#set description for plot
description = paste("c = ", c, ", theta_H = ", theta_h, ", theta_L = ", theta_l, ", q = ", q, ", beta = ", beta, sep = "")

#create plot
plot_tau = ggplot(test, aes(X, y=value, color = variable)) +
  geom_line(aes(y=diff, col = "Diff")) +
  geom_line(aes(y=L, col = "L")) +
  geom_line(aes(y=NL, col = "NL")) +
  labs(title = "Decisions for varying test-difficulty", x = "Difficulty in (0,1)", y = "Expected utility", caption = description) +
  theme(plot.caption = element_text(size=7))
plot_tau + coord_fixed(ratio = 2)


#for loop to generate and save a series of plots with (c.p.) different values for one parameter

for (l in 1:10){
  #set parameter to be adjusted
  q = l/10
  #define set of x (tau) values - 0 wont work because of division error
  X = seq(0.001, 0.999, length.out = 1000)
  #dataframe with values
  plot.frame = data.frame(X)
  plot.frame$df = apply(plot.frame, MARGIN=1, FUN=function(x2) calculate(c, beta, x2, q, theta_h, theta_l))
  #extract values
  test = plot.frame
  test$diff = 0
  test$NL = 0
  test$L = 0
  for (i in 1:1000){
    test$diff[i] = unlist(test$df[i])[3]
    test$NL[i] = unlist(test$df[i])[2]
    test$L[i] = unlist(test$df[i])[1]
  }
  #set filename for output
  filen = paste("PATH", l, ".pdf", sep ="")
  #create description for plot
  description = paste("c = ", c, ", theta_H = ", theta_h, ", theta_L = ", theta_l, ", q = ", q, ", beta = ", beta, sep = "")
  
  #create plot
  plot_tau = ggplot(test, aes(X, y=value, color = variable)) +
    geom_line(aes(y=diff, col = "Diff")) +
    geom_line(aes(y=L, col = "L")) +
    geom_line(aes(y=NL, col = "NL")) +
    labs(title = "Decisions for varying test-difficulty", x = "Difficulty in (0,1)", y = "Expected utility", caption = description) +
    theme(plot.caption = element_text(size=7))
  plot_tau + coord_fixed(ratio = 2)
  #save plot
  pdf(filen)
  print(plot_tau)
  dev.off()
}