## SKILL LIST STRUCTURE
## DATA FRAME
## SKILLNAME(character) / DELAY(numeric, ms) / COOLTIME(numeric, ms) / COOLRESETAPPLICABLE(logical) / CALCULATEDDMG (numeric)

ExSkills <- data.frame(SkillName=c("Skill1", "Skill2", "Skill3", "Skill4", "Skill5"), 
                       Delay=c(300, 510, 600, 630, 720), 
                       CoolTime=c(0, 15000, 9000, 10000, 12000), 
                       CoolResetApplicable=c(T, T, T, T, F), 
                       CalculatedDMG=c(20, 80, 120, 160, 220))


## DEALCYCLE STRATEGY


## TIME-VARYING MARKOV CHAIN
## VARIABLES : Cool_Reset / Skill_CoolTimes / Remained_Time / Before_Status / Status
## VARIABLES : Cool_Reset_Try / Skill_Probs / Skill_Times

## Init_Values
Skill1_data <- data.frame(Iter_0 = rep(0, 2000))
Skill2_data <- data.frame(Iter_0 = rep(0, 2000))
Skill3_data <- data.frame(Iter_0 = rep(0, 2000))
Skill4_data <- data.frame(Iter_0 = rep(0, 2000))
Skill5_data <- data.frame(Iter_0 = rep(0, 2000))

for(j in 1:1) {
  set.seed(j * 10)
  Init_Values <- data.frame(Cool_Reset = .2, 
                            Skill2_CoolTime = 0, 
                            Skill3_CoolTime = 0, 
                            Skill4_CoolTime = 0, 
                            Skill5_CoolTime = 12000 - 30, 
                            
                            Remained_Time = 720 - 30, 
                            Before_Status = 0, 
                            Status = 5, 
                            
                            Cool_Reset_Try = rbinom(1, 1, .2), 
                            
                            Skill1_Times = 0, 
                            Skill2_Times = 0, 
                            Skill3_Times = 0, 
                            Skill4_Times = 0, 
                            Skill5_Times = 1)
  
  ## Rules
  Iters <- Init_Values
  Times_1 <- Init_Values$Skill1_Times
  Times_2 <- Init_Values$Skill2_Times
  Times_3 <- Init_Values$Skill3_Times
  Times_4 <- Init_Values$Skill4_Times
  Times_5 <- Init_Values$Skill5_Times
  
  for(i in 1:200000) {
    Iters <- rbind(Iters, Iters)
    
    Iters$Before_Status[nrow(Iters)] <- Iters$Status[nrow(Iters)-1]
    Iters$Status[nrow(Iters)] <- ifelse(Iters$Remained_Time[nrow(Iters)-1] == 0, 
                                        ifelse(Iters$Skill5_CoolTime[nrow(Iters)-1] == 0, 5, 
                                               ifelse(Iters$Skill4_CoolTime[nrow(Iters)-1] == 0, 4, 
                                                      ifelse(Iters$Skill3_CoolTime[nrow(Iters)-1] == 0, 3, 
                                                             ifelse(Iters$Skill2_CoolTime[nrow(Iters)-1] == 0, 2, 1)))), Iters$Status[nrow(Iters)-1])
    Iters$Cool_Reset_Try[nrow(Iters)] <- rbinom(1, 1, Iters$Cool_Reset[nrow(Iters)-1])
    
    Iters$Skill2_CoolTime[nrow(Iters)] <- ifelse(Iters$Remained_Time[nrow(Iters)-1] == 0 & Iters$Status[nrow(Iters)] == 2, 
                                                 ifelse(Iters$Cool_Reset_Try[nrow(Iters)]==1, 0, 15000 - 30), 
                                                 max(0, Iters$Skill2_CoolTime[nrow(Iters)-1] - 30))
    Iters$Skill3_CoolTime[nrow(Iters)] <- ifelse(Iters$Remained_Time[nrow(Iters)-1] == 0 & Iters$Status[nrow(Iters)] == 3, 
                                                 ifelse(Iters$Cool_Reset_Try[nrow(Iters)]==1, 0, 9000 - 30), 
                                                 max(0, Iters$Skill3_CoolTime[nrow(Iters)-1] - 30))
    Iters$Skill4_CoolTime[nrow(Iters)] <- ifelse(Iters$Remained_Time[nrow(Iters)-1] == 0 & Iters$Status[nrow(Iters)] == 4, 
                                                 ifelse(Iters$Cool_Reset_Try[nrow(Iters)]==1, 0, 10000 - 30), 
                                                 max(0, Iters$Skill4_CoolTime[nrow(Iters)-1] - 30))
    Iters$Skill5_CoolTime[nrow(Iters)] <- ifelse(Iters$Remained_Time[nrow(Iters)-1] == 0 & Iters$Status[nrow(Iters)] == 5, 
                                                 12000 - 30, 
                                                 max(0, Iters$Skill5_CoolTime[nrow(Iters)-1] - 30))
    
    Iters$Remained_Time[nrow(Iters)] <- ifelse(Iters$Remained_Time[nrow(Iters)-1] == 0, 
                                               ifelse(Iters$Status[nrow(Iters)] == 5, 720 - 30, 
                                                      ifelse(Iters$Status[nrow(Iters)] == 4, 630 - 30, 
                                                             ifelse(Iters$Status[nrow(Iters)] == 3, 600 - 30, 
                                                                    ifelse(Iters$Status[nrow(Iters)] == 2, 510 - 30, 300 - 30)))), Iters$Remained_Time[nrow(Iters)-1] - 30)
    
    Iters$Skill1_Times[nrow(Iters)] <- ifelse(Iters$Status[nrow(Iters)]==1, Iters$Skill1_Times[nrow(Iters)-1] + 1, Iters$Skill1_Times[nrow(Iters)-1])
    Iters$Skill2_Times[nrow(Iters)] <- ifelse(Iters$Status[nrow(Iters)]==2, Iters$Skill2_Times[nrow(Iters)-1] + 1, Iters$Skill2_Times[nrow(Iters)-1])
    Iters$Skill3_Times[nrow(Iters)] <- ifelse(Iters$Status[nrow(Iters)]==3, Iters$Skill3_Times[nrow(Iters)-1] + 1, Iters$Skill3_Times[nrow(Iters)-1])
    Iters$Skill4_Times[nrow(Iters)] <- ifelse(Iters$Status[nrow(Iters)]==4, Iters$Skill4_Times[nrow(Iters)-1] + 1, Iters$Skill4_Times[nrow(Iters)-1])
    Iters$Skill5_Times[nrow(Iters)] <- ifelse(Iters$Status[nrow(Iters)]==5, Iters$Skill5_Times[nrow(Iters)-1] + 1, Iters$Skill5_Times[nrow(Iters)-1])
    
    if(i / 100 == round(i / 100)) {
      Times_1 <- c(Times_1, Iters$Skill1_Times[nrow(Iters)])
      Times_2 <- c(Times_2, Iters$Skill2_Times[nrow(Iters)])
      Times_3 <- c(Times_3, Iters$Skill3_Times[nrow(Iters)])
      Times_4 <- c(Times_4, Iters$Skill4_Times[nrow(Iters)])
      Times_5 <- c(Times_5, Iters$Skill5_Times[nrow(Iters)])
    }
    
    Iters <- Iters[-1, ]
    
    if(round(i / 1000) == i / 1000) {
      print(paste(i, "th iteration done", sep=""))
    }
  }
  
  rownames(Iters) <- 1:nrow(Iters)
  
  for(i in 1:length(Times_1)) {
    Times_1[i] <- (Times_1[i]) / (i * 100)
    Times_2[i] <- (Times_2[i]) / (i * 100)
    Times_3[i] <- (Times_3[i]) / (i * 100)
    Times_4[i] <- (Times_4[i]) / (i * 100)
    Times_5[i] <- (Times_5[i]) / (i * 100)
  }
  
  Skill1_data <- cbind(Skill1_data, Times_1[2:length(Times_2)]) ; colnames(Skill1_data)[j+1] <- paste("Iter_", j, sep="")
  Skill2_data <- cbind(Skill2_data, Times_2[2:length(Times_2)]) ; colnames(Skill2_data)[j+1] <- paste("Iter_", j, sep="")
  Skill3_data <- cbind(Skill3_data, Times_3[2:length(Times_3)]) ; colnames(Skill3_data)[j+1] <- paste("Iter_", j, sep="")
  Skill4_data <- cbind(Skill4_data, Times_4[2:length(Times_4)]) ; colnames(Skill4_data)[j+1] <- paste("Iter_", j, sep="")
  Skill5_data <- cbind(Skill5_data, Times_5[2:length(Times_5)]) ; colnames(Skill5_data)[j+1] <- paste("Iter_", j, sep="")
}

plot(Skill1_data[, 2], type="l", col="Black", lwd=2, xlab="Iteration", ylab="Rate", main="Skill1", ylim=c(0.5, 0.8))
lines(Skill1_data[, 3], type="l", col="Gray60", lwd=2)
lines(Skill1_data[, 4], type="l", col="Gray20", lwd=2)
lines(Skill1_data[, 5], type="l", col="Skyblue", lwd=2)
lines(Skill1_data[, 6], type="l", col="Navy", lwd=2)
lines(Skill1_data2[, 2], type="l", col="Red", lwd=2)

plot(Skill2_data[, 2], type="l", col="Black", lwd=2, xlab="Iteration", ylab="Rate", main="Skill2", ylim=c(0, 0.1))
lines(Skill2_data[, 3], type="l", col="Gray60", lwd=2)
lines(Skill2_data[, 4], type="l", col="Gray20", lwd=2)
lines(Skill2_data[, 5], type="l", col="Skyblue", lwd=2)
lines(Skill2_data[, 6], type="l", col="Navy", lwd=2)
lines(Skill2_data2[, 2], type="l", col="Red", lwd=2)

plot(Skill3_data[, 2], type="l", col="Black", lwd=2, xlab="Iteration", ylab="Rate", main="Skill3", ylim=c(0, 0.2))
lines(Skill3_data[, 3], type="l", col="Gray60", lwd=2)
lines(Skill3_data[, 4], type="l", col="Gray20", lwd=2)
lines(Skill3_data[, 5], type="l", col="Skyblue", lwd=2)
lines(Skill3_data[, 5], type="l", col="Navy", lwd=2)
lines(Skill3_data2[, 2], type="l", col="Red", lwd=2)

plot(Skill4_data[, 2], type="l", col="Black", lwd=2, xlab="Iteration", ylab="Rate", main="Skill4", ylim=c(0, 0.2))
lines(Skill4_data[, 3], type="l", col="Gray60", lwd=2)
lines(Skill4_data[, 4], type="l", col="Gray20", lwd=2)
lines(Skill4_data[, 5], type="l", col="Skyblue", lwd=2)
lines(Skill4_data[, 5], type="l", col="Navy", lwd=2)
lines(Skill4_data2[, 2], type="l", col="Red", lwd=2)

plot(Skill5_data[, 2], type="l", col="Black", lwd=2, xlab="Iteration", ylab="Rate", main="Skill5", ylim=c(0, 0.1))
lines(Skill5_data[, 3], type="l", col="Gray60", lwd=2)
lines(Skill5_data[, 4], type="l", col="Gray20", lwd=2)
lines(Skill5_data[, 5], type="l", col="Skyblue", lwd=2)
lines(Skill5_data[, 5], type="l", col="Navy", lwd=2)
lines(Skill5_data2[, 2], type="l", col="Red", lwd=2)