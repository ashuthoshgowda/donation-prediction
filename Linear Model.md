# donation-prediction
#This Dataset contains data of the People to whom the Mailings were sent, requesting Donation.Using the Data from the previous years, Predicting the Donation Amounts of the 
df<-FINAL.DATASET.VERSION.3.1
fullmodel<-(lm(df$TARGET_D~.,data = df))
summary(fullmodel)
minmodel<-lm(df$TARGET_D~1,data = df)
fwdmodel=step(minmodel,direction = 'forward',scope = fullmodel)
summary(fwdmodel)
# Select the Variables picked out by Forward Step and use them to build a Linear Model.
finalmodel<- lm(df$TARGET_D ~ RFA_2A + RFA_2F + LASTGIFT + RFA13right + PROM1 + 
                  RAMNTALL + RAMNT_7 + RAMNT_11 + MAXRAMNT + RAMNT_16 + CARDPM12 + 
                  RFA3left + RAMNT_14 + RAMNT_9 + RAMNT_8 + RAMNT_12 + RAMNT_15 + 
                  RAMNT_13 + RFA3mid + RAMNT_10 + RFA5left + RAMNT_6 + pc_18 + 
                  INCOME + RAMNT_18 + pc_13 + pc_22 + RAMNT_21 + AVGGIFT + 
                  pc_11 + PEPSTRFL + pc_7 + PROM2 + pc_2 + pc_27 + pc_31 + 
                  pc_21 + pc_5 + HPHONE_D ,data = df)
summary(finalmodel)
#Alternatively  we can use the STEPAIC to select the best combination of Variables.
step<-stepAIC(fullmodel,direction = "both")

# Select the Variables picked out by Forward Step and use them to build a Linear Model.
refinedmodel<- lm(df$TARGET_D ~ DOM1 + INCOME + PROM1 + PROM2 + CARDPM12 + LASTGIFT + 
                    AVGGIFT + HPHONE_D + RFA_2F + RFA_2A + RFA3left + RFA3mid + 
                    RFA5left + RFA13right + pc_1 + pc_2 + pc_5 + pc_11 + pc_13 + 
                    pc_15 + pc_18 + pc_22 + pc_27 + pc_28 + pc_31 + RAMNTALL + 
                    MAXRAMNT + RAMNT_6 + RAMNT_7 + RAMNT_8 + RAMNT_9 + RAMNT_10 + 
                    RAMNT_11 + RAMNT_12 + RAMNT_13 + RAMNT_14 + RAMNT_15 + RAMNT_16 + 
                    RAMNT_18 + RAMNT_21,data = df)

summary(refinedmodel)

td<-predict.lm(refinedmodel,newdf)
