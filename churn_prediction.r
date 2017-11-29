churnTrain<-read.csv("train.csv")
nrow(churnTrain[which(churnTrain$Responders == 1),])

churnTrain<-churnTrain[,!names(churnTrain) %in% c("ATM_C_prev1","ATM_C_prev2","ATM_C_prev3","ATM_C_prev4","ATM_C_prev5","ATM_C_prev6","COUNT_ATM_C_prev1","COUNT_ATM_C_prev2","COUNT_ATM_C_prev3","COUNT_ATM_C_prev4","COUNT_ATM_C_prev5","COUNT_ATM_C_prev6")]
churnTrain$NO_OF_Accs<- ifelse(churnTrain$NO_OF_Accs > 5,6,churnTrain$NO_OF_Accs)

table(churnTrain$Responders,churnTrain$HNW_CATEGORY)
chisq.test(table(churnTrain$Responders,churnTrain$HNW_CATEGORY))

anyNA(churnTrain$vintage)
summary(aov(churnTrain$vintage~churnTrain$Responders))

anyNA(churnTrain$EMAIL_UNSUBSCRIBE)
str(churnTrain$EMAIL_UNSUBSCRIBE)
churnTrain$EMAIL_UNSUBSCRIBE <- ifelse(churnTrain$EMAIL_UNSUBSCRIBE == "Y", "Y", "N")
table(churnTrain$Responders,churnTrain$EMAIL_UNSUBSCRIBE)
chisq.test(table(churnTrain$Responders,churnTrain$EMAIL_UNSUBSCRIBE))

levels(churnTrain$OCCUP_ALL_NEW)
levels(churnTrain$OCCUP_ALL_NEW)[levels(churnTrain$OCCUP_ALL_NEW) == ""]<-"MISSING"
table(churnTrain$Responders,churnTrain$OCCUP_ALL_NEW)
chisq.test(table(churnTrain$Responders,churnTrain$OCCUP_ALL_NEW))

churnTrain<-churnTrain[,!names(churnTrain) %in% c("city","zip")]
names(churnTrain)

levels(churnTrain$FINAL_WORTH_prev1)
levels(churnTrain$FINAL_WORTH_prev1)[levels(churnTrain$FINAL_WORTH_prev1) == ""]<-"HIGH"
table(churnTrain$Responders,churnTrain$FINAL_WORTH_prev1)
chisq.test(table(churnTrain$Responders,churnTrain$FINAL_WORTH_prev1))

levels(churnTrain$ENGAGEMENT_TAG_prev1)
levels(churnTrain$ENGAGEMENT_TAG_prev1)[levels(churnTrain$ENGAGEMENT_TAG_prev1) == ""]<-"LOW"
table(churnTrain$Responders,churnTrain$ENGAGEMENT_TAG_prev1)
chisq.test(table(churnTrain$Responders,churnTrain$ENGAGEMENT_TAG_prev1))

churnTrain<-churnTrain[,!names(churnTrain) %in% c("MB_C_prev1","MB_C_prev2","MB_C_prev3","MB_C_prev4","MB_C_prev5","MB_C_prev6","COUNT_MB_C_prev1","COUNT_MB_C_prev2","COUNT_MB_C_prev3","COUNT_MB_C_prev4","COUNT_MB_C_prev5","COUNT_MB_C_prev6")]
names(churnTrain)

churnTrain<-churnTrain[,!names(churnTrain) %in% c("MB_D_prev1","MB_D_prev2","MB_D_prev3","MB_D_prev4","MB_D_prev5","MB_D_prev6")]

churnTrain<-churnTrain[,!names(churnTrain) %in% c("POS_C_prev1","POS_C_prev2","POS_C_prev3","POS_C_prev4","POS_C_prev5","POS_C_prev6","COUNT_POS_C_prev1","COUNT_POS_C_prev2","COUNT_POS_C_prev3","COUNT_POS_C_prev4","COUNT_POS_C_prev5","COUNT_POS_C_prev6")]

churnTrain<-churnTrain[,!names(churnTrain) %in% c("ATM_D_prev1","BRANCH_C_prev1","BRANCH_D_prev1","IB_C_prev1","IB_D_prev1","POS_D_prev1","COUNT_ATM_D_prev1",
"COUNT_BRANCH_C_prev1","COUNT_BRANCH_D_prev1","COUNT_IB_C_prev1","COUNT_IB_D_prev1","COUNT_MB_D_prev1","COUNT_POS_D_prev1")]

churnTrain<-churnTrain[,!names(churnTrain) %in% c("ATM_D_prev2","BRANCH_C_prev2","BRANCH_D_prev2","IB_C_prev2","IB_D_prev2","POS_D_prev2","COUNT_ATM_D_prev2",
                                                  "COUNT_BRANCH_C_prev2","COUNT_BRANCH_D_prev2","COUNT_IB_C_prev2","COUNT_IB_D_prev2","COUNT_MB_D_prev2","COUNT_POS_D_prev2")]

churnTrain<-churnTrain[,!names(churnTrain) %in% c("ATM_D_prev3","BRANCH_C_prev3","BRANCH_D_prev3","IB_C_prev3","IB_D_prev3","POS_D_prev3","COUNT_ATM_D_prev3",
                                                  "COUNT_BRANCH_C_prev3","COUNT_BRANCH_D_prev3","COUNT_IB_C_prev3","COUNT_IB_D_prev3","COUNT_MB_D_prev3","COUNT_POS_D_prev3")]

churnTrain<-churnTrain[,!names(churnTrain) %in% c("ATM_D_prev4","BRANCH_C_prev4","BRANCH_D_prev4","IB_C_prev4","IB_D_prev4","POS_D_prev4","COUNT_ATM_D_prev4","COUNT_BRANCH_C_prev4","COUNT_BRANCH_D_prev4","COUNT_IB_C_prev4","COUNT_IB_D_prev4","COUNT_MB_D_prev4","COUNT_POS_D_prev4")]

churnTrain<-churnTrain[,!names(churnTrain) %in% c("ATM_D_prev5","BRANCH_C_prev5","BRANCH_D_prev5","IB_C_prev5","IB_D_prev5","POS_D_prev5","COUNT_ATM_D_prev5","COUNT_BRANCH_C_prev5","COUNT_BRANCH_D_prev5","COUNT_IB_C_prev5","COUNT_IB_D_prev5","COUNT_MB_D_prev5","COUNT_POS_D_prev5")]

churnTrain<-churnTrain[,!names(churnTrain) %in% c("ATM_D_prev6","BRANCH_C_prev6","BRANCH_D_prev6","IB_C_prev6","IB_D_prev6","POS_D_prev6","COUNT_ATM_D_prev6","COUNT_BRANCH_C_prev6","COUNT_BRANCH_D_prev6","COUNT_IB_C_prev6","COUNT_IB_D_prev6","COUNT_MB_D_prev6","COUNT_POS_D_prev6")]

churnTrain$dependents<- ifelse(churnTrain$dependents > 5,6,churnTrain$dependents)
table(churnTrain$Responders,churnTrain$dependents)
str(churnTrain$dependents)

churnTrain[is.na(churnTrain$dependents),]$dependents<-0
levels(churnTrain$FINAL_WORTH_prev1)
table(churnTrain$Responders,churnTrain$FINAL_WORTH_prev1)

churnTrain<-churnTrain[,!names(churnTrain) %in% c("ATM_amt_prev1","ATM_CW_Amt_prev1","ATM_CW_Cnt_prev1","BRN_CW_Amt_prev1","BRN_CW_Cnt_prev1","BRN_CASH_Dep_Amt_prev1","BRN_CASH_Dep_Cnt_prev1")]

churnTrain<-churnTrain[,!names(churnTrain) %in% c("ATM_amt_prev2","ATM_CW_Amt_prev2","ATM_CW_Cnt_prev2","BRN_CW_Amt_prev2","BRN_CW_Cnt_prev2","BRN_CASH_Dep_Amt_prev2","BRN_CASH_Dep_Cnt_prev2")]

churnTrain<-churnTrain[,!names(churnTrain) %in% c("ATM_amt_prev3","ATM_CW_Amt_prev3","ATM_CW_Cnt_prev3","BRN_CW_Amt_prev3","BRN_CW_Cnt_prev3","BRN_CASH_Dep_Amt_prev3","BRN_CASH_Dep_Cnt_prev3")]

churnTrain<-churnTrain[,!names(churnTrain) %in% c("ATM_amt_prev4","ATM_CW_Amt_prev4","ATM_CW_Cnt_prev4","BRN_CW_Amt_prev4","BRN_CW_Cnt_prev4","BRN_CASH_Dep_Amt_prev4","BRN_CASH_Dep_Cnt_prev4")]

churnTrain<-churnTrain[,!names(churnTrain) %in% c("ATM_amt_prev5","ATM_CW_Amt_prev5","ATM_CW_Cnt_prev5","BRN_CW_Amt_prev5","BRN_CW_Cnt_prev5","BRN_CASH_Dep_Amt_prev5","BRN_CASH_Dep_Cnt_prev5")]

churnTrain<-churnTrain[,!names(churnTrain) %in% c("ATM_amt_prev6","ATM_CW_Amt_prev6","ATM_CW_Cnt_prev6","BRN_CW_Amt_prev6","BRN_CW_Cnt_prev6","BRN_CASH_Dep_Amt_prev6","BRN_CASH_Dep_Cnt_prev6")]

names(churnTrain)

anyNA(churnTrain$custinit_CR_amt_prev1)

churnTrain[is.na(churnTrain$custinit_CR_amt_prev1),]$custinit_CR_amt_prev1<-0
churnTrain[is.na(churnTrain$custinit_DR_amt_prev1),]$custinit_DR_amt_prev1<-0
churnTrain[is.na(churnTrain$custinit_CR_cnt_prev1),]$custinit_CR_cnt_prev1<-0
churnTrain[is.na(churnTrain$custinit_DR_cnt_prev1),]$custinit_DR_cnt_prev1<-0

churnTrain[is.na(churnTrain$custinit_CR_amt_prev2),]$custinit_CR_amt_prev2<-0
churnTrain[is.na(churnTrain$custinit_DR_amt_prev2),]$custinit_DR_amt_prev2<-0
churnTrain[is.na(churnTrain$custinit_CR_cnt_prev2),]$custinit_CR_cnt_prev2<-0
churnTrain[is.na(churnTrain$custinit_DR_cnt_prev2),]$custinit_DR_cnt_prev2<-0

churnTrain[is.na(churnTrain$custinit_CR_amt_prev3),]$custinit_CR_amt_prev3<-0
churnTrain[is.na(churnTrain$custinit_DR_amt_prev3),]$custinit_DR_amt_prev3<-0
churnTrain[is.na(churnTrain$custinit_CR_cnt_prev3),]$custinit_CR_cnt_prev3<-0
churnTrain[is.na(churnTrain$custinit_DR_cnt_prev3),]$custinit_DR_cnt_prev3<-0

churnTrain[is.na(churnTrain$custinit_CR_amt_prev4),]$custinit_CR_amt_prev4<-0
churnTrain[is.na(churnTrain$custinit_DR_amt_prev4),]$custinit_DR_amt_prev4<-0
churnTrain[is.na(churnTrain$custinit_CR_cnt_prev4),]$custinit_CR_cnt_prev4<-0
churnTrain[is.na(churnTrain$custinit_DR_cnt_prev4),]$custinit_DR_cnt_prev4<-0

churnTrain[is.na(churnTrain$custinit_CR_amt_prev5),]$custinit_CR_amt_prev5<-0
churnTrain[is.na(churnTrain$custinit_DR_amt_prev5),]$custinit_DR_amt_prev5<-0
churnTrain[is.na(churnTrain$custinit_CR_cnt_prev5),]$custinit_CR_cnt_prev5<-0
churnTrain[is.na(churnTrain$custinit_DR_cnt_prev5),]$custinit_DR_cnt_prev5<-0

churnTrain[is.na(churnTrain$custinit_CR_amt_prev6),]$custinit_CR_amt_prev6<-0
churnTrain[is.na(churnTrain$custinit_DR_amt_prev6),]$custinit_DR_amt_prev6<-0
churnTrain[is.na(churnTrain$custinit_CR_cnt_prev6),]$custinit_CR_cnt_prev6<-0
churnTrain[is.na(churnTrain$custinit_DR_cnt_prev6),]$custinit_DR_cnt_prev6<-0


churnTrain<-churnTrain[,!names(churnTrain) %in% "FRX_PrevQ1"]
chisq.test(table(churnTrain$Responders,churnTrain$FRX_PrevQ1_N))
churnTrain<-churnTrain[,!names(churnTrain) %in% "FRX_PrevQ1_N"]

chisq.test(table(churnTrain$Responders,churnTrain$EFT_SELF_TRANSFER_PrevQ1))

chisq.test(table(churnTrain$Responders,churnTrain$Billpay_Active_PrevQ1_N))
chisq.test(table(churnTrain$Responders,churnTrain$Billpay_Reg_ason_Prev1_N))
churnTrain<-churnTrain[,!names(churnTrain) %in% c("Billpay_Active_PrevQ1_N","Billpay_Reg_ason_Prev1_N")]

anyNA(churnTrain$NO_OF_FD_BOOK_PrevQ1)

churnTrain[is.na(churnTrain$NO_OF_FD_BOOK_PrevQ1),]$NO_OF_FD_BOOK_PrevQ1<-0
churnTrain[is.na(churnTrain$NO_OF_FD_BOOK_PrevQ2),]$NO_OF_FD_BOOK_PrevQ2<-0
churnTrain[is.na(churnTrain$NO_OF_RD_BOOK_PrevQ1),]$NO_OF_RD_BOOK_PrevQ1<-0
churnTrain[is.na(churnTrain$NO_OF_RD_BOOK_PrevQ2),]$NO_OF_RD_BOOK_PrevQ2<-0

churnTrain[is.na(churnTrain$count_No_of_MF_PrevQ1),]$count_No_of_MF_PrevQ1<-0
churnTrain[is.na(churnTrain$count_No_of_MF_PrevQ2),]$count_No_of_MF_PrevQ2<-0

churnTrain[is.na(churnTrain$AGRI_PREM_CLOSED_PREVQ1),]$AGRI_PREM_CLOSED_PREVQ1<-0
chisq.test(table(churnTrain$Responders,churnTrain$AGRI_PREM_CLOSED_PREVQ1))

churnTrain[is.na(churnTrain$AL_CNC_PREM_CLOSED_PREVQ1),]$AL_CNC_PREM_CLOSED_PREVQ1<-0
chisq.test(table(churnTrain$Responders,churnTrain$AL_CNC_PREM_CLOSED_PREVQ1))

churnTrain[is.na(churnTrain$AL_PREM_CLOSED_PREVQ1),]$AL_PREM_CLOSED_PREVQ1<-0
chisq.test(table(churnTrain$Responders,churnTrain$AL_PREM_CLOSED_PREVQ1))

churnTrain[is.na(churnTrain$BL_PREM_CLOSED_PREVQ1),]$BL_PREM_CLOSED_PREVQ1<-0
chisq.test(table(churnTrain$Responders,churnTrain$BL_PREM_CLOSED_PREVQ1))

churnTrain<-churnTrain[,!names(churnTrain) %in% "CC_PREM_CLOSED_PREVQ1"]

churnTrain[is.na(churnTrain$CE_PREM_CLOSED_PREVQ1),]$CE_PREM_CLOSED_PREVQ1<-0
chisq.test(table(churnTrain$Responders,churnTrain$CE_PREM_CLOSED_PREVQ1))

churnTrain<-churnTrain[,!names(churnTrain) %in% "CE_PREM_CLOSED_PREVQ1"]

churnTrain[is.na(churnTrain$CV_PREM_CLOSED_PREVQ1),]$CV_PREM_CLOSED_PREVQ1<-0
chisq.test(table(churnTrain$Responders,churnTrain$CV_PREM_CLOSED_PREVQ1))

churnTrain<-churnTrain[,!names(churnTrain) %in% "CV_PREM_CLOSED_PREVQ1"]

churnTrain<-churnTrain[,!names(churnTrain) %in% "EDU_PREM_CLOSED_PREVQ1"]

churnTrain[is.na(churnTrain$OTHER_LOANS_PREM_CLOSED_PREVQ1),]$OTHER_LOANS_PREM_CLOSED_PREVQ1<-0
chisq.test(table(churnTrain$Responders,churnTrain$OTHER_LOANS_PREM_CLOSED_PREVQ1))

churnTrain[is.na(churnTrain$PL_PREM_CLOSED_PREVQ1),]$PL_PREM_CLOSED_PREVQ1<-0
chisq.test(table(churnTrain$Responders,churnTrain$PL_PREM_CLOSED_PREVQ1))

churnTrain[is.na(churnTrain$RD_PREM_CLOSED_PREVQ1),]$RD_PREM_CLOSED_PREVQ1<-0
chisq.test(table(churnTrain$Responders,churnTrain$RD_PREM_CLOSED_PREVQ1))

churnTrain[is.na(churnTrain$FD_PREM_CLOSED_PREVQ1),]$FD_PREM_CLOSED_PREVQ1<-0
chisq.test(table(churnTrain$Responders,churnTrain$FD_PREM_CLOSED_PREVQ1))

churnTrain<-churnTrain[,!names(churnTrain) %in% "TL_PREM_CLOSED_PREVQ1"]

churnTrain[is.na(churnTrain$TWL_PREM_CLOSED_PREVQ1),]$TWL_PREM_CLOSED_PREVQ1<-0
chisq.test(table(churnTrain$Responders,churnTrain$TWL_PREM_CLOSED_PREVQ1))

churnTrain[is.na(churnTrain$AGRI_Closed_PrevQ1),]$AGRI_Closed_PrevQ1<-0
chisq.test(table(churnTrain$Responders,churnTrain$AGRI_Closed_PrevQ1))

churnTrain[is.na(churnTrain$AL_Closed_PrevQ1),]$AL_Closed_PrevQ1<-0
chisq.test(table(churnTrain$Responders,churnTrain$AL_Closed_PrevQ1))

churnTrain[is.na(churnTrain$AL_CNC_Closed_PrevQ1),]$AL_CNC_Closed_PrevQ1<-0
chisq.test(table(churnTrain$Responders,churnTrain$AL_CNC_Closed_PrevQ1))

churnTrain<-churnTrain[,!names(churnTrain) %in% "EDU_Closed_PrevQ1"]

names(churnTrain)

churnTrain[is.na(churnTrain$BL_Closed_PrevQ1),]$BL_Closed_PrevQ1<-0
churnTrain[is.na(churnTrain$CC_CLOSED_PREVQ1),]$CC_CLOSED_PREVQ1<-0
churnTrain[is.na(churnTrain$CE_Closed_PrevQ1),]$CE_Closed_PrevQ1<-0
churnTrain[is.na(churnTrain$CV_Closed_PrevQ1),]$CV_Closed_PrevQ1<-0
churnTrain[is.na(churnTrain$GL_Closed_PrevQ1),]$GL_Closed_PrevQ1<-0
churnTrain[is.na(churnTrain$OTHER_LOANS_Closed_PrevQ1),]$OTHER_LOANS_Closed_PrevQ1<-0
churnTrain[is.na(churnTrain$PL_Closed_PrevQ1),]$PL_Closed_PrevQ1<-0
churnTrain[is.na(churnTrain$RD_CLOSED_PREVQ1),]$RD_CLOSED_PREVQ1<-0
churnTrain[is.na(churnTrain$FD_CLOSED_PREVQ1),]$FD_CLOSED_PREVQ1<-0
churnTrain[is.na(churnTrain$TL_Closed_PrevQ1),]$TL_Closed_PrevQ1<-0
churnTrain[is.na(churnTrain$TWL_Closed_PrevQ1),]$TWL_Closed_PrevQ1<-0
churnTrain[is.na(churnTrain$DEMAT_CLOSED_PREV1YR),]$DEMAT_CLOSED_PREV1YR<-0
churnTrain[is.na(churnTrain$SEC_ACC_CLOSED_PREV1YR),]$SEC_ACC_CLOSED_PREV1YR<-0

levels(churnTrain$AGRI_TAG_LIVE)[levels(churnTrain$AGRI_TAG_LIVE) == ""]<-"N"
chisq.test(table(churnTrain$Responders,churnTrain$AGRI_TAG_LIVE))

churnTrain<-churnTrain[,!names(churnTrain) %in% "AGRI_TAG_LIVE"]

levels(churnTrain$AL_CNC_TAG_LIVE)[levels(churnTrain$AL_CNC_TAG_LIVE) == ""]<-"N"
chisq.test(table(churnTrain$Responders,churnTrain$AL_CNC_TAG_LIVE))

levels(churnTrain$AL_TAG_LIVE)[levels(churnTrain$AL_TAG_LIVE) == ""]<-"N"
levels(churnTrain$BL_TAG_LIVE)[levels(churnTrain$BL_TAG_LIVE) == ""]<-"N"
levels(churnTrain$CC_TAG_LIVE)[levels(churnTrain$CC_TAG_LIVE) == ""]<-"N"
levels(churnTrain$CE_TAG_LIVE)[levels(churnTrain$CE_TAG_LIVE) == ""]<-"N"
levels(churnTrain$CV_TAG_LIVE)[levels(churnTrain$CV_TAG_LIVE) == ""]<-"N"
levels(churnTrain$DEMAT_TAG_LIVE)[levels(churnTrain$DEMAT_TAG_LIVE) == ""]<-"N"
levels(churnTrain$EDU_TAG_LIVE)[levels(churnTrain$EDU_TAG_LIVE) == ""]<-"N"
levels(churnTrain$GL_TAG_LIVE)[levels(churnTrain$GL_TAG_LIVE) == ""]<-"N"
levels(churnTrain$HL_TAG_LIVE)[levels(churnTrain$HL_TAG_LIVE) == ""]<-"N"
levels(churnTrain$SEC_ACC_TAG_LIVE)[levels(churnTrain$SEC_ACC_TAG_LIVE) == ""]<-"N"
levels(churnTrain$INS_TAG_LIVE)[levels(churnTrain$INS_TAG_LIVE) == ""]<-"N"
levels(churnTrain$LAS_TAG_LIVE)[levels(churnTrain$LAS_TAG_LIVE) == ""]<-"N"
levels(churnTrain$MF_TAG_LIVE)[levels(churnTrain$MF_TAG_LIVE) == ""]<-"N"
levels(churnTrain$OTHER_LOANS_TAG_LIVE)[levels(churnTrain$OTHER_LOANS_TAG_LIVE) == ""]<-"N"
levels(churnTrain$PL_TAG_LIVE)[levels(churnTrain$PL_TAG_LIVE) == ""]<-"N"
levels(churnTrain$RD_TAG_LIVE)[levels(churnTrain$RD_TAG_LIVE) == ""]<-"N"
levels(churnTrain$FD_TAG_LIVE)[levels(churnTrain$FD_TAG_LIVE) == ""]<-"N"
levels(churnTrain$TL_TAG_LIVE)[levels(churnTrain$TL_TAG_LIVE) == ""]<-"N"
levels(churnTrain$TWL_TAG_LIVE)[levels(churnTrain$TWL_TAG_LIVE) == ""]<-"N"
levels(churnTrain$lap_tag_live)[levels(churnTrain$lap_tag_live) == ""]<-"N"
levels(churnTrain$TWL_TAG_LIVE)[levels(churnTrain$TWL_TAG_LIVE) == ""]<-"N"


churnTrain<-churnTrain[,!names(churnTrain) %in% c("AGRI_DATE","AL_CNC_DATE","AL_DATE","BL_DATE","CE_DATE","CV_DATE","EDU_DATE","GL_DATE","LAP_DATE","LAS_DATE","OTHER_LOANS_DATE","PL_DATE","TL_DATE","TWL_DATE")]

names(churnTrain)

churnTrain[is.na(churnTrain$Charges_cnt_PrevQ1),]$Charges_cnt_PrevQ1<-0

churnTrain<-churnTrain[,!names(churnTrain) %in% "Charges_cnt_PrevQ1_N"]

churnTrain[is.na(churnTrain$NO_OF_COMPLAINTS),]$NO_OF_COMPLAINTS<-0

churnTrain<-churnTrain[,!names(churnTrain) %in% "brn_code"]

churnTrain<-churnTrain[,!names(churnTrain) %in% c("Recency_of_CR_TXN","Recency_of_DR_TXN","Recency_of_IB_TXN","Recency_of_ATM_TXN","Recency_of_BRANCH_TXN","Recency_of_MB_TXN")]

churnTrain[is.na(churnTrain$Req_Logged_PrevQ1),]$Req_Logged_PrevQ1<-0
churnTrain[is.na(churnTrain$Req_Resolved_PrevQ1),]$Req_Resolved_PrevQ1<-0

churnTrain[is.na(churnTrain$Query_Logged_PrevQ1),]$Query_Logged_PrevQ1<-0

churnTrain$Query_Resolved_PrevQ1<-ifelse(churnTrain$Query_Resolved_PrevQ1 == ">",churnTrain$Query_Logged_PrevQ1,churnTrain$Query_Resolved_PrevQ1)

str(churnTrain$Query_Resolved_PrevQ1)
anyNA(churnTrain$Query_Resolved_PrevQ1)

churnTrain[is.na(churnTrain$Complaint_Logged_PrevQ1),]$Complaint_Logged_PrevQ1<-0

levels(churnTrain$Complaint_Resolved_PrevQ1)[levels(churnTrain$Complaint_Resolved_PrevQ1) == ""]<-"0"

churnTrain$Complaint_Resolved_PrevQ1<-ifelse(churnTrain$Complaint_Resolved_PrevQ1 == ">",churnTrain$Complaint_Logged_PrevQ1,churnTrain$Complaint_Resolved_PrevQ1)

levels(churnTrain$Query_Resolved_PrevQ1)[levels(churnTrain$Query_Resolved_PrevQ1) == ""]<-"0"

churnTrain[is.na(churnTrain$NO_OF_CHEQUE_BOUNCE_V1),]$NO_OF_CHEQUE_BOUNCE_V1<-0

churnTrain[is.na(churnTrain$Percent_Change_in_Credits),]$Percent_Change_in_Credits<-0

churnTrain[is.na(churnTrain$Percent_Change_in_FT_Bank),]$Percent_Change_in_FT_Bank<-0

churnTrain[is.na(churnTrain$Percent_Change_in_FT_outside),]$Percent_Change_in_FT_outside<-0

churnTrain[is.na(churnTrain$Percent_Change_in_Self_Txn),]$Percent_Change_in_Self_Txn<-0

churnTrain[is.na(churnTrain$Percent_Change_in_Big_Expenses),]$Percent_Change_in_Big_Expenses<-0

anyNA(churnTrain)

churnTrain<-churnTrain[,!names(churnTrain) %in% "UCIC_ID"]

colnames(churnTrain)[colSums(is.na(churnTrain)) > 0]

churnTrain$Query_Resolved_PrevQ1<-as.numeric(churnTrain$Query_Resolved_PrevQ1)
churnTrain$Req_Resolved_PrevQ1<-as.numeric(churnTrain$Req_Resolved_PrevQ1)


churnTrain<-churnTrain[,!names(churnTrain) %in% c("Billpay_Active_PrevQ1","Billpay_Reg_ason_Prev1","Recency_of_POS_TXN")]

churnTrain[is.na(churnTrain$Recency_of_Activity),]$Recency_of_Activity<-0

library(rpart)
library(RColorBrewer)
library(rattle)

names(churnTrain)

cart.ctrl = rpart.control(minsplit=1000, minbucket = 500, cp = 0, xval = 10)

CartM1<-rpart(formula = Responders ~.,data = churnTrain, method = "class", control = cart.ctrl)

Cprune<- prune(CartM1,cp=  CartM1$cptable[which.min(CartM1$cptable[,"xerror"]),"CP"])
fancyRpartPlot(Cprune, uniform=TRUE,  main="Pruned Tree")

#####Test Data######
################################
################################
################################

churnTest<-read.csv("test.csv")
churnTest<-churnTest[,!names(churnTest) %in% c("ATM_C_prev1","ATM_C_prev2","ATM_C_prev3","ATM_C_prev4","ATM_C_prev5","ATM_C_prev6","COUNT_ATM_C_prev1","COUNT_ATM_C_prev2","COUNT_ATM_C_prev3","COUNT_ATM_C_prev4","COUNT_ATM_C_prev5","COUNT_ATM_C_prev6")]
churnTest$NO_OF_Accs<- ifelse(churnTest$NO_OF_Accs > 5,6,churnTest$NO_OF_Accs)
churnTest$EMAIL_UNSUBSCRIBE <- ifelse(churnTest$EMAIL_UNSUBSCRIBE == "Y", "Y", "N")

levels(churnTest$OCCUP_ALL_NEW)[levels(churnTest$OCCUP_ALL_NEW) == ""]<-"MISSING"
churnTest<-churnTest[,!names(churnTest) %in% c("city","zip")]
levels(churnTest$FINAL_WORTH_prev1)[levels(churnTest$FINAL_WORTH_prev1) == ""]<-"HIGH"
levels(churnTest$ENGAGEMENT_TAG_prev1)[levels(churnTest$ENGAGEMENT_TAG_prev1) == ""]<-"LOW"
churnTest<-churnTest[,!names(churnTest) %in% c("MB_C_prev1","MB_C_prev2","MB_C_prev3","MB_C_prev4","MB_C_prev5","MB_C_prev6","COUNT_MB_C_prev1","COUNT_MB_C_prev2","COUNT_MB_C_prev3","COUNT_MB_C_prev4","COUNT_MB_C_prev5","COUNT_MB_C_prev6")]
churnTest<-churnTest[,!names(churnTest) %in% c("MB_D_prev1","MB_D_prev2","MB_D_prev3","MB_D_prev4","MB_D_prev5","MB_D_prev6")]
churnTest<-churnTest[,!names(churnTest) %in% c("POS_C_prev1","POS_C_prev2","POS_C_prev3","POS_C_prev4","POS_C_prev5","POS_C_prev6","COUNT_POS_C_prev1","COUNT_POS_C_prev2","COUNT_POS_C_prev3","COUNT_POS_C_prev4","COUNT_POS_C_prev5","COUNT_POS_C_prev6")]

churnTest<-churnTest[,!names(churnTest) %in% c("ATM_D_prev1","BRANCH_C_prev1","BRANCH_D_prev1","IB_C_prev1","IB_D_prev1","POS_D_prev1","COUNT_ATM_D_prev1",
                                               "COUNT_BRANCH_C_prev1","COUNT_BRANCH_D_prev1","COUNT_IB_C_prev1","COUNT_IB_D_prev1","COUNT_MB_D_prev1","COUNT_POS_D_prev1")]

churnTest<-churnTest[,!names(churnTest) %in% c("ATM_D_prev2","BRANCH_C_prev2","BRANCH_D_prev2","IB_C_prev2","IB_D_prev2","POS_D_prev2","COUNT_ATM_D_prev2",
                                               "COUNT_BRANCH_C_prev2","COUNT_BRANCH_D_prev2","COUNT_IB_C_prev2","COUNT_IB_D_prev2","COUNT_MB_D_prev2","COUNT_POS_D_prev2")]

churnTest<-churnTest[,!names(churnTest) %in% c("ATM_D_prev3","BRANCH_C_prev3","BRANCH_D_prev3","IB_C_prev3","IB_D_prev3","POS_D_prev3","COUNT_ATM_D_prev3",
                                               "COUNT_BRANCH_C_prev3","COUNT_BRANCH_D_prev3","COUNT_IB_C_prev3","COUNT_IB_D_prev3","COUNT_MB_D_prev3","COUNT_POS_D_prev3")]

churnTest<-churnTest[,!names(churnTest) %in% c("ATM_D_prev4","BRANCH_C_prev4","BRANCH_D_prev4","IB_C_prev4","IB_D_prev4","POS_D_prev4","COUNT_ATM_D_prev4","COUNT_BRANCH_C_prev4","COUNT_BRANCH_D_prev4","COUNT_IB_C_prev4","COUNT_IB_D_prev4","COUNT_MB_D_prev4","COUNT_POS_D_prev4")]

churnTest<-churnTest[,!names(churnTest) %in% c("ATM_D_prev5","BRANCH_C_prev5","BRANCH_D_prev5","IB_C_prev5","IB_D_prev5","POS_D_prev5","COUNT_ATM_D_prev5","COUNT_BRANCH_C_prev5","COUNT_BRANCH_D_prev5","COUNT_IB_C_prev5","COUNT_IB_D_prev5","COUNT_MB_D_prev5","COUNT_POS_D_prev5")]

churnTest<-churnTest[,!names(churnTest) %in% c("ATM_D_prev6","BRANCH_C_prev6","BRANCH_D_prev6","IB_C_prev6","IB_D_prev6","POS_D_prev6","COUNT_ATM_D_prev6","COUNT_BRANCH_C_prev6","COUNT_BRANCH_D_prev6","COUNT_IB_C_prev6","COUNT_IB_D_prev6","COUNT_MB_D_prev6","COUNT_POS_D_prev6")]

churnTest$dependents<- ifelse(churnTest$dependents > 5,6,churnTest$dependents)

churnTest[is.na(churnTest$dependents),]$dependents<-0

churnTest<-churnTest[,!names(churnTest) %in% c("ATM_amt_prev1","ATM_CW_Amt_prev1","ATM_CW_Cnt_prev1","BRN_CW_Amt_prev1","BRN_CW_Cnt_prev1","BRN_CASH_Dep_Amt_prev1","BRN_CASH_Dep_Cnt_prev1")]

churnTest<-churnTest[,!names(churnTest) %in% c("ATM_amt_prev2","ATM_CW_Amt_prev2","ATM_CW_Cnt_prev2","BRN_CW_Amt_prev2","BRN_CW_Cnt_prev2","BRN_CASH_Dep_Amt_prev2","BRN_CASH_Dep_Cnt_prev2")]

churnTest<-churnTest[,!names(churnTest) %in% c("ATM_amt_prev3","ATM_CW_Amt_prev3","ATM_CW_Cnt_prev3","BRN_CW_Amt_prev3","BRN_CW_Cnt_prev3","BRN_CASH_Dep_Amt_prev3","BRN_CASH_Dep_Cnt_prev3")]

churnTest<-churnTest[,!names(churnTest) %in% c("ATM_amt_prev4","ATM_CW_Amt_prev4","ATM_CW_Cnt_prev4","BRN_CW_Amt_prev4","BRN_CW_Cnt_prev4","BRN_CASH_Dep_Amt_prev4","BRN_CASH_Dep_Cnt_prev4")]

churnTest<-churnTest[,!names(churnTest) %in% c("ATM_amt_prev5","ATM_CW_Amt_prev5","ATM_CW_Cnt_prev5","BRN_CW_Amt_prev5","BRN_CW_Cnt_prev5","BRN_CASH_Dep_Amt_prev5","BRN_CASH_Dep_Cnt_prev5")]

churnTest<-churnTest[,!names(churnTest) %in% c("ATM_amt_prev6","ATM_CW_Amt_prev6","ATM_CW_Cnt_prev6","BRN_CW_Amt_prev6","BRN_CW_Cnt_prev6","BRN_CASH_Dep_Amt_prev6","BRN_CASH_Dep_Cnt_prev6")]

churnTest[is.na(churnTest$custinit_CR_amt_prev1),]$custinit_CR_amt_prev1<-0
churnTest[is.na(churnTest$custinit_DR_amt_prev1),]$custinit_DR_amt_prev1<-0
churnTest[is.na(churnTest$custinit_CR_cnt_prev1),]$custinit_CR_cnt_prev1<-0
churnTest[is.na(churnTest$custinit_DR_cnt_prev1),]$custinit_DR_cnt_prev1<-0

churnTest[is.na(churnTest$custinit_CR_amt_prev2),]$custinit_CR_amt_prev2<-0
churnTest[is.na(churnTest$custinit_DR_amt_prev2),]$custinit_DR_amt_prev2<-0
churnTest[is.na(churnTest$custinit_CR_cnt_prev2),]$custinit_CR_cnt_prev2<-0
churnTest[is.na(churnTest$custinit_DR_cnt_prev2),]$custinit_DR_cnt_prev2<-0

churnTest[is.na(churnTest$custinit_CR_amt_prev3),]$custinit_CR_amt_prev3<-0
churnTest[is.na(churnTest$custinit_DR_amt_prev3),]$custinit_DR_amt_prev3<-0
churnTest[is.na(churnTest$custinit_CR_cnt_prev3),]$custinit_CR_cnt_prev3<-0
churnTest[is.na(churnTest$custinit_DR_cnt_prev3),]$custinit_DR_cnt_prev3<-0

churnTest[is.na(churnTest$custinit_CR_amt_prev4),]$custinit_CR_amt_prev4<-0
churnTest[is.na(churnTest$custinit_DR_amt_prev4),]$custinit_DR_amt_prev4<-0
churnTest[is.na(churnTest$custinit_CR_cnt_prev4),]$custinit_CR_cnt_prev4<-0
churnTest[is.na(churnTest$custinit_DR_cnt_prev4),]$custinit_DR_cnt_prev4<-0

churnTest[is.na(churnTest$custinit_CR_amt_prev5),]$custinit_CR_amt_prev5<-0
churnTest[is.na(churnTest$custinit_DR_amt_prev5),]$custinit_DR_amt_prev5<-0
churnTest[is.na(churnTest$custinit_CR_cnt_prev5),]$custinit_CR_cnt_prev5<-0
churnTest[is.na(churnTest$custinit_DR_cnt_prev5),]$custinit_DR_cnt_prev5<-0

churnTest[is.na(churnTest$custinit_CR_amt_prev6),]$custinit_CR_amt_prev6<-0
churnTest[is.na(churnTest$custinit_DR_amt_prev6),]$custinit_DR_amt_prev6<-0
churnTest[is.na(churnTest$custinit_CR_cnt_prev6),]$custinit_CR_cnt_prev6<-0
churnTest[is.na(churnTest$custinit_DR_cnt_prev6),]$custinit_DR_cnt_prev6<-0

churnTest<-churnTest[,!names(churnTest) %in% "FRX_PrevQ1"]
churnTest<-churnTest[,!names(churnTest) %in% "FRX_PrevQ1_N"]

churnTest<-churnTest[,!names(churnTest) %in% c("Billpay_Active_PrevQ1_N","Billpay_Reg_ason_Prev1_N")]

churnTest[is.na(churnTest$NO_OF_FD_BOOK_PrevQ1),]$NO_OF_FD_BOOK_PrevQ1<-0
churnTest[is.na(churnTest$NO_OF_FD_BOOK_PrevQ2),]$NO_OF_FD_BOOK_PrevQ2<-0
churnTest[is.na(churnTest$NO_OF_RD_BOOK_PrevQ1),]$NO_OF_RD_BOOK_PrevQ1<-0
churnTest[is.na(churnTest$NO_OF_RD_BOOK_PrevQ2),]$NO_OF_RD_BOOK_PrevQ2<-0

churnTest[is.na(churnTest$count_No_of_MF_PrevQ1),]$count_No_of_MF_PrevQ1<-0
churnTest[is.na(churnTest$count_No_of_MF_PrevQ2),]$count_No_of_MF_PrevQ2<-0

churnTest[is.na(churnTest$AGRI_PREM_CLOSED_PREVQ1),]$AGRI_PREM_CLOSED_PREVQ1<-0

churnTest[is.na(churnTest$AL_CNC_PREM_CLOSED_PREVQ1),]$AL_CNC_PREM_CLOSED_PREVQ1<-0

churnTest[is.na(churnTest$AL_PREM_CLOSED_PREVQ1),]$AL_PREM_CLOSED_PREVQ1<-0

churnTest[is.na(churnTest$BL_PREM_CLOSED_PREVQ1),]$BL_PREM_CLOSED_PREVQ1<-0

churnTest<-churnTest[,!names(churnTest) %in% "CC_PREM_CLOSED_PREVQ1"]

churnTest[is.na(churnTest$CE_PREM_CLOSED_PREVQ1),]$CE_PREM_CLOSED_PREVQ1<-0

churnTest<-churnTest[,!names(churnTest) %in% "CE_PREM_CLOSED_PREVQ1"]

churnTest[is.na(churnTest$CV_PREM_CLOSED_PREVQ1),]$CV_PREM_CLOSED_PREVQ1<-0

churnTest<-churnTest[,!names(churnTest) %in% "CV_PREM_CLOSED_PREVQ1"]

churnTest<-churnTest[,!names(churnTest) %in% "EDU_PREM_CLOSED_PREVQ1"]

churnTest[is.na(churnTest$OTHER_LOANS_PREM_CLOSED_PREVQ1),]$OTHER_LOANS_PREM_CLOSED_PREVQ1<-0

churnTest[is.na(churnTest$PL_PREM_CLOSED_PREVQ1),]$PL_PREM_CLOSED_PREVQ1<-0

churnTest[is.na(churnTest$RD_PREM_CLOSED_PREVQ1),]$RD_PREM_CLOSED_PREVQ1<-0

churnTest[is.na(churnTest$FD_PREM_CLOSED_PREVQ1),]$FD_PREM_CLOSED_PREVQ1<-0

churnTest<-churnTest[,!names(churnTest) %in% "TL_PREM_CLOSED_PREVQ1"]

churnTest[is.na(churnTest$TWL_PREM_CLOSED_PREVQ1),]$TWL_PREM_CLOSED_PREVQ1<-0

churnTest[is.na(churnTest$AGRI_Closed_PrevQ1),]$AGRI_Closed_PrevQ1<-0

churnTest[is.na(churnTest$AL_Closed_PrevQ1),]$AL_Closed_PrevQ1<-0

churnTest[is.na(churnTest$AL_CNC_Closed_PrevQ1),]$AL_CNC_Closed_PrevQ1<-0

churnTest<-churnTest[,!names(churnTest) %in% "EDU_Closed_PrevQ1"]

churnTest[is.na(churnTest$BL_Closed_PrevQ1),]$BL_Closed_PrevQ1<-0
churnTest[is.na(churnTest$CC_CLOSED_PREVQ1),]$CC_CLOSED_PREVQ1<-0
churnTest[is.na(churnTest$CE_Closed_PrevQ1),]$CE_Closed_PrevQ1<-0
churnTest[is.na(churnTest$CV_Closed_PrevQ1),]$CV_Closed_PrevQ1<-0
churnTest[is.na(churnTest$GL_Closed_PrevQ1),]$GL_Closed_PrevQ1<-0
churnTest[is.na(churnTest$OTHER_LOANS_Closed_PrevQ1),]$OTHER_LOANS_Closed_PrevQ1<-0
churnTest[is.na(churnTest$PL_Closed_PrevQ1),]$PL_Closed_PrevQ1<-0
churnTest[is.na(churnTest$RD_CLOSED_PREVQ1),]$RD_CLOSED_PREVQ1<-0
churnTest[is.na(churnTest$FD_CLOSED_PREVQ1),]$FD_CLOSED_PREVQ1<-0
churnTest[is.na(churnTest$TL_Closed_PrevQ1),]$TL_Closed_PrevQ1<-0
churnTest[is.na(churnTest$TWL_Closed_PrevQ1),]$TWL_Closed_PrevQ1<-0
churnTest[is.na(churnTest$DEMAT_CLOSED_PREV1YR),]$DEMAT_CLOSED_PREV1YR<-0
churnTest[is.na(churnTest$SEC_ACC_CLOSED_PREV1YR),]$SEC_ACC_CLOSED_PREV1YR<-0

levels(churnTest$AGRI_TAG_LIVE)[levels(churnTest$AGRI_TAG_LIVE) == ""]<-"N"

churnTest<-churnTest[,!names(churnTest) %in% "AGRI_TAG_LIVE"]

levels(churnTest$AL_CNC_TAG_LIVE)[levels(churnTest$AL_CNC_TAG_LIVE) == ""]<-"N"

levels(churnTest$AL_TAG_LIVE)[levels(churnTest$AL_TAG_LIVE) == ""]<-"N"
levels(churnTest$BL_TAG_LIVE)[levels(churnTest$BL_TAG_LIVE) == ""]<-"N"
levels(churnTest$CC_TAG_LIVE)[levels(churnTest$CC_TAG_LIVE) == ""]<-"N"
levels(churnTest$CE_TAG_LIVE)[levels(churnTest$CE_TAG_LIVE) == ""]<-"N"
levels(churnTest$CV_TAG_LIVE)[levels(churnTest$CV_TAG_LIVE) == ""]<-"N"
levels(churnTest$DEMAT_TAG_LIVE)[levels(churnTest$DEMAT_TAG_LIVE) == ""]<-"N"
levels(churnTest$EDU_TAG_LIVE)[levels(churnTest$EDU_TAG_LIVE) == ""]<-"N"
levels(churnTest$GL_TAG_LIVE)[levels(churnTest$GL_TAG_LIVE) == ""]<-"N"
levels(churnTest$HL_TAG_LIVE)[levels(churnTest$HL_TAG_LIVE) == ""]<-"N"
levels(churnTest$SEC_ACC_TAG_LIVE)[levels(churnTest$SEC_ACC_TAG_LIVE) == ""]<-"N"
levels(churnTest$INS_TAG_LIVE)[levels(churnTest$INS_TAG_LIVE) == ""]<-"N"
levels(churnTest$LAS_TAG_LIVE)[levels(churnTest$LAS_TAG_LIVE) == ""]<-"N"
levels(churnTest$MF_TAG_LIVE)[levels(churnTest$MF_TAG_LIVE) == ""]<-"N"
levels(churnTest$OTHER_LOANS_TAG_LIVE)[levels(churnTest$OTHER_LOANS_TAG_LIVE) == ""]<-"N"
levels(churnTest$PL_TAG_LIVE)[levels(churnTest$PL_TAG_LIVE) == ""]<-"N"
levels(churnTest$RD_TAG_LIVE)[levels(churnTest$RD_TAG_LIVE) == ""]<-"N"
levels(churnTest$FD_TAG_LIVE)[levels(churnTest$FD_TAG_LIVE) == ""]<-"N"
levels(churnTest$TL_TAG_LIVE)[levels(churnTest$TL_TAG_LIVE) == ""]<-"N"
levels(churnTest$TWL_TAG_LIVE)[levels(churnTest$TWL_TAG_LIVE) == ""]<-"N"
levels(churnTest$lap_tag_live)[levels(churnTest$lap_tag_live) == ""]<-"N"
levels(churnTest$TWL_TAG_LIVE)[levels(churnTest$TWL_TAG_LIVE) == ""]<-"N"

churnTest<-churnTest[,!names(churnTest) %in% c("AGRI_DATE","AL_CNC_DATE","AL_DATE","BL_DATE","CE_DATE","CV_DATE","EDU_DATE","GL_DATE","LAP_DATE","LAS_DATE","OTHER_LOANS_DATE","PL_DATE","TL_DATE","TWL_DATE")]

names(churnTest)

churnTest[is.na(churnTest$Charges_cnt_PrevQ1),]$Charges_cnt_PrevQ1<-0

churnTest<-churnTest[,!names(churnTest) %in% "Charges_cnt_PrevQ1_N"]

churnTest[is.na(churnTest$NO_OF_COMPLAINTS),]$NO_OF_COMPLAINTS<-0

churnTest<-churnTest[,!names(churnTest) %in% "brn_code"]

churnTest<-churnTest[,!names(churnTest) %in% c("Recency_of_CR_TXN","Recency_of_DR_TXN","Recency_of_IB_TXN","Recency_of_ATM_TXN","Recency_of_BRANCH_TXN","Recency_of_MB_TXN")]

churnTest[is.na(churnTest$Req_Logged_PrevQ1),]$Req_Logged_PrevQ1<-0
levels(churnTest$Req_Resolved_PrevQ1)[levels(churnTest$Req_Resolved_PrevQ1) == ""]<-"0"
churnTest[is.na(churnTest$Query_Logged_PrevQ1),]$Query_Logged_PrevQ1<-0

churnTest$Req_Resolved_PrevQ1<-ifelse(churnTest$Req_Resolved_PrevQ1 == ">",churnTest$Req_Logged_PrevQ1,churnTest$Req_Resolved_PrevQ1)
churnTest[is.na(churnTest$Query_Logged_PrevQ1),]$Query_Logged_PrevQ1<-0
churnTest$Query_Resolved_PrevQ1<-ifelse(churnTest$Query_Resolved_PrevQ1 == ">",churnTest$Query_Logged_PrevQ1,churnTest$Query_Resolved_PrevQ1)
levels(churnTest$Query_Resolved_PrevQ1)[levels(churnTest$Query_Resolved_PrevQ1) == ""]<-"0"

churnTest[is.na(churnTest$Complaint_Logged_PrevQ1),]$Complaint_Logged_PrevQ1<-0

levels(churnTest$Complaint_Resolved_PrevQ1)[levels(churnTest$Complaint_Resolved_PrevQ1) == ""]<-"0"

churnTest$Complaint_Resolved_PrevQ1<-ifelse(churnTest$Complaint_Resolved_PrevQ1 == ">",churnTest$Complaint_Logged_PrevQ1,churnTest$Complaint_Resolved_PrevQ1)

churnTest[is.na(churnTest$NO_OF_CHEQUE_BOUNCE_V1),]$NO_OF_CHEQUE_BOUNCE_V1<-0

churnTest[is.na(churnTest$Percent_Change_in_Credits),]$Percent_Change_in_Credits<-0

churnTest[is.na(churnTest$Percent_Change_in_FT_Bank),]$Percent_Change_in_FT_Bank<-0

churnTest[is.na(churnTest$Percent_Change_in_FT_outside),]$Percent_Change_in_FT_outside<-0

churnTest[is.na(churnTest$Percent_Change_in_Self_Txn),]$Percent_Change_in_Self_Txn<-0

churnTest[is.na(churnTest$Percent_Change_in_Big_Expenses),]$Percent_Change_in_Big_Expenses<-0

anyNA(churnTest)

colnames(churnTest)[colSums(is.na(churnTest)) > 0]
churnTest<-churnTest[,!names(churnTest) %in% "UCIC_ID"]
churnTest<-churnTest[,!names(churnTest) %in% c("Billpay_Active_PrevQ1","Billpay_Reg_ason_Prev1","Recency_of_POS_TXN")]
churnTest[is.na(churnTest$Recency_of_Activity),]$Recency_of_Activity<-0
churnTest[is.na(churnTest$LAS_TAG_LIVE),]$LAS_TAG_LIVE<-"N"

churnTest$Query_Resolved_PrevQ1<-as.numeric(churnTest$Query_Resolved_PrevQ1)
churnTest$Req_Resolved_PrevQ1<-as.numeric(churnTest$Req_Resolved_PrevQ1)

churnTest$predict.score <- predict(Cprune, churnTest)

churnTest$predict.score

