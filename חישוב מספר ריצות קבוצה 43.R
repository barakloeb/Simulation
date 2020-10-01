

################################################ Calc N #####################################

gammaProp <- 0.05/1.05

alfaTot <- 0.1

alfaPerOpt <- alfaTot/3


###################################### בדיקה האם המדדים עומדים בדיוק יחסי - מצב קיים ################
# 1. אורך תור ממוצע למזכירה
test1<- t.test(x= QLength,y=NULL, alternative="two.sided",conf.level=1-alfaPerOpt)
print(test1)
confInterval1<-(test1$conf.int[2]-avgQueueLengthForSec)
duke <- confInterval1/avgQueueLengthForSec
print(duke<gammaProp)
#2. יחס מספר המסיימים
test1<- t.test(x= propOfFinished,y=NULL, alternative="two.sided",conf.level=1-alfaPerOpt)
print(test1)
confInterval1<-(test1$conf.int[2]-meanPropOfFinished)
duke <- confInterval1/meanPropOfFinished
print(duke<gammaProp)
#3. זמן שהייה ממוצע במערכת
test1<- t.test(x= meanFlowByReplication,y=NULL, alternative="two.sided",conf.level=1-alfaPerOpt)
print(test1)
confInterval1<-(test1$conf.int[2]-meanFlow)
duke <- confInterval1/meanFlow
print(duke<gammaProp)


###################################### בדיקה אם המדדים עומדים בדיוק יחסי- חלופה 1 ##############
# 1. אורך תור ממוצע למזכירה
test2<- t.test(x= QLengthOp1,y=NULL, alternative="two.sided",conf.level=1-alfaPerOpt)
print(test2)
confInterval2<-(test2$conf.int[2]-avgQueueLengthForSecOp1)
duke2 <- confInterval2/avgQueueLengthForSecOp1
print(duke2<gammaProp)
#2. יחס מספר המסיימים
test2<- t.test(x= propOfFinishedOp1,y=NULL, alternative="two.sided",conf.level=1-alfaPerOpt)
print(test2)
confInterval2<-(test2$conf.int[2]-meanPropOfFinishedOp1)
duke <- confInterval2/meanPropOfFinishedOp1
print(duke<gammaProp)
#3. זמן שהייה ממוצע במערכת
test2<- t.test(x= meanFlowByReplicationOp1,y=NULL, alternative="two.sided",conf.level=1-alfaPerOpt)
print(test2)
confInterval2<-(test2$conf.int[2]-meanFlowOp1)
duke <- confInterval2/meanFlowOp1
print(duke<gammaProp)


###################################### בדיקה האם המדדים עומדים בדיוק יחסי - חלופה 2 ##########
# 1. אורך תור ממוצע למזכירה
test3<- t.test(x= QLengthOp2,y=NULL, alternative="two.sided",conf.level=1-alfaPerOpt)
print(test3)
confInterval3<-(test3$conf.int[2]-avgQueueLengthForSecOp2)
duke <- confInterval3/avgQueueLengthForSecOp2
print(duke<gammaProp)
#2. יחס מספר המסיימים
test3<- t.test(x= propOfFinishedOp2,y=NULL, alternative="two.sided",conf.level=1-alfaPerOpt)
print(test3)
confInterval3<-(test3$conf.int[2]-meanPropOfFinishedOp2)
duke <- confInterval3/meanPropOfFinishedOp2
print(duke<gammaProp)
#3. זמן שהייה ממוצע במערכת
test3<- t.test(x= meanFlowByReplicationOp2,y=NULL, alternative="two.sided",conf.level=1-alfaPerOpt)
print(test3)
confInterval3<-(test3$conf.int[2]-meanFlowOp2)
duke <- confInterval3/meanFlowOp2
print(duke<gammaProp)


######## קירוב גס לחישוב מספר ריצות ##########################

#חישוב גס למדד אחוז המסיימים במצב קיים
N <-n*(confInterval1/(meanPropOfFinished*gammaProp))^2
######## קיבלנו N שווה 16.77272 #######

################### חישוב גס למדד אחוז המסיימים בחלופה 2 ################
N1 <-n*(confInterval3/(meanPropOfFinishedOp2*gammaProp))^2

######################## קיבלנו N שווה 17.422 #####################

###################### קיבלנו שעל מנת לעמוד בדיוק היחסי נצטרך להריץ במקסימום 18 פעמים לכן נריץ את כל המודלים לפי זה #####

####################### לאחר הרצה של 18 פעמים קיבלנו שכל במדדים בכל המודלים עומדים בדיוק היחסי #####
