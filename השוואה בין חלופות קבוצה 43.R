alfaTot <- 0.1
alfaPerCompare <- alfaTot/9
################################# השוואה בין חלופות לכל מדד #########################

###### מדד אורך תור ממוצע - רוצים למזער######
#### השוואה בין חלופה 1 למצב קיים ######
WelchTest1<- t.test(x= QLength,y=QLengthOp1, alternative="two.sided",paired=FALSE,var.equal=FALSE,conf.level=1-alfaPerCompare)
print(WelchTest1)
######## קיבלנו רווח סמך חיובי לכן נאמר שחלופה 1 עדיפה על המצב הקיים באורך תור ממוצע למזכירה ######

#### השוואה בין חלופה 2 למצב קיים #######
WelchTest2<- t.test(x= QLength,y=QLengthOp2, alternative="two.sided",paired=FALSE,var.equal=FALSE,conf.level=1-alfaPerCompare)
print(WelchTest2)
##### קיבלנו ש-0 נכלל ברווח סמך לכן נהיה אדישים למצב זה ולא ניתן להגיד איזה מודל יותר טוב ######

#######השוואה בין חלופה 1 לחלופה #####2 ######################
WelchTest3<- t.test(x= QLengthOp1,y=QLengthOp2, alternative="two.sided",paired=FALSE,var.equal=FALSE,conf.level=1-alfaPerCompare)
print(WelchTest3)
######################### קיבלנו רווח סמך שלילי לכן נאמר שחלופה 1 עדיפה על חלופה 2 באורך תור ממוצע למזכירה ######

######## מדד אחוז המסיימים את היום - רוצים למקסם###################
#### השוואה בין חלופה 1 למצב קיים ######
WelchTest4<- t.test(x= propOfFinished,y=propOfFinishedOp1, alternative="two.sided",paired=FALSE,var.equal=FALSE,conf.level=1-alfaPerCompare)
print(WelchTest4)
################ קיבלנו רווח סמך שלילי לכן נאמר שחלופה 1 עדיפה על המצב הקיים במדד אחוז המסיימים את היום ######

######השוואה בין חלופה 2 למצב קיים##################
WelchTest5<- t.test(x= propOfFinished,y=propOfFinishedOp2, alternative="two.sided",paired=FALSE,var.equal=FALSE,conf.level=1-alfaPerCompare)
print(WelchTest5)
########## קיבלנו רווח סמך שלילי לכן נאמר שחלופה 2 עדיפה על המצב הקיים במדד אחוז המסיימים את היום ####

################# השוואה בין חלופה 1 לחלופה 2 ###############
WelchTest6<- t.test(x= propOfFinishedOp1,y=propOfFinishedOp2, alternative="two.sided",paired=FALSE,var.equal=FALSE,conf.level=1-alfaPerCompare)
print(WelchTest6)
################ קיבלנו רווח סמך חיובי לכן נאמר שחלופה 1 עדיפה על חלופה 2 במדד אחוז המסיימים את היום ##############

################### מדד זמן שהייה ממוצע במערכת - רוצים למזער ############
#### השוואה בין חלופה 1 למצב קיים ######
WelchTest7<- t.test(x= meanFlowByReplication,y=meanFlowByReplicationOp1, alternative="two.sided",paired=FALSE,var.equal=FALSE,conf.level=1-alfaPerCompare)
print(WelchTest7)
####### קיבלנו רווח סמך חיובי לכן נאמר שחלופה 1 עדיפה על המצב הקיים במדד זמן שהייה ממוצע במערכת #######

######השוואה בין חלופה 2 למצב קיים##################
WelchTest8<- t.test(x= meanFlowByReplication,y=meanFlowByReplicationOp2, alternative="two.sided",paired=FALSE,var.equal=FALSE,conf.level=1-alfaPerCompare)
print(WelchTest8)
####### קיבלנו רווח סמך חיובי לכן נאמר שחלופה 2 עדיפה על המצב הקיים במדד זמן שהייה ממוצע במערכת #######

################# השוואה בין חלופה 1 לחלופה 2 ###############
WelchTest9<- t.test(x= meanFlowByReplicationOp1,y=meanFlowByReplicationOp2, alternative="two.sided",paired=FALSE,var.equal=FALSE,conf.level=1-alfaPerCompare)
print(WelchTest9)
################ קיבלנו ש-0 נמצא ברווח סמך לכן נהיה אדישים להשוואה זו ולא ניתן להגיד איזה מודל יותר טוב לפי מדד זה #####