alfaTot <- 0.1
alfaPerCompare <- alfaTot/9
################################# ������ ��� ������ ��� ��� #########################

###### ��� ���� ��� ����� - ����� �����######
#### ������ ��� ����� 1 ���� ���� ######
WelchTest1<- t.test(x= QLength,y=QLengthOp1, alternative="two.sided",paired=FALSE,var.equal=FALSE,conf.level=1-alfaPerCompare)
print(WelchTest1)
######## ������ ���� ��� ����� ��� ���� ������ 1 ����� �� ���� ����� ����� ��� ����� ������� ######

#### ������ ��� ����� 2 ���� ���� #######
WelchTest2<- t.test(x= QLength,y=QLengthOp2, alternative="two.sided",paired=FALSE,var.equal=FALSE,conf.level=1-alfaPerCompare)
print(WelchTest2)
##### ������ �-0 ���� ����� ��� ��� ���� ������ ���� �� ��� ���� ����� ���� ���� ���� ��� ######

#######������ ��� ����� 1 ������ #####2 ######################
WelchTest3<- t.test(x= QLengthOp1,y=QLengthOp2, alternative="two.sided",paired=FALSE,var.equal=FALSE,conf.level=1-alfaPerCompare)
print(WelchTest3)
######################### ������ ���� ��� ����� ��� ���� ������ 1 ����� �� ����� 2 ����� ��� ����� ������� ######

######## ��� ���� �������� �� ���� - ����� �����###################
#### ������ ��� ����� 1 ���� ���� ######
WelchTest4<- t.test(x= propOfFinished,y=propOfFinishedOp1, alternative="two.sided",paired=FALSE,var.equal=FALSE,conf.level=1-alfaPerCompare)
print(WelchTest4)
################ ������ ���� ��� ����� ��� ���� ������ 1 ����� �� ���� ����� ���� ���� �������� �� ���� ######

######������ ��� ����� 2 ���� ����##################
WelchTest5<- t.test(x= propOfFinished,y=propOfFinishedOp2, alternative="two.sided",paired=FALSE,var.equal=FALSE,conf.level=1-alfaPerCompare)
print(WelchTest5)
########## ������ ���� ��� ����� ��� ���� ������ 2 ����� �� ���� ����� ���� ���� �������� �� ���� ####

################# ������ ��� ����� 1 ������ 2 ###############
WelchTest6<- t.test(x= propOfFinishedOp1,y=propOfFinishedOp2, alternative="two.sided",paired=FALSE,var.equal=FALSE,conf.level=1-alfaPerCompare)
print(WelchTest6)
################ ������ ���� ��� ����� ��� ���� ������ 1 ����� �� ����� 2 ���� ���� �������� �� ���� ##############

################### ��� ��� ����� ����� ������ - ����� ����� ############
#### ������ ��� ����� 1 ���� ���� ######
WelchTest7<- t.test(x= meanFlowByReplication,y=meanFlowByReplicationOp1, alternative="two.sided",paired=FALSE,var.equal=FALSE,conf.level=1-alfaPerCompare)
print(WelchTest7)
####### ������ ���� ��� ����� ��� ���� ������ 1 ����� �� ���� ����� ���� ��� ����� ����� ������ #######

######������ ��� ����� 2 ���� ����##################
WelchTest8<- t.test(x= meanFlowByReplication,y=meanFlowByReplicationOp2, alternative="two.sided",paired=FALSE,var.equal=FALSE,conf.level=1-alfaPerCompare)
print(WelchTest8)
####### ������ ���� ��� ����� ��� ���� ������ 2 ����� �� ���� ����� ���� ��� ����� ����� ������ #######

################# ������ ��� ����� 1 ������ 2 ###############
WelchTest9<- t.test(x= meanFlowByReplicationOp1,y=meanFlowByReplicationOp2, alternative="two.sided",paired=FALSE,var.equal=FALSE,conf.level=1-alfaPerCompare)
print(WelchTest9)
################ ������ �-0 ���� ����� ��� ��� ���� ������ ������� �� ��� ���� ����� ���� ���� ���� ��� ��� ��� �� #####