##----------------------------------------- 1.  all functions ------------------------------------------------

## add service 
addService<- function  (path,sname,timeDist){
  updatedPath <- seize(path, sname)%>%
    timeout(timeDist) %>%
    release(sname)
  
  return(updatedPath)
}

## Dropping the negatives sampels
trimmedNorm<-function(mu,sd){
  while(TRUE){
    sample<-rnorm(1,mu,sd)
    if (sample>0)
      return (sample)
  }
}
##find the technition with the shortest Q
get_shortest_Q <- function(){
  q1=get_queue_count(meravclinic,"tech1")
  q2=get_queue_count(meravclinic,"tech2")
  q3=get_queue_count(meravclinic,"tech3")
  q4=get_queue_count(meravclinic,"tech4")
  short=min(c(q1,q2,q3,q4))
  if(q1==short)
  {team=1}
  if(q2==short)
  {team=2}
  if(q3==short)
  {team=3}
  if(q4==short)
  {team=4}
  return (team)
}

##sample the form time for evrey patient
formTime<-function(){
  if (substring(get_name(meravclinic),1,1)=='h'){
    patientType=1
    formTime= runif(1,5,7)
  }
  if (substring(get_name(meravclinic),1,1)=='m'){
    patientType=2
    formTime=runif(1,10,12)
  }
  if (substring(get_name(meravclinic),1,1)=='s'){
    patientType=3
    formTime=0
  }
  if (substring(get_name(meravclinic),1,1)=='B'){
    patientType=4
    formTime= runif(1,10,12)
  }
  return (c(patientType,formTime))
}

##sampelling the check type and the medical team for each patient
attFunc<-function(){
  if (substring(get_name(meravclinic),1,1)=='h'){
    checkType=(rdiscrete (1, c(0.163,0.412,0.425),c(1,2,3)))
    medicalTeam=get_shortest_Q()
  }
  if (substring(get_name(meravclinic),1,1)=='m'){
    checkType=(rdiscrete (1, c(0.917,0,0.0833),c(1,2,3)))
    medicalTeam=(rdiscrete (1, c(0.25,0.25,0.25,0.25),c(1,2,3,4)))
  }
  if (substring(get_name(meravclinic),1,1)=='s'){
    checkType=(rdiscrete (1, c(0.1404,0.071,0.7886),c(1,2,3)))
    medicalTeam=(rdiscrete (1, c(0.25,0.25,0.25,0.25),c(1,2,3,4)))
  }
  if (substring(get_name(meravclinic),1,1)=='B'){
    checkType=(rdiscrete (1, c(0.3962,0.577,0.0268),c(1,2,3)))
    medicalTeam=(rdiscrete (1, c(0.25,0.25,0.25,0.25),c(1,2,3,4)))
  }
  return (c(checkType,medicalTeam))
}

#avg queue per resource
avgQueue <- function(time, queueLength, simTime){
  Lavg = 0;
  L = queueLength[1];
  Tnow = time[1];
  Llast = time[1];
  TL = 0;
  Tmax = simTime;
  if (length(time) == length(queueLength)){
    for (i in 2:length(time)){
      if(queueLength[i] != queueLength[i-1]){
        Tnow = time[i];
        TL = TL+L*(Tnow-Llast);
        L = queueLength[i];
        Llast = Tnow;
      }#if
    }#for
  }#end if
  TL=TL+L*(Tmax-Llast);
  Lavg = TL/Tmax;
  return (Lavg);
}#end func

##----------------------------------------- 2.  all simulation parameters ------------------------------------------------
simulationTime<-16*60
n <- 18

##----------------------------------------- 3.  Init Simulation and add all resources  ------------------------------------------------

meravclinic<-simmer("meravclinic") %>%
  add_resource(name="secretary", capacity=2,queue_size=Inf)%>% 
  add_resource(name="tech1", capacity=1,queue_size=Inf)%>%
  add_resource(name="tech2", capacity=1,queue_size=Inf)%>%
  add_resource(name="tech3", capacity=1,queue_size=Inf)%>%
  add_resource(name="tech4", capacity=1,queue_size=Inf)%>%
  add_resource(name="doc1", capacity=1,queue_size=Inf)%>%
  add_resource(name="doc2", capacity=1,queue_size=Inf)%>%
  add_resource(name="doc3", capacity=1,queue_size=Inf)%>%
  add_resource(name="doc4", capacity=1,queue_size=Inf)%>%
  add_resource(name="memoroom", capacity=3,queue_size=Inf)%>%
  add_resource(name="ultraroom", capacity=4,queue_size=Inf)

##----------------------------------------- 4.  All trajectories, start from main trajectory and add sub-trajectories ABOVE IT it . ------------------------------------------------
##preparing the clinic for a new day
setclinic<-
  trajectory("setclinic")%>%
  seize("tech1")%>%
  seize("tech2")%>%
  seize("tech3")%>%
  seize("tech4")%>%
  seize("doc1")%>%
  seize("doc2")%>%
  seize("doc3")%>%
  seize("doc4")%>%
  timeout(function() trimmedNorm(17.064216, 1.866223))%>%
  release("tech1")%>%
  release("tech2")%>%
  release("tech3")%>%
  release("tech4")%>%
  release("doc1")%>%
  release("doc2")%>%
  release("doc3")%>%
  release("doc4")

##sending each doctor for a break
docbreak1 <-
  trajectory("doctor break")%>%
  #("i'm in doc1 break")%>%
  addService("doc1", function() runif(1,4,6))
docbreak2 <-
  trajectory("doctor break")%>%
  #("i'm in doc2 break")%>%
  addService("doc2", function() runif(1,4,6))
docbreak3 <-
  trajectory("doctor break")%>%
  #("i'm in doc3 break")%>%
  addService("doc3", function() runif(1,4,6))
docbreak4 <-
  trajectory("doctor break")%>%
  #("i'm in doc4 break")%>%
  addService("doc4", function() runif(1,4,6))

##sending each technition for a break
techbreak1 <-
  trajectory("technicion1 break")%>%
  #("i'm in tech1 break")%>%
  addService("tech1", function() runif(1,5,7))
techbreak2 <-
  trajectory("technicion1 break")%>%
  #("i'm in tech2 break")%>%
  addService("tech2", function() runif(1,5,7))
techbreak3 <-
  trajectory("technicion3 break")%>%
  #("i'm in tech3 break")%>%
  addService("tech3", function() runif(1,5,7))
techbreak4 <-
  trajectory("technicion4 break")%>%
  #("i'm in tech4 break")%>%
  addService("tech4", function() runif(1,5,7))




secondmemo<-
  trajectory("memo check")%>%
  #("i'm in memo check again")%>%
  simmer::select(resources = function() paste0("tech",get_attribute(meravclinic,"medical team")), id=5)%>%
  seize_selected(amount = 1,id=5)%>%
  seize("memoroom")%>%
  #("i'm in memoroom")%>%
  timeout(function() rgamma(1,5.7,0.87))%>%
  #("i'm after memocheck")%>%
  release_selected(amount = 1,id=5)%>%
  simmer::select(resources = function() paste0("doc",get_attribute(meravclinic,"medical team")), id=6)%>%
  seize_selected(amount = 1,id=6)%>%
  timeout(function() runif(1,3,5))%>%
  #("i got my results...leaving")%>%
  release_selected(amount = 1,id=6)%>%
  release("memoroom")%>%
  set_prioritization(c(1,1,FALSE),mod="+")%>%
  simmer::select(resources = function() paste0("tech",get_attribute(meravclinic,"medical team")), id=5)%>%
  seize_selected(amount = 1,id=5)%>%
  timeout(function() runif(1,1,2))%>%
  release_selected(amount = 1,id=5)%>%
  set_prioritization(c(-1,-1,FALSE),mod="+")%>%
  set_attribute(keys ="finished", value =1)





memo<-
  trajectory("memo check")%>%
  #("i'm in memo check")%>%
  simmer::select(resources = function() paste0("tech",get_attribute(meravclinic,"medical team")), id=1)%>%
  seize_selected(amount = 1,id=1)%>%
  seize("memoroom")%>%
  #("i'm in memoroom")%>%
  timeout(function() rgamma(1,5.7,0.87))%>%
  #("i'm after memocheck")%>%
  release_selected(amount = 1,id=1)%>%
  simmer::select(resources = function() paste0("doc",get_attribute(meravclinic,"medical team")), id=2)%>%
  seize_selected(amount = 1,id=2)%>%
  timeout(function() runif(1,3,5))%>%
  #("i got my results...leaving")%>%
  release_selected(amount = 1,id=2)%>%
  release("memoroom")%>%
  set_prioritization(c(1,1,FALSE),mod="+")%>%
  simmer::select(resources = function() paste0("tech",get_attribute(meravclinic,"medical team")), id=1)%>%
  seize_selected(amount = 1,id=1)%>%
  timeout(function() runif(1,1,2))%>%
  release_selected(amount = 1,id=1)%>%
  set_prioritization(c(-1,-1,FALSE),mod="+")%>%#
  branch(option=function() rdiscrete(1,c(0.7,0.3),c(0,1)),continue= c(FALSE),secondmemo)%>%
  set_attribute(keys ="finished", value =1)





ultra<-
  trajectory("ultra check")%>%
  #("i'm in ultra check")%>%
  simmer::select(resources = function() paste0("tech",get_attribute(meravclinic,"medical team")), id=3)%>%
  seize_selected(amount = 1,id=3)%>%
  seize("ultraroom")%>%
  #("i'm in ultraroom")%>%
  timeout(function() rgamma(1,3.01,1.23))%>%
  release_selected(amount = 1,id=3)%>%
  #("im ready for ultra")%>%
  simmer::select(resources = function() paste0("doc",get_attribute(meravclinic,"medical team")), id=4)%>%
  seize_selected(amount = 1,id=4)%>%
  timeout(function() rgamma(1,6.91,0.54))%>%
  timeout(function() rgamma(1,3.77,0.31))%>%
  #("i got my results...leaving")%>%
  release_selected(amount = 1,id=4)%>%
  release("ultraroom")%>%
  set_prioritization(c(1,1,FALSE),mod="+")%>%
  simmer::select(resources = function() paste0("tech",get_attribute(meravclinic,"medical team")), id=3)%>%
  seize_selected(amount = 1,id=3)%>%
  timeout(function() runif(1,1,2))%>%
  release_selected(amount = 1,id=3)%>%
  set_prioritization(c(-1,-1,FALSE),mod="+")%>%
  set_attribute(keys ="finished", value =1)





memoUltra<-
  trajectory("memo & ultra checks")%>%
  #("i'm in memo & ultra check")%>%
  simmer::select(resources = function() paste0("tech",get_attribute(meravclinic,"medical team")), id=7)%>%##choosing the right tech from the attribute
  seize_selected(amount = 1,id=7)%>%##cacth tech
  seize("memoroom")%>%#entering into the mamo room
  #("i'm in memoroom")%>%
  timeout(function() rgamma(1,5.7,0.87))%>%#the technition doing the mamo room 
  #("i'm after memocheck")%>%
  release_selected(amount = 1,id=7)%>%##release tech
  release("memoroom")%>%##the patient leave the mamo room
  #("i'm in ultra check")%>%
  simmer::select(resources = function() paste0("tech",get_attribute(meravclinic,"medical team")), id=8)%>%#choosing the right tech from the attribute
  seize_selected(amount = 1,id=8)%>%#cacth tech
  seize("ultraroom")%>%#entering into ultra room
  #("i'm in ultraroom")%>%
  timeout(function() rgamma(1,3.01,1.23))%>%##the tech prepering ultra check
  release_selected(amount = 1,id=8)%>%##release tech
  #("im ready for ultra")%>%
  simmer::select(resources = function() paste0("doc",get_attribute(meravclinic,"medical team")), id=9)%>%##choosing the right doc from the attribut
  seize_selected(amount = 1,id=9)%>%##catch doc
  timeout(function() rgamma(1,6.91,0.54))%>%##ultra check
  timeout(function() runif(1,3,5))%>%###decoding memo 
  timeout(function() rgamma(1,3.77,0.31))%>%##decoding ultra 
  release_selected(amount = 1,id=9)%>%##release doc
  release("ultraroom")%>%##leave ultra room
  set_prioritization(c(1,1,FALSE),mod="+")%>%#update the priority for getting results from tecnition
  simmer::select(resources = function() paste0("tech",get_attribute(meravclinic,"medical team")), id=10)%>%##choosing the right tech from the attribut
  seize_selected(amount = 1,id=10)%>%##cacth tech
  timeout(function() runif(1,1,2))%>%##giving results to the patient
  release_selected(amount = 1,id=10)%>%##release tech
  set_prioritization(c(-1,-1,FALSE),mod="+")%>%##update priority
  branch(option=function() rdiscrete(1,c(0.7,0.3),c(0,1)),continue= c(FALSE),secondmemo)%>%##spliting the patients by distribution to second mamo check
  set_attribute(keys ="finished", value =1)


secretary<-
  trajectory("secretary")%>%
  #("hii")%>%
  set_attribute(keys ="finished", value =0)%>%
  set_attribute(keys = c("Patient Type","form time"), value =function() formTime())%>%
  timeout_from_attribute("form time")%>%
  #("finish form")%>%
  addService("secretary",function() rtriangle(1,3,7,5))%>%
  set_attribute(keys =c("check type","medical team") , value =function() attFunc())%>%
  branch(option=function() get_attribute(meravclinic,"check type") ,continue= c(FALSE,FALSE,FALSE),memo,ultra,memoUltra)











##----------------------------------------- 5.  All Generators, ALWAYS LAST. ------------------------------------------------

meravclinic%>%
  add_generator("setclinic",setclinic,at(0),mon=2, priority=4, preemptible=4, restart=FALSE)%>%
  add_generator("hospitalized", secretary, to(5*60,function () rexp(1,0.097)) , mon=2)%>%
  add_generator("marked", secretary,to(5*60,function () runif(1,0,4)), mon=2, priority=2, preemptible=2, restart=FALSE)%>%
  add_generator("survey", secretary,to(5*60,function () rexp(1,0.0425)), mon=2)%>%
  add_generator("BRCA", secretary, to(5*60,function () rexp(1,0.798)), mon=2)%>%
  add_generator("breakdoc1", docbreak1, function () rexp(1,1/24), mon=2, priority=4, preemptible=4, restart=FALSE)%>%
  add_generator("breakdoc2", docbreak2, function () rexp(1,1/24), mon=2, priority=4, preemptible=4, restart=FALSE)%>%
  add_generator("breakdoc3", docbreak3, function () rexp(1,1/24), mon=2, priority=4, preemptible=4, restart=FALSE)%>%
  add_generator("breakdoc4", docbreak4, function () rexp(1,1/24), mon=2, priority=4, preemptible=4, restart=FALSE)%>%
  add_generator("breaktech1", techbreak1,function () rexp(1,1/20), mon=2, priority=4, preemptible=4, restart=FALSE)%>%
  add_generator("breaktech2", techbreak2,function () rexp(1,1/20), mon=2, priority=4, preemptible=4, restart=FALSE)%>%
  add_generator("breaktech3", techbreak3,function () rexp(1,1/20), mon=2, priority=4, preemptible=4, restart=FALSE)%>%
  add_generator("breaktech4", techbreak4,function () rexp(1,1/20), mon=2, priority=4, preemptible=4, restart=FALSE)


##----------------------------------------- 6.  reset, run, plots, outputs ------------------------------------------------
# set.seed(456)
# reset(meravclinic)%>%run(until=simulationTime)


simoXn <- mclapply(1:n, function(i) {
  set.seed(i+456)
  reset(meravclinic)%>%run(until=simulationTime)%>%
    wrap()
})
arrivalDataOp11 <- get_mon_arrivals(simoXn,ongoing=FALSE)
arrivalDataOp12 <- get_mon_arrivals(simoXn,ongoing=TRUE)
arrivalDataOp13 <- get_mon_arrivals(simoXn,ongoing=TRUE,per_resource=TRUE)
resourceDataOp1 <- get_mon_resources(simoXn)
attributeDataOp1 <- get_mon_attributes(simoXn)

##########################################################מדדים#############################################
#1.avereage Q for secretary

QLengthOp1 <- 0
for(i in 1:n ){
  timeOp1 <- as.matrix(subset(resourceDataOp1, resource == "secretary" & replication==i, select = c(time)))
  queueLengthOp1 <- as.matrix(subset(resourceDataOp1, resource == "secretary" & replication==i, select = c(queue)));
  QLengthOp1[i] <-  avgQueue(timeOp1,queueLengthOp1,simulationTime)
}
QLengthOp1 <- as.table(QLengthOp1)
avgQueueLengthForSecOp1 <- mean(QLengthOp1)
stdQLengthOp1 <- sd(QLengthOp1)
paste("the avg Queue length for the secretary is:",avgQueueLengthForSecOp1)

#2.prop of finished

numOfArrivedOp1 <- sqldf("SELECT count(*)
                      FROM arrivalDataOp12
                      where name!='setclinic0' and (name not like 'break%')
                      group by replication")

numOfFinishedOp1 <- sqldf("SELECT count(*) as propration
                       FROM arrivalDataOp11
                       where name!='setclinic0' and (name not like 'break%')
                       group by replication")

propOfFinishedOp1 <- numOfFinishedOp1/numOfArrivedOp1
meanPropOfFinishedOp1 <- sqldf("SELECT avg(propration)
                            From propOfFinishedOp1")

stdPropOfFinishedOp1 <- sqldf("SELECT stdev(propration)
                           From propOfFinishedOp1")


paste("the proporation of customers that finished the day is",meanPropOfFinishedOp1*100,"%")

#3.avereage mean flow
meanFlowByReplicationOp1<-sqldf("select avg(end_time-start_time) as FlowTime
                             from arrivalDataOp11
                             where name!='setclinic0' and (name not like 'break%')
                             group by replication")

meanFlowOp1 <-  sqldf("SELECT avg(FlowTime)
                   From meanFlowByReplicationOp1")
stdMeanFlowOp1 <- sqldf("SELECT stdev(FlowTime)
                     From meanFlowByReplicationOp1")

paste("the mean flow time in system is:",meanFlowOp1, "minutes")



