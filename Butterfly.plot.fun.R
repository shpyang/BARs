
#' The likert.plot.fun function
#'
#' @param data.file  The data file name.
#'
#' @param index1 The name of the predictor index.
#'
#' @param dur The treatment duration.
#'
#' @param data.path The folder name where the data is saved.
#'
#' @param plot.path  The folder name for the plots to be saved.
#'
#' @examples
#'
#' # This is the function for generating plots for Likert score data.
#'
#' @export


##############################
# The R code for generate plots

library("HH"); library('likert')

Butterfly.plot.fun=function(data.file, index1, dur, data.path, plot.path)
{
  print("Start preparing data for generating a butterfly plot.")
  datax=read.csv(paste0(data.path, data.file) )
datax=as.data.frame(datax[!duplicated(datax),])

sys=c("nausea","vomit","diarrhea","shortbreath","handfoot","numb","pain","muscache","fatigue")

#data=as.data.frame(data)
#table(data$cycle)
#data$dtime=data$dtime+12+7*(data$rand_tx_duration==duration)
#data$dtime=data$dtime+12+7*(data$chemo_dur==dur)
#which(data$Time>data$dtime)
#dicd=data[which(data$Time>data$dtime),]

#data=data[order(data$part_id, data$cycle),]
datax=datax[order(datax$part_id, datax$pcycle),]

#data=data[order(data$part_id, data$pcycle),]
#data00=data
datax00=datax
#data=data[data$dtime>data$Time,]
#data$TRT=NA
#data$TRT[data$intervention==1]="RT"
#data$TRT[data$intervention==0]="UC"
datax$TRT=NA
datax$TRT[datax$intervention==1]="RT"
datax$TRT[datax$intervention==0]="UC"

#udata=data[!duplicated(data$part_id),]
#medianT=median(udata[, index], na.rm=TRUE)
dataxu=as.data.frame(datax[,which(names(datax)%in%sys)])

includx=rowSums(is.na(dataxu))<9
table(includx)

datax=datax[includx,]
datax=datax[!is.na(datax[, index1]),]
dataxu=as.data.frame(datax[,which(names(datax)%in%sys)])

udatax=datax[!duplicated(datax$part_id),]
medianT=median(udatax[, index1], na.rm=TRUE)

#data$TRT[data[, index]>=medianT]=paste0("High baseline ", index)
#data$TRT[data[, index]<medianT]=paste0("Low baseline ", index)
datax$TRT[datax[, index1]>=medianT]=paste0("High baseline ", index1)
datax$TRT[datax[, index1]<medianT]=paste0("Low baseline ", index1)

#if (toupper(index)%in%toupper(c("intervention",	"FOLFOX", "oxaliplatin")))
#{data$TRT=paste0(toupper(index), ifelse(data[,index]==1, " YES"," NO"))
#}
if (toupper(index1)%in%toupper(c("intervention",	"FOLFOX", "oxaliplatin")))
{datax$TRT=paste0(toupper(index1), ifelse(datax[,index1]==1, " YES"," NO"))
}

#data$t.start=data$Time
#data$t.stop=data$time1

#uid=unique(data$part_id)
uid=unique(datax$part_id)

#winn=14+7*duration%in%c("3 month", "3 months")


#datau=as.data.frame(data[,which(names(data)%in%sys)])

#for (i in 1:dim(datau)[[2]])
#{datau[is.na(datau[,i]),i]=0
#datau[,i]=as.numeric(datau[,i])
#}
for (i in 1:dim(dataxu)[[2]])
{dataxu[is.na(dataxu[,i]),i]=0
dataxu[,i]=as.numeric(dataxu[,i])
}

#data$allsym=round(9*(rowSums( (datau))/rowSums(!is.na (data[,which(names(data)%in%sys)]))))
datax$allsym=round(9*(rowSums( (dataxu))/rowSums(!is.na (datax[,which(names(datax)%in%sys)]))))


sys=c(sys, "allsym")#, "intervention", "FOLFOX")

#for (ss in 1:(length(sys)-1))
#{data[,sys[ss]]=factor(data[,sys[ss]], levels=0:3)
#}
for (ss in 1:(length(sys)-1))
{datax[,sys[ss]]=factor(datax[,sys[ss]], levels=0:3)
}

#data6=data[data$rand_tx_duration==duration,]
data6=datax[datax$chemo_dur==dur,]
uid6=unique(data6$part_id)

i=j=t=1
#win=14
#if (duration%in%c("3 months", "3 month")) {win=21}
#start=seq(0, max(data6$Time), win)
#stop=seq(0, max(data6$Time), win)+win-1
#wins=cbind(start, stop)

udata6=data6[!duplicated(data6$part_id),]
upcycle=sort(unique(data6$pcycle))
if (dur=="3m") {upcycle=sort(upcycle[upcycle%in%c(1:4)])}
if (dur=="6m") {upcycle=sort(upcycle[upcycle%in%c(1:12)])}
udata6=data.frame(part_id=udata6$part_id, TRT=udata6$TRT, score=NA)

list6=vector("list", length(sys))
names(list6)=sys


for (j in 1:length(sys))
{
  #  for (t in 1:nrow(wins))
  for (t in 1:length(upcycle))
  {#data.t=data6[data6$Time>=wins[t,1]&data6$Time<=wins[t,2],]
    data.t=data6[data6$pcycle==upcycle[t],]

    data.tj=data.t[,c("part_id", "TRT", sys[j])]
    data.tj=data.tj[!duplicated(data.tj),]
    uidtj=unique(data.tj$part_id)
    for (d in 1:length(uidtj))
    {
      doutd=data.tj[data.tj$part_id==uidtj[d],][1,]
      #print(nrow(doutd))
      #   if (nrow(doutd)>1) {print(doutd)}
      outd=data.frame(part_id=doutd$part_id[1], rscore=mean(as.numeric(as.character(doutd[,sys[j]]))))

      if (d==1) {udata.tj=outd} else
      {udata.tj=rbind(udata.tj, outd)}

    }

    mtj=merge(udata6, udata.tj, by="part_id", all.x=TRUE)
    mtj$score[!is.na(mtj[,"rscore"])]=mtj[,"rscore"][!is.na(mtj[,"rscore"])]
    # outtj=mtj[,1:3]; names(outtj)[3]=paste0("W",t, "-", t+1+duration%in%c("3 month", "3 months"), " (n=", sum(!is.na(outtj[,3])[substr(outtj[,2],1,1)=="H"]),";", sum(!is.na(outtj[,3])[substr(outtj[,2],1,1)=="L"]),")")
    outtj=mtj[,1:3];

    names(outtj)[3]=paste0("Cycle ",t, " (n=", sum(!is.na(outtj[,3])[substr(outtj[,2],1,1)=="H"]),";", sum(!is.na(outtj[,3])[substr(outtj[,2],1,1)=="L"]),")")

    if (toupper(index1)%in%toupper(c("intervention",	"FOLFOX", "oxaliplatin")))
    {names(outtj)[3]=paste0("Cycle ",t, " (n=", sum(!is.na(outtj[,3])[regexpr(" NO", outtj[,2])>0]),";", sum(!is.na(outtj[,3])[regexpr(" YES", outtj[,2])>0]),")")
    }

    if (t==1) {outt=outtj} else
      #  {if (all(outt[,1]==outtj[,1])) {otj=as.data.frame(outtj[,3]); names(otj)=paste0("W",t*(2+duration%in%c("3 month", "3 months"))-1-duration%in%c("3 month", "3 months"), "-", t*(2+duration%in%c("3 month", "3 months")), " (n=", sum(!is.na(outtj[,3])[substr(outtj[,2],1,1)=="H"]),";", sum(!is.na(outtj[,3])[substr(outtj[,2],1,1)=="L"]),")")
    {if (all(outt[,1]==outtj[,1]))
    {otj=as.data.frame(outtj[,3])
    names(otj)=paste0("Cycle ",t , " (n=", sum(!is.na(outtj[,3])[substr(outtj[,2],1,1)=="H"]),";", sum(!is.na(outtj[,3])[substr(outtj[,2],1,1)=="L"]),")")

    if (toupper(index1)%in%toupper(c("intervention",	"FOLFOX", "oxaliplatin")))
    {
      names(otj)=paste0("Cycle ",t , " (n=", sum(!is.na(outtj[,3])[regexpr(" NO", outtj[,2])>0]),";", sum(!is.na(outtj[,3])[regexpr(" YES", outtj[,2])>0]),")")
    }

    outt=cbind(outt, otj)}
    }
  }


  if (j==10)
  { for (c in 3:dim(outt)[[2]])

  {outtt=outt[,c]
  outt[,c]=NA
  outt[,c][outtt<5&(!is.na(outtt))]="Number <5"
  outt[,c][outtt>=5&outtt<10&(!is.na(outtt))]="Number 5-10"
  outt[,c][outtt>=10&outtt<15&(!is.na(outtt))]="Number 10-15"
  outt[,c][outtt>=15&(!is.na(outtt))]="Number >=15"}

  }
  list6[[j]]=outt

  #    write.csv(outt, paste0("H:/WFH/Stephanie2/out/", index, ".Table.", sys[j], ".6.20230518.csv"), row.names=FALSE)

}

sys[10]="allc"


j=1

for (j in c(1:9))
{library(irutils)
  library(reshape)
  ddj=list6[[j]]



  if (dur%in%c("3 months", "3 month", "3m", "3M")) {ddd=(ddj[,3:min(6, dim(ddj)[[2]])])} else { ddd=(ddj[,3:12])}

  for (c in 1:dim(ddd)[[2]])
  {# ddd[,c]=round(ddd[,c])
    ddd[,c][ddd[,c]==0]="no"
    ddd[,c][ddd[,c]==1]="mild"
    ddd[,c][ddd[,c]==2]="moderate"
    ddd[,c][ddd[,c]==3]="severe"

    ddd[,c]=factor(ddd[,c], levels = c("no",          "mild",         "moderate","severe"))
  }

  rddd=ddd

  #if (j==6) {print(ddj);print(rddd)}
  for (i in 1:dim(ddd)[[2]])
  {rddd[,i]=ddd[,dim(ddd)[[2]]+1-i]
  }
  names(rddd)=rev(names(ddd))
  #print("SSSSSSSSSSSSS")

  #print(rddd); print(ddj)
  like=likert(rddd, group=ddj[,2])
  #detach(package:irutils)
  #if (j==6) {print(like)}
  #library(HH)
  #library(irutils)
  if (dur%in%c("3 months", "3 month", "3m", "3M"))
  {tiff(paste0(plot.path, index1, ".", sys[j], ".", dur, ".",Sys.Date(),".plot.tif"),
        pointsize = 1, width=5800, height=3300, res=600, compression = "zip")
    plt=plot.likert(like, ylab="Time", main=paste0("Patient reported ", sys[j]), col=c("blue", "lightblue", "red", "darkred"))

    print(plt)

    dev.off()
  } else

  {tiff(paste0(plot.path, index1, ".", sys[j], ".", dur, ".",Sys.Date(),".plot.tif"),
        pointsize = 1, width=5800, height=5300, res=600, compression = "zip")
    plt=plot.likert(like, ylab="Time", main=paste0("Patient reported ", sys[j]), col=c("blue", "lightblue", "red", "darkred"))

    print(plt)

    dev.off()
  }

}


#print("Completing")


j=10

library(irutils)
library(reshape)
ddj=list6[[j]]


if (dur%in%c("3 months", "3 month", "3m", "3M")) {ddd=(ddj[,3:6])} else {ddd=(ddj[,3:14])}

for (ss in 1:dim(ddd)[[2]])
{ddd[,ss]=factor(ddd[,ss], levels=c("Number <5", "Number 5-10","Number 10-15","Number >=15"))
}

rddd=ddd
for (i in 1:dim(ddd)[[2]])
{rddd[,i]=ddd[,dim(ddd)[[2]]+1-i]
}
names(rddd)=rev(names(ddd))

like=likert(rddd, group=ddj[,2])
#detach(package:irutils)
#
#library(HH)
#library(irutils)
tiff(paste0(plot.path, index1, ".All symptoms", ".", dur, ".",Sys.Date(),".plot.tif"),
     pointsize = 1, width=5800, height=ifelse(dur%in%c("3 months", "3 month", "3m", "3M"), 3300,5300), res=600, compression = "zip")

plta=plot.likert(like, ylab="Time", main=paste0("Patient reported symptoms (sum of severity)"), col=c("blue", "lightblue", "red", "darkred"))
print(plta)
dev.off()



}
