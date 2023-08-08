
#' The likert.plot.fun function
#'
#' @param data.file  The data file name.
#'
#' @param index The name of the predictor index.
#'
#' @param duration The treatment duration.
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

Likert.plot.fun=function(data.file, index, duration, data.path, plot.path)
{data=read.table(paste0(data.path, data.file), header = TRUE, sep="\t")
table(data$cycle)
data$dtime=data$dtime+12+7*(data$rand_tx_duration==duration)
which(data$Time>data$dtime)
dicd=data[which(data$Time>data$dtime),]

data=data[order(data$part_id, data$cycle),]
data00=data
data=data[data$dtime>data$Time,]
data$TRT=NA
data$TRT[data$intervention==1]="RT"
data$TRT[data$intervention==0]="UC"

udata=data[!duplicated(data$part_id),]

medianT=median(udata[, index], na.rm=TRUE)

data$TRT[data[, index]>=medianT]=paste0("High baseline ", index)
data$TRT[data[, index]<medianT]=paste0("Low baseline ", index)

data$t.start=data$Time
data$t.stop=data$time1
data$status=data$status+0
data$event=0+(data$outcome!=0)
data=data[!is.na(data$event),]

uid=unique(data$part_id)
data=data[!duplicated(data),]

winn=14+7*duration%in%c("3 month", "3 months")
for (i in 1:length(uid))
{

  data.i=data[data$part_id==uid[i],]
  diff.T=diff(data.i$Time)
  if (any(diff.T<winn))
  {cc=winn-diff.T[which(diff.T<winn)]
  data.i$Time[(1+which(diff.T<winn)):nrow(data.i)]=cc+data.i$Time[(1+which(diff.T<winn)):nrow(data.i)]

  diff.T=diff(data.i$Time)
  if (any(diff.T<winn))
  {cc=winn-diff.T[which(diff.T<winn)]
  data.i$Time[(1+which(diff.T<winn)):nrow(data.i)]=cc+data.i$Time[(1+which(diff.T<winn)):nrow(data.i)]

  diff.T=diff(data.i$Time)
  if (any(diff.T<winn))
  {cc=winn-diff.T[which(diff.T<winn)]
  data.i$Time[(1+which(diff.T<winn)):nrow(data.i)]=cc+data.i$Time[(1+which(diff.T<winn)):nrow(data.i)]

  diff.T=diff(data.i$Time)
  if (any(diff.T<winn))
  {cc=winn-diff.T[which(diff.T<winn)]
  data.i$Time[(1+which(diff.T<winn)):nrow(data.i)]=cc+data.i$Time[(1+which(diff.T<winn)):nrow(data.i)]

  diff.T=diff(data.i$Time)
  if (any(diff.T<winn))
  {cc=winn-diff.T[which(diff.T<winn)]
  data.i$Time[(1+which(diff.T<winn)):nrow(data.i)]=cc+data.i$Time[(1+which(diff.T<winn)):nrow(data.i)]

  diff.T=diff(data.i$Time)
  if (any(diff.T<winn))
  {cc=winn-diff.T[which(diff.T<winn)]
  data.i$Time[(1+which(diff.T<winn)):nrow(data.i)]=cc+data.i$Time[(1+which(diff.T<winn)):nrow(data.i)]
  }
  }
  }

  }
  }

  }
}

sys=names(data)[12:22]
data$allsym=round(9*(rowSums( (data[,which(names(data)%in%sys)]))/rowSums(!is.na (data[,which(names(data)%in%sys)]))))


sys=c(sys, "allsym")

for (ss in 1:(length(sys)-1))
{data[,sys[ss]]=factor(data[,sys[ss]], levels=0:3)
}

data6=data[data$rand_tx_duration==duration,]

uid6=unique(data6$part_id)

i=j=t=1
win=14
if (duration%in%c("3 months", "3 month")) {win=21}
start=seq(0, max(data6$Time), win)
stop=seq(0, max(data6$Time), win)+win-1
wins=cbind(start, stop)

udata6=data6[!duplicated(data6$part_id),]
udata6=data.frame(part_id=udata6$part_id, TRT=udata6$TRT, score=NA)

list6=vector("list", length(sys))
names(list6)=sys
for (j in 1:length(sys))
{
  for (t in 1:nrow(wins))
  {data.t=data6[data6$Time>=wins[t,1]&data6$Time<=wins[t,2],]

  data.tj=data.t[,c("part_id", "TRT", sys[j])]
  data.tj=data.tj[!duplicated(data.tj),]
  uidtj=unique(data.tj$part_id)
  for (d in 1:length(uidtj))
  {
    doutd=data.tj[data.tj$part_id==uidtj[d],]
    outd=data.frame(part_id=doutd$part_id[1], rscore=mean(as.numeric(as.character(doutd[,sys[j]]))))

    if (d==1) {udata.tj=outd} else
    {udata.tj=rbind(udata.tj, outd)}

  }

  mtj=merge(udata6, udata.tj, by="part_id", all.x=TRUE)
  mtj$score[!is.na(mtj[,"rscore"])]=mtj[,"rscore"][!is.na(mtj[,"rscore"])]
 # outtj=mtj[,1:3]; names(outtj)[3]=paste0("W",t, "-", t+1+duration%in%c("3 month", "3 months"), " (n=", sum(!is.na(outtj[,3])[substr(outtj[,2],1,1)=="H"]),";", sum(!is.na(outtj[,3])[substr(outtj[,2],1,1)=="L"]),")")
  outtj=mtj[,1:3]; names(outtj)[3]=paste0("Cycle ",t, " (n=", sum(!is.na(outtj[,3])[substr(outtj[,2],1,1)=="H"]),";", sum(!is.na(outtj[,3])[substr(outtj[,2],1,1)=="L"]),")")
  if (t==1) {outt=outtj} else
#  {if (all(outt[,1]==outtj[,1])) {otj=as.data.frame(outtj[,3]); names(otj)=paste0("W",t*(2+duration%in%c("3 month", "3 months"))-1-duration%in%c("3 month", "3 months"), "-", t*(2+duration%in%c("3 month", "3 months")), " (n=", sum(!is.na(outtj[,3])[substr(outtj[,2],1,1)=="H"]),";", sum(!is.na(outtj[,3])[substr(outtj[,2],1,1)=="L"]),")")
  {if (all(outt[,1]==outtj[,1])) {otj=as.data.frame(outtj[,3]); names(otj)=paste0("Cycle ",t , " (n=", sum(!is.na(outtj[,3])[substr(outtj[,2],1,1)=="H"]),";", sum(!is.na(outtj[,3])[substr(outtj[,2],1,1)=="L"]),")")
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

for (j in 1:9)
{library(irutils)
  library(reshape)
  ddj=list6[[j]]



  if (duration%in%c("3 months", "3 month")) {ddd=(ddj[,3:6])} else { ddd=(ddj[,3:14])}

  for (c in 1:dim(ddd)[[2]])
  {# ddd[,c]=round(ddd[,c])
    ddd[,c][ddd[,c]==0]="no"
    ddd[,c][ddd[,c]==1]="mild"
    ddd[,c][ddd[,c]==2]="moderate"
    ddd[,c][ddd[,c]==3]="severe"

    ddd[,c]=factor(ddd[,c], levels = c("no",          "mild",         "moderate","severe"))
  }

  rddd=ddd
  for (i in 1:dim(ddd)[[2]])
  {rddd[,i]=ddd[,dim(ddd)[[2]]+1-i]
  }
  names(rddd)=rev(names(ddd))

  like=likert(rddd, group=ddj[,2])
  #detach(package:irutils)

  #library(HH)
  #library(irutils)
  if (duration%in%c("3 months", "3 month"))
  {tiff(paste0(plot.path, index, ".", sys[j], ".", duration, ".20230518.plot.tif"),
       pointsize = 1, width=5800, height=3300, res=600, compression = "zip")
  plt=plot.likert(like, ylab="Time", main=paste0("Patient reported ", sys[j]), col=c("blue", "lightblue", "red", "darkred"))

  print(plt)

  dev.off()
  } else

  {tiff(paste0(plot.path, index, ".", sys[j], ".", duration, ".20230518.plot.tif"),
        pointsize = 1, width=5800, height=5300, res=600, compression = "zip")
    plt=plot.likert(like, ylab="Time", main=paste0("Patient reported ", sys[j]), col=c("blue", "lightblue", "red", "darkred"))

    print(plt)

    dev.off()
  }

}




j=10

library(irutils)
library(reshape)
ddj=list6[[j]]


if (duration%in%c("3 months", "3 month")) {ddd=(ddj[,3:6])} else {ddd=(ddj[,3:14])}

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
tiff(paste0(plot.path, index, ".All symptoms", ".", duration, ".20230518.plot.tif"),
     pointsize = 1, width=5800, height=5300, res=600, compression = "zip")

plta=plot.likert(like, ylab="Time", main=paste0("Patient reported symptoms (sum of severity)"), col=c("blue", "lightblue", "red", "darkred"))
print(plta)
dev.off()

}
