library(MASS)
str(survey)

#1.a
mean(survey["Pulse"])
# there is NA

#1.b
mean(survey$Pulse, na.rm = TRUE)

#1.c
survey$wholeage = floor(survey$Age)

#1.d
mean(survey[survey$wholeage<20,]$Pulse, na.rm = TRUE)

#1.e
mean(subset(survey, W.Hnd == "Right", select = "Age")$Age, na.rm = T)

#1.f
prop.table(table(with(survey, Clap != "Left" & W.Hnd == "Left")))
mean(survey$Clap[survey$W.Hnd == "Left"] != "Left", na.rm = T)

#1.g
plot(survey$Age, survey$Pulse)
plot(log(survey$Age-10), survey$Pulse)

#2.a
hill_class = hills
round((hill_class$climb/12)*2.54,-1)

#2.b
rownames(hill_class[hill_class$time>60,])

#2.c
rownames(hill_class)[which(hill_class$dist<5)[1:3]]

#2.d
hill_class=hill_class[!rownames(hill_class) == "Meall Ant-Suidhe",]

#2.e
df = hill_class[,c("time","dist")]
plot( 
  gvisScatterChart(df)
)
plot(hill_class$time,hill_class$dist)
# last one, running too slow

#2.f
hill_class[hill_class$time >200,"time"] = hill_class[hill_class$time >200,"time"]-60

#2.g
hillsMat = as.matrix(hills)
print.default(hills)
print.default(hillsMat)
#2.h
is(hillsMat,hills)
plot(hillsMat)
pairs(hillsMat)
pairs(hills)

#2.i
as.matrix(survey)

#3.a
class(birthwt$race)
# integer, should be in factor

#3.b
birthwt_class = birthwt
birthwt_class$race = factor(birthwt_class$race)

#3.c
table(birthwt_class$race)

#3.d
tab = with(birthwt_class, table(smoke, low))

#4.a
setwd("/Users/xiaofeifei/Downloads")
dat = read.table("sprays.dat.txt", header = TRUE)

#4.b
str(dat)

#4.c
with(dat, tapply(count, spray, mean))

#4.e
with(dat,quantile(count,prob = c(0.25,0.75)))
with(dat, tapply(count, spray, quantile(probs = c(0.25,0.75))))

#4.f
with(dat, tapply(count, spray, function(x) quantile(x,probs = c(0.25,0.75))))

#4.g
with(dat, tapply(count, spray, boxplot))

# just for tesing the github at windows

# title, date, author,name,affiliation and contact information
# a summary(abstract) what is the topic and why it worth reading
# an introduction: gernal context and giving what backgroud information to understand the report
# main body: results methods conclusions and discussion
# refernces
# appendix