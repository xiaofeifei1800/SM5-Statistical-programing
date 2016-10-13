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
prop.table(table(with(survey, Clap == "Neither" & W.Hnd == "Left")))

#1.g
plot(survey$Age, survey$Pulse)
plot(log(survey$Age-10), survey$Pulse)

#2.a
hill_class = hills
round((hill_class$climb/12)*2.54)

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
