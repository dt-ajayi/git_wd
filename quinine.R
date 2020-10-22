
library(janitor)

# Displacement value (DV) 

dvdf <- read.csv("dvdf.csv")

head(dvdf)
dvdf$Time <- as.factor(dvdf$Time)
dvdf$Temp <- as.factor(dvdf$Temp)

str(dvdf)


tabyl(dvdf, Time, Temp)

aggregate(DV ~ Time + Temp, data = dvdf, mean)
aggregate(DV ~ Time + Temp, data = dvdf, sd)

mdv <- aov(DV ~ Time*Temp, data = dvdf)
summary(mdv)


with(dvdf, 
     interaction.plot(Time, Temp, DV, type="b",
                      pch=c(16, 18),
                      xlab = "Time (week)",
                      ylab = "Mean DV",
                      trace.label = paste0("Temp (","\u00b0C)" )
     ))


# Weight of suppositories

wtdf <- read.csv("wtdf.csv")

head(wtdf)
cat(names(wtdf), fill = TRUE, sep = ", ")

# Weight, Time, Temp

wtdf$Time <- as.factor(wtdf$Time)
wtdf$Temp <- as.factor(wtdf$Temp)

str(wtdf)

tabyl(wtdf, Time,Temp)

aggregate(Weight ~ Time + Temp, data = wtdf, mean)
aggregate(Weight ~ Time + Temp, data = wtdf, sd)

mwt <- aov(Weight ~ Time*Temp, data = wtdf)
summary(mwt)

with(wtdf, 
     interaction.plot(Time, Temp, Weight, type="b",
                      pch=c(16, 18),
                      xlab = "Time (week)",
                      ylab = "Mean Weight (g)",
                      trace.label = paste0("Temp (","\u00b0C)" )
     ))


# Liquefaction time (LT)

ltdf <- read.csv("ltdf.csv")
head(ltdf)

cat(names(ltdf), fill = TRUE, sep = ", ")

# LT, Time, Temp

ltdf$Time <- as.factor(ltdf$Time)
ltdf$Temp <- as.factor(ltdf$Temp)

str(ltdf)

tabyl(ltdf, Time, Temp)

aggregate(LT ~ Time + Temp, data = ltdf, mean)
aggregate(LT ~ Time + Temp, data = ltdf, sd)

mlt <- aov(LT ~ Time*Temp, data = ltdf)
summary(mlt)

with(ltdf, 
     interaction.plot(Time, Temp, LT, type="b",
                      pch=c(16, 18),
                      xlab = "Time (week)",
                      ylab = "Mean Liquefaction Time (min)",
                      trace.label = paste0("Temp (","\u00b0C)" )
     ))



# Melting range (MR)

mrdf <- read.csv("mrdf.csv")
head(mrdf)

cat(names(mrdf), fill = TRUE, sep = ", ")

# MR, Time, Temp

mrdf$Time <- as.factor(mrdf$Time)
mrdf$Temp <- as.factor(mrdf$Temp)

str(mrdf)

tabyl(mrdf, Time, Temp)

aggregate(MR ~ Time + Temp, data = mrdf, mean)
aggregate(MR ~ Time + Temp, data = mrdf, sd)

mmr <- aov(MR ~ Time*Temp, data = mrdf)
summary(mmr)

with(mrdf, 
     interaction.plot(Time, Temp, MR, type="b",
                      pch=c(16, 18),
                      xlab = "Time (week)",
                      ylab = "Mean Melting Range (min)",
                      trace.label = paste0("Temp (","\u00b0C)" )
     ))


# Suppository hardness

hddf <- read.csv("hddf.csv")
head(hddf)

cat(names(hddf), fill = TRUE, sep = ", ")

# Hardness, Time, Temp

hddf$Time <- as.factor(hddf$Time)
hddf$Temp <- as.factor(hddf$Temp)

str(hddf)

tabyl(hddf, Time, Temp)

aggregate(Hardness ~ Time + Temp, data = hddf, mean)
aggregate(Hardness ~ Time + Temp, data = hddf, sd)

mhd <- aov(Hardness ~ Time*Temp, data = hddf)
summary(mhd)

with(hddf, 
     interaction.plot(Time, Temp, Hardness, type="b",
                      pch=c(16, 18),
                      xlab = "Time (week)",
                      ylab = "Mean Hardness (N)",
                      trace.label = paste0("Temp (","\u00b0C)" )
     ))



# Quantity of drug released in 120 min (Q120)

reldf <- read.csv("reldf.csv")
head(reldf)

reldf$Time <- as.factor(reldf$Time)
reldf$Temp <- as.factor(reldf$Temp)

str(reldf)
cat(names(reldf), fill = TRUE, sep = ", ")

# Q120, Time, Temp

tabyl(reldf, Time, Temp)

aggregate(Q120 ~ Time + Temp, data = reldf, mean)
aggregate(Q120 ~ Time + Temp, data = reldf, sd)

mrel <- aov(Q120 ~ Time*Temp, data = reldf)
summary(mrel)

with(reldf, 
     interaction.plot(Time, Temp, Q120, type="b",
                      pch=c(16, 18),
                      xlab = "Time (week)",
                      ylab = "Mean Q120 (%)",
                      trace.label = paste0("Temp (","\u00b0C)" )
     ))



# Drug content

condf <- read.csv("condf.csv")
head(condf)

condf$Time <- as.factor(condf$Time)
condf$Temp <- as.factor(condf$Temp)

str(condf)
cat(names(condf), fill = TRUE, sep = ", ")

# Content, Time, Temp

tabyl(condf, Time, Temp)

aggregate(Content ~ Time + Temp, data = condf, mean)
aggregate(Content ~ Time + Temp, data = condf, sd)

mcon <- aov(Content ~ Time*Temp, data = condf)
summary(mcon)

with(condf, 
     interaction.plot(Time, Temp, Content, type="b",
                      pch=c(16, 18),
                      xlab = "Time (week)",
                      ylab = "Mean Drug Content (%)",
                      trace.label = paste0("Temp (","\u00b0C)" )
     ))






