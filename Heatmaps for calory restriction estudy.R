# 1. Set working directory ----
setwd("D:/IDIBAPS/Macarena")

# 2. Load libraries ----
library(readxl)
library(reshape2)
library(latticeExtra) 
library(lattice)

# 3. Load data ----
Exp_Macarena_adapted_for_R <- read_excel("Exp_Macarena_adapted_for_R.xlsx")
df <- Exp_Macarena_adapted_for_R
Experiment_design <- read_excel("Experiment_design.xlsx", 
                                sheet = "Mice", col_names = FALSE)
Time_design <- read_excel("Experiment_design.xlsx", 
                                sheet = "Time", col_names = FALSE)
# 4. Pre-process data ----

rownames(df) <- df$Day
df[,-1] <- apply(df[,-1],2,as.numeric)
df2 <- t(df)
colnames(df2) <- paste("Day", colnames(df2), sep = "_")
df2 <- df2[-1,]
df3 <- melt(data = df2, id.vars = "Reaction_time", measure.vars = c("Days","Mice"))

mice <- rownames(df2)
df2 <- apply(df2,2,as.numeric)

colnames(Time_design) <- Time_design[1,]
Time_design <- Time_design[-1,]
Time_design$Day <- paste("Day", Time_design$Day, sep = "_")
rownames(Time_design) <- Time_design$Day
Before <- rownames(Time_design)[Time_design$Phase=='Before_cross-over']
After <- rownames(Time_design)[Time_design$Phase=='After_cross-over']

colnames(Experiment_design) <- c("Food", "Mouse") 
rownames(Experiment_design) <- Experiment_design$Mouse
Fed <- rownames(Experiment_design)[Experiment_design$Food=='Fed']
Restr <- rownames(Experiment_design)[Experiment_design$Food=='Food Restricted']

# 5. Plot ----
pdf(file = "Levelplot all sections toguether.pdf",width = 10, height = 20) 
levelplot(value ~ Var2 * Var1, df3, xlab="Opening lapse (Days)", ylab = "Mice",aspect="iso", scales=list(x=list(rot=90)),
          panel = panel.levelplot.points,cex = 0.2,main = "Time until mice open the door.") + 
  layer_(panel.2dsmoother(..., n = 300)) 

contourplot(value ~ Var2 * Var1, df3, xlab="Opening lapse (Days)", ylab = "Mice",aspect="iso", scales=list(x=list(rot=90)),
            main = "Time until mice open the door.", panel = panel.levelplot.points, cex = 1.5) + 
  layer_(panel.2dsmoother(..., n = 200)) 

levelplot(t(df2), col.regions = terrain.colors(1000),main = "Time until mice open the door.", xlab="Opening lapse (Days)", ylab = "Mice",
          aspect="iso", scales=list(x=list(rot=90))) 
levelplot(t(df2), col.regions = cm.colors(1000),main = "Time until mice open the door.", xlab="Opening lapse (Days)", ylab = "Mice",aspect="iso", scales=list(x=list(rot=90)))
library(RColorBrewer)
coul <- colorRampPalette(brewer.pal(8, "PiYG"))(30)
levelplot(t(df2), col.regions = coul,main = "Time until mice open the door.", xlab="Opening lapse (Days)", ylab = "Mice",aspect="iso", scales=list(x=list(rot=90))) 
library(viridisLite)
coul <- viridis(100)
levelplot(t(df2), col.regions = coul,main = "Time until mice open the door.", xlab="Opening lapse (Days)", ylab = "Mice",aspect="iso", scales=list(x=list(rot=90)))  
levelplot(t(df2), col.regions = magma(100),main = "Time until mice open the door.", xlab="Opening lapse (Days)", ylab = "Mice",aspect="iso", scales=list(x=list(rot=90))) 
levelplot(t(df2), col.regions=colorRampPalette(c("blue", "yellow"), space = "rgb")(120),main = "Time until mice open the door.", xlab="Opening lapse (Days)", ylab = "Mice",aspect="iso", scales=list(x=list(rot=90))) 
dev.off()

# 6. Split sections and try it again ----

fed <- as.data.frame(df3[df3$Var1 %in% Fed,])
restr <- as.data.frame(df3[df3$Var1 %in% Restr,])
fed_a <- fed[fed$Var2 %in% After,]
fed_b <- fed[fed$Var2 %in% Before,]
restr_a <- restr[restr$Var2 %in% After,]
restr_b <- restr[restr$Var2 %in% Before,]

pdf(file = "Levelplot all sections splited.pdf",width = 10, height = 20) 
levelplot(value ~ Var2 * Var1, fed_b, xlab="Opening lapse (Days)", ylab = "Mice",aspect="iso", scales=list(x=list(rot=90)),
          region = TRUE,pretty = FALSE,cuts = 15, 
          panel = panel.levelplot.points,cex = 0.000000001,main = "Fed mice, before cross-over") + 
  layer_(panel.2dsmoother(..., n = 300)) 
levelplot(value ~ Var2 * Var1, fed_a, xlab="Opening lapse (Days)", ylab = "Mice",aspect="iso", scales=list(x=list(rot=90)),
          region = TRUE,pretty = FALSE,cuts = 15, 
          panel = panel.levelplot.points,cex = 0.000000001,main = "Fed mice, after cross-over") + 
  layer_(panel.2dsmoother(..., n = 300)) 
levelplot(value ~ Var2 * Var1, restr_b, xlab="Opening lapse (Days)", ylab = "Mice",aspect="iso", scales=list(x=list(rot=90)),
          region = TRUE,pretty = FALSE,cuts = 15, 
          panel = panel.levelplot.points,cex = 0.000000001,main = "Food restricted mice, before cross-over") + 
  layer_(panel.2dsmoother(..., n = 300)) 
levelplot(value ~ Var2 * Var1, restr_a, xlab="Opening lapse (Days)", ylab = "Mice",aspect="iso", scales=list(x=list(rot=90)),
          region = TRUE,pretty = FALSE,cuts = 15, 
          panel = panel.levelplot.points,cex = 0.000000001,main = "Food restricted mice, after cross-over") + 
  layer_(panel.2dsmoother(..., n = 300)) 
dev.off()
