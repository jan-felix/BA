library(lmtest)
library(plyr)
library(dplyr)
library(tidyr)
library(sandwich)
library(plm)
library(stargazer)
#Dieses Skript macht die Regressionen und erstellt anschlie?end die Tabellen.
rm(list=ls())

panel<-read.csv("finalpanelIRB.csv") #Datensatz laden
#Variablen Umformen
###Logs
panel= panel %>%
  mutate(logCDSit=log(CDSit))%>% #CDS von Bank i zum Zeitpunkt t -> CDSit
  mutate(logsovindexexdomCDS=log(sovindexexdomCDS))%>% #CDS-Index
  mutate(logVOLA=log(EUROSTOXX50VOLA))%>% #Vola
  mutate(logEuroSTOXX600=log(EuroSTOXX600))%>% #Europe Stoxx 600 
  mutate(logCDSisov=log(CDSisov))%>% #Sovereign CDS von Bank i -> CDSisov
  mutate(logitraxxfin=log(ItraxxFIN)) #iTraxx Financials

detach(package:plyr)    #Nur zur sicherheit, manchmal funktionieren plyr und dplyr nicht zusammen
library(dplyr)
#Yield in real growth rates transformieren
panel= panel%>%
  group_by(BankName, Period)%>%
  mutate(gsovindexexdomYield=(sovindexexdomYield/lag(sovindexexdomYield,n=1L)-1))%>%
  mutate(gYieldisov=(Yieldisov/lag(Yieldisov,n=1L)-1))%>%
  ungroup()

#Share = Sovereign Ratio
panel=panel%>%
  mutate(share=(RwSovX/CommonEquityT1))%>%
  mutate(avgshare=mean(share)) #f?rs skalieren


panel<-panel%>%
  mutate(scaledshare=share-avgshare)%>% #scaledshare -> 0 = average
  group_by(BankName,Period) %>% #first-differences
  mutate(logfdCDSit=log(CDSit)-log(lag(CDSit, n=1L)))%>%
  mutate(logfdsovindexexdomCDS=log(sovindexexdomCDS)-log(lag(sovindexexdomCDS, n=1L)))%>%
  mutate(logfdVOLA=log(EUROSTOXX50VOLA)-log(lag(EUROSTOXX50VOLA, n=1L)))%>%
  mutate(logfdEuroSTOXX600=log(EuroSTOXX600)-log(lag(EuroSTOXX600, n=1L)))%>%
  mutate(logfdCDSisov=log(CDSisov)-log(lag(CDSisov, n=1L)))%>%
  mutate(logfditraxxfin=logitraxxfin-lag(logitraxxfin,n=1L))%>%
  ungroup()
  
#interaction terms
panel<-panel%>%               
  mutate(KKSCDS=logfdsovindexexdomCDS*scaledshare)%>%
  mutate(KKSYield=(gsovindexexdomYield*scaledshare))%>%
  ungroup()

#Zeitr?ume aufteilen
beforesovbuf <- c("201012","201109","201112")
duringsovbuf <- c("201206","201212","201306","201312","201412")
aftersovbuf <- c("201506","201512")

panel <- panel%>%mutate(sovbuf=ifelse(Period %in% beforesovbuf, "b", # "b" ist before, damit "a" (was R autamtisch als base category nimmt) "during" darstellt.
                                      ifelse(Period %in% duringsovbuf, "a","c")))


#Panel aufteilen
corebanks<- c("AT","DE","DK","FR","GB","NL","BE")
periphbanks <- c("BG","ES","IE","IT","PT")
panelc <- subset(panel, Country %in% corebanks)
panelp <- subset(panel, Country %in% periphbanks)



#Standard Fixed effects model
##Table 2: Fundamental Regressions -> Stufenweiser Aufbau des Models
#Konkrete Spezifikation: "within" model mit effect "individual" -> Bank FE ohne Dummies
basic<- plm(logfdCDSit~
                  logfdsovindexexdomCDS+scaledshare+KKSCDS+
                  logfdCDSisov+
                  CET1ratio
                ,data = panel, model = "within" ,index=c("BankName","Date"), effect="individual")


market<- plm(logfdCDSit~
              logfdsovindexexdomCDS+scaledshare+KKSCDS+
              logfdCDSisov+
              CET1ratio
            +logfdVOLA+
              logfdEuroSTOXX600+
              logfditraxxfin
            +factor(Period) #Period als Dummie eingef?gt
            ,data = panel, model = "within" ,index=c("BankName","Date"), effect="individual")

full <- plm(logfdCDSit~
              logfdsovindexexdomCDS+scaledshare+KKSCDS+
              logfdCDSisov+
              CET1ratio
            +logfdVOLA+
              logfdEuroSTOXX600+
              logfditraxxfin+
            +factor(Period)
            +factor(BankName)*(logfdVOLA+logfdEuroSTOXX600+logfditraxxfin+logfdCDSisov) #Bank-specific coefficients
            ,data = panel, model = "within" ,index=c("BankName","Date"), effect="individual")

#Clustered Standard errors:
#"Arellano" -> robust against heteroskedasticity and serial correlation in FE panelmodels
#"sss" -> statas small sample correction
basiccse        <- coeftest(basic, vcov=vcovHC(basic,method = "arellano" ,type="sss",cluster="group"))
mktctrlscse     <- coeftest(market, vcov=vcovHC(market, method = "arellano" ,type="sss",cluster = "group"))
fullcse         <- coeftest(full, vcov=vcovHC(full, method = "arellano" ,type="sss",cluster="group"))


#Erstelle Tabelle 2 -> 
xtab <- stargazer(basic,market,full, #R? und Coefficients
                  se= list(basiccse[,"Std. Error"],mktctrlscse[,"Std. Error"],fullcse[,"Std. Error"]) # Std. Errors aus den clustered error estimates
                  ,table.placement = "H",title = "Model Build-Up",
                  add.lines = list(c("Time FE","N","Y","Y"), #Diese infos habe ich am ende nochmal manuell gek?rzt, das ist optik
                                   c("Individual Coefficients","N","N","Y"),
                                   c("Clustered SE", "Bank","Bank","Bank")),
                  column.labels = c("Basic Model","Basic Model with Market Controls","Complete Model"),
                  dep.var.labels = "$\\Delta{\\log{CDS_{i,j,t}}}$",
                  covariate.labels	= c("$\\Delta\\log{SovereignIndex}_{i,t}$","$SovereignRatio_{i,t}$"
                                       ,"$\\Delta\\log{SovereignIndex}_{i,t}*SovereignRatio_{i,t}$",
                                       "$\\Delta\\log{DomesticCDS}_{j,t}$","$CET1ratio_{i,t}$",
                                       "$\\Delta\\log{Vola}_{t}$","$\\Delta\\log{EuroSTOXX600}_{t}$",
                                       "$\\Delta\\log{iTraxxFin}_{t}$"),
                  column.sep.width = "1pt",single.row = F,
                  style="aer"
                  )

xtab2<-stargazer(basic,market,full) #die Tabelle mit naiven standard errors

#Funktion um die naiven SEs in eckigen klammern unter die robusten zu machen
n <- length(coef(full))             
sq1 <- seq(16, 16 + (n-1)*3, by=3)
sq2 <- seq(16, 16 + (n-1)*3, by=3)#...r?ckblickend etwas redundant...
xtab2[sq2]<-gsub("\\(","\\[",xtab2[sq2])
xtab2[sq2]<-gsub("\\)","\\]",xtab2[sq2])
xtab[sq2] <- paste(xtab2[sq1], " & & \\\\\n", sep="\n")
xtab <- gsub("\\begin{tabular}","\\resizebox{0.9\\textwidth}{!}{\\begin{tabular}", xtab,fixed=T)
xtab <- gsub("\\end{tabular}","\\end{tabular}}", xtab,fixed=T)
#print table 2:
sink("ModelBuilding.tex", append=FALSE, split=FALSE)
cat(xtab, sep="\n")
sink()

###Table 3: 2 Dimensions of Sovereign Spillovers
#drei mal dieselbe regression mit anderen daten.
#Full panel -> data= panel
sovbufCDS<- plm(logfdCDSit~
             factor(sovbuf)*(logfdsovindexexdomCDS+scaledshare+KKSCDS)+#factor(sovbuf) hat 3 levels -> es werden before und after dummies erstellt
             logfdCDSisov+
             CET1ratio
           +logfdVOLA+
             logfdEuroSTOXX600+
             logfditraxxfin
           +factor(Period)
           +factor(BankName)*(logfdVOLA+logfdEuroSTOXX600+logfditraxxfin+logfdCDSisov)
           ,data = panel, model = "within" ,index=c("BankName","Date"), effect="individual")

#Core -> data = panelc f?r panel core
sovbufCDSC<- plm(logfdCDSit~
                  factor(sovbuf)*(logfdsovindexexdomCDS+scaledshare+KKSCDS)+
                  logfdCDSisov+
                  CET1ratio
                +logfdVOLA+
                  logfdEuroSTOXX600+
                  logfditraxxfin
                +factor(Period)
                +factor(BankName)*(logfdVOLA+logfdEuroSTOXX600+logfditraxxfin+logfdCDSisov)
                ,data = panelc, model = "within" ,index=c("BankName","Date"), effect="individual")

#Periphery -> data = panelp
sovbufCDSP<- plm(logfdCDSit~
                  factor(sovbuf)*(logfdsovindexexdomCDS+scaledshare+KKSCDS)+
                  logfdCDSisov+
                  CET1ratio
                +logfdVOLA+
                  logfdEuroSTOXX600+
                  logfditraxxfin
                +factor(Period)
                +factor(BankName)*(logfdVOLA+logfdEuroSTOXX600+logfditraxxfin+logfdCDSisov)
                ,data = panelp, model = "within" ,index=c("BankName","Date"), effect="individual")

#Clustered SE
sovbufCDScse    <- coeftest(sovbufCDS, vcov=vcovHC(sovbufCDS, method = "arellano" ,type="sss",cluster="group"))
sovbufCDScseP   <- coeftest(sovbufCDSP, vcov=vcovHC(sovbufCDSP, method = "arellano" ,type="sss",cluster="group"))
sovbufCDScseC   <- coeftest(sovbufCDSC, vcov=vcovHC(sovbufCDSC, method = "arellano" ,type="sss",cluster="group"))

#Tabelle erstellen (99% gleich wie oben)
xtab <-stargazer(sovbufCDS,sovbufCDSP,sovbufCDSC, #type = "text", 
          se= list(sovbufCDScse[,"Std. Error"],sovbufCDScseP[,"Std. Error"],sovbufCDScseC[,"Std. Error"]
          ),table.placement = "H",title = "Core vs Periphery",
          add.lines = list(c("Time FE","Y","Y","Y"),
                           c("Individual Coefficients","Y","Y","Y"),
                           c("Clustered SE", "Bank","Bank","Bank")),
          column.labels = c("Full Sample","Periphery","Core"),
          dep.var.labels = "$\\Delta{\\log{CDS_{i,j,t}}}$",
          covariate.labels	= c("$\\Delta\\log{SovereignIndex}_{i,t}$",
                               "$SovereignRatio_{i,t}$"
                               ,"$\\Delta\\log{SovereignIndex}_{i,t}*SovereignRatio_{i,t}$",
                               "$Before*\\Delta\\log{SovereignIndex}_{i,t}$","$After*\\Delta\\log{SovereignIndex}_{i,t}$",
                               "$Before*SovereignRatio_{i,t}$","$After*SovereignRatio_{i,t}$",
                               "$Before*\\Delta\\log{SovereignIndex}_{i,t}*SovereignRatio_{i,t}$","$After*\\Delta\\log{SovereignIndex}_{i,t}*SovereignRatio_{i,t}$"
                               ),
          column.sep.width = "1pt",single.row = F,keep = c("logfdsovindexexdomCDS","scaledshare", #keep beh?lt nur die interessanten Coefficients
                                                           "KKSCDS"),
          style="aer")

xtab2<-stargazer(sovbufCDS,sovbufCDSP,sovbufCDSC,keep = c("logfdsovindexexdomCDS","scaledshare",
                                                           "KKSCDS"))
n <- 9
sq1 <- seq(16, 16 + (n-1)*3, by=3)
sq2 <- seq(16, 16 + (n-1)*3, by=3)
xtab2[sq2]<-gsub("\\(","\\[",xtab2[sq2])
xtab2[sq2]<-gsub("\\)","\\]",xtab2[sq2])
xtab[sq2] <- paste(xtab2[sq1], " & & \\\\\n", sep="\n")
xtab <- gsub("\\begin{tabular}","\\resizebox{0.9\\textwidth}{!}{\\begin{tabular}", xtab,fixed=T)
xtab <- gsub("\\end{tabular}","\\end{tabular}}", xtab,fixed=T)
sink("SovereignBuffer.tex", append=FALSE, split=FALSE)
cat(xtab, sep="\n")
sink()


#Tabelle 4: Das gleiche spiel mit Yields
#Full sample
sovbufYield <- plm(logfdCDSit~
                   factor(sovbuf)*(gsovindexexdomYield+scaledshare+KKSYield)+
                   gYieldisov+
                   CET1ratio
                   +logfdVOLA+
                   logfdEuroSTOXX600+
                   logfditraxxfin
                   +factor(Period)+
                   factor(BankName)*(logfdVOLA+logfdEuroSTOXX600+gYieldisov+logfditraxxfin)
                 ,data = panel, model = "within" ,index=c("BankName","Date"), effect="individual")
#Periphery
sovbufYieldP <- plm(logfdCDSit~
                    factor(sovbuf)*(gsovindexexdomYield+scaledshare+KKSYield)+
                    gYieldisov+
                    CET1ratio
                  +logfdVOLA+
                    logfdEuroSTOXX600+
                    logfditraxxfin
                  +factor(Period)+
                    factor(BankName)*(logfdVOLA+logfdEuroSTOXX600+gYieldisov+logfditraxxfin)
                  ,data = panelp, model = "within" ,index=c("BankName","Date"), effect="individual")
#Core
sovbufYieldC <- plm(logfdCDSit~
                    factor(sovbuf)*(gsovindexexdomYield+scaledshare+KKSYield)+
                    gYieldisov+
                    CET1ratio
                  +logfdVOLA+
                    logfdEuroSTOXX600+
                    logfditraxxfin
                  +factor(Period)+
                    factor(BankName)*(logfdVOLA+logfdEuroSTOXX600+gYieldisov+logfditraxxfin)
                  ,data = panelc, model = "within" ,index=c("BankName","Date"), effect="individual")


#Clustered SE
sovbufYieldcse    <- coeftest(sovbufYield, vcov=vcovHC(sovbufYield, method = "arellano" ,type="sss",cluster="group"))
sovbufYieldcseP   <- coeftest(sovbufYieldP, vcov=vcovHC(sovbufYieldP, method = "arellano" ,type="sss",cluster="group"))
sovbufYieldcseC   <- coeftest(sovbufYieldC, vcov=vcovHC(sovbufYieldC, method = "arellano" ,type="sss",cluster="group"))


#Tabelle 4 Erstellen

xtab <- stargazer(sovbufYield,sovbufYieldP,sovbufYieldC, #type = "text", 
                 se= list(sovbufYieldcse[,"Std. Error"],sovbufYieldcseP[,"Std. Error"],sovbufYieldcseC[,"Std. Error"]
                 ),table.placement = "H",title = "Core vs Periphery Yield",
                 add.lines = list(c("Time FE","Y","Y","Y"),
                                  c("Individual Coefficients","Y","Y","Y"),
                                  c("Clustered SE", "Bank","Bank","Bank")),
                 column.labels = c("Full Sample","Periphery","Core"),
                 dep.var.labels = "$\\Delta{\\log{CDS_{i,j,t}}}$",
                 covariate.labels	= c("$\\Growth\ SovereignIndex_{i,t}$", #Kleines Problem: Am ende muss man in LaTex noch \\Growth\ zu \Grwoth\ machen
                                      "$SovereignRatio_{i,t}$"
                                      ,"$\\Growth\ SovereignIndex_{i,t}*SovereignRatio_{i,t}$",
                                      "$Before*\\Growth\ SovereignIndex_{i,t}$",
                                      "$After*\\Growth\ SovereignIndex_{i,t}$",
                                      "$Before*SovereignRatio_{i,t}$",
                                      "$After*SovereignRatio_{i,t}$",
                                      "$Before*\\Growth\ SovereignIndex_{i,t}*SovereignRatio_{i,t}$",
                                      "$After*\\Growth\ SovereignIndex_{i,t}*SovereignRatio_{i,t}$"
                                      ),keep=c("gsovindexexdomYield","scaledshare","KKSYield"),
                 column.sep.width = "1pt",single.row = F,
                 style="aer"
                 #out="sovereignbufferYield.tex")
)

xtab2<-stargazer(sovbufYield,sovbufYieldP,sovbufYieldC,keep = c("gsovindexexdomYield","scaledshare","KKSYield"))

n <- 9
sq1 <- seq(16, 16 + (n-1)*3, by=3)
sq2 <- seq(16, 16 + (n-1)*3, by=3)
xtab2[sq2]<-gsub("\\(","\\[",xtab2[sq2])
xtab2[sq2]<-gsub("\\)","\\]",xtab2[sq2])
xtab[sq2] <- paste(xtab2[sq1], " & & \\\\\n", sep="\n")
xtab <- gsub("\\begin{tabular}","\\resizebox{0.9\\textwidth}{!}{\\begin{tabular}", xtab,fixed=T)
xtab <- gsub("\\end{tabular}","\\end{tabular}}", xtab,fixed=T)
sink("SovereignBufferYield.tex", append=FALSE, split=FALSE)
cat(xtab, sep="\n")
sink()




###Alles von vorne f?r das Panel mit risk-weights nach StA approach. 
# panel<- readRDS("RDS/cleanpanelprudentsta.RDS") #Standardized Approach
# Nce11<- c("AxaBankEurope","INGGroep",
#           "BancodeSabadell,SA","Mediobanca-BancadiCreditoFinanziarioSpA",
#           "BancaPopolarediMilanoScarl",
#           "Raiffeisen-Landesbanken-HoldingGmbH")
# 
# panel <- panel[!(panel$BankName %in% Nce11),]
# panel <- panel %>% arrange(BankName,Date)
# write.csv(panel, "finalpanelStA.csv")

panel<-read.csv("finalpanelStA.csv")

#Variablen Umformen
###Logs
panel= panel %>%
  mutate(logCDSit=log(CDSit))%>% #CDS von Bank i zum Zeitpunkt t -> CDSit
  mutate(logsovindexexdomCDS=log(sovindexexdomCDS))%>% #CDS-Index
  mutate(logVOLA=log(EUROSTOXX50VOLA))%>% #Vola
  mutate(logEuroSTOXX600=log(EuroSTOXX600))%>% #Europe Stoxx 600 
  mutate(logCDSisov=log(CDSisov))%>% #Sovereign CDS von Bank i -> CDSisov
  mutate(logsovindexexdomYield=log(sovindexexdomYield+0.3))%>% #hier die version mit konstante f?r die logs
  mutate(logYieldisov=log(Yieldisov+0.3))%>% #same
  mutate(logitraxxfin=log(ItraxxFIN)) #iTraxx Financials

detach(package:plyr)    #Nur zur sicherheit, manchmal funktionieren plyr und dplyr nicht zusammen
library(dplyr)
#Yield in real growth rates transformieren
panel= panel%>%
  group_by(BankName, Period)%>%
  mutate(gsovindexexdomYield=(sovindexexdomYield/lag(sovindexexdomYield,n=1L)-1))%>%
  mutate(gYieldisov=(Yieldisov/lag(Yieldisov,n=1L)-1))%>%
  ungroup()

#Share = Sovereign Ratio
panel=panel%>%
  mutate(share=(RwSovX/CommonEquityT1))%>%
  mutate(avgshare=mean(share)) #f?rs skalieren


panel<-panel%>%
  mutate(scaledshare=share-avgshare)%>% #scaledshare -> 0 = average
  group_by(BankName,Period) %>% #first-differences
  mutate(logfdCDSit=log(CDSit)-log(lag(CDSit, n=1L)))%>%
  mutate(logfdsovindexexdomCDS=log(sovindexexdomCDS)-log(lag(sovindexexdomCDS, n=1L)))%>%
  mutate(logfdVOLA=log(EUROSTOXX50VOLA)-log(lag(EUROSTOXX50VOLA, n=1L)))%>%
  mutate(logfdEuroSTOXX600=log(EuroSTOXX600)-log(lag(EuroSTOXX600, n=1L)))%>%
  mutate(logfdCDSisov=log(CDSisov)-log(lag(CDSisov, n=1L)))%>%
  mutate(logfdYieldisov=logYieldisov-lag(logYieldisov, n=1L))%>%
  mutate(logfdsovindexexdomYield=logsovindexexdomYield-lag(logsovindexexdomYield, n=1L))%>%
  mutate(logfditraxxfin=logitraxxfin-lag(logitraxxfin,n=1L))%>%
  ungroup()

#interaction terms
panel<-panel%>%               
  mutate(KKSCDS=logfdsovindexexdomCDS*scaledshare)%>%
  mutate(KKSYield=(gsovindexexdomYield*scaledshare))%>%
  ungroup()

#Zeitr?ume aufteilen
beforesovbuf <- c("201012","201109","201112")
duringsovbuf <- c("201206","201212","201306","201312","201412")
aftersovbuf <- c("201506","201512")

panel <- panel%>%mutate(sovbuf=ifelse(Period %in% beforesovbuf, "b", # "b" ist before, damit "a" (was R autamtisch als base category nimmt) "during" darstellt.
                                      ifelse(Period %in% duringsovbuf, "a","c")))


#Panel aufteilen
corebanks<- c("AT","DE","DK","FR","GB","NL","BE")
periphbanks <- c("BG","ES","IE","IT","PT")
panelc <- subset(panel, Country %in% corebanks)
panelp <- subset(panel, Country %in% periphbanks)



#Tabelle 5: StA Robustness -> exakt die gleiche regression wie oben nur eben mit etwas anderen Holding Daten
sovbufCDS<- plm(logfdCDSit~
                  factor(sovbuf)*(logfdsovindexexdomCDS+scaledshare+KKSCDS)+
                  logfdCDSisov+
                  CET1ratio
                +logfdVOLA+
                  logfdEuroSTOXX600+
                  logfditraxxfin
                +factor(Period)
                +factor(BankName)*(logfdVOLA+logfdEuroSTOXX600+logfditraxxfin+logfdCDSisov)
                ,data = panel, model = "within" ,index=c("BankName","Date"), effect="individual")

sovbufCDSC<- plm(logfdCDSit~
                   factor(sovbuf)*(logfdsovindexexdomCDS+scaledshare+KKSCDS)+
                   logfdCDSisov+
                   CET1ratio
                 +logfdVOLA+
                   logfdEuroSTOXX600+
                   logfditraxxfin
                 +factor(Period)
                 +factor(BankName)*(logfdVOLA+logfdEuroSTOXX600+logfditraxxfin+logfdCDSisov)
                 ,data = panelc, model = "within" ,index=c("BankName","Date"), effect="individual")


sovbufCDSP<- plm(logfdCDSit~
                   factor(sovbuf)*(logfdsovindexexdomCDS+scaledshare+KKSCDS)+
                   logfdCDSisov+
                   CET1ratio
                 +logfdVOLA+
                   logfdEuroSTOXX600+
                   logfditraxxfin
                 +factor(Period)
                 +factor(BankName)*(logfdVOLA+logfdEuroSTOXX600+logfditraxxfin+logfdCDSisov)
                 ,data = panelp, model = "within" ,index=c("BankName","Date"), effect="individual")


sovbufCDScse    <- coeftest(sovbufCDS, vcov=vcovHC(sovbufCDS, method = "arellano" ,type="sss",cluster="group"))
sovbufCDScseP   <- coeftest(sovbufCDSP, vcov=vcovHC(sovbufCDSP, method = "arellano" ,type="sss",cluster="group"))
sovbufCDScseC   <- coeftest(sovbufCDSC, vcov=vcovHC(sovbufCDSC, method = "arellano" ,type="sss",cluster="group"))

xtab <-stargazer(sovbufCDS,sovbufCDSP,sovbufCDSC, #type = "text", 
                 se= list(sovbufCDScse[,"Std. Error"],sovbufCDScseP[,"Std. Error"],sovbufCDScseC[,"Std. Error"]
                 ),table.placement = "H",title = "Core vs Periphery StA",
                 add.lines = list(c("Time FE","Y","Y","Y"),
                                  c("Individual Coefficients","Y","Y","Y"),
                                  c("Clustered SE", "Bank","Bank","Bank")),
                 column.labels = c("Full Sample","Periphery","Core"),
                 dep.var.labels = "$\\Delta{\\log{CDS_{i,j,t}}}$",
                 covariate.labels	= c("$\\Delta\\log{SovereignIndex}_{i,t}$",
                                      "$SovereignRatio_{i,t}$"
                                      ,"$\\Delta\\log{SovereignIndex}_{i,t}*SovereignRatio_{i,t}$",
                                      "$Before*\\Delta\\log{SovereignIndex}_{i,t}$","$After*\\Delta\\log{SovereignIndex}_{i,t}$",
                                      "$Before*SovereignRatio_{i,t}$","$After*SovereignRatio_{i,t}$",
                                      "$Before*\\Delta\\log{SovereignIndex}_{i,t}*SovereignRatio_{i,t}$","$After*\\Delta\\log{SovereignIndex}_{i,t}*SovereignRatio_{i,t}$"
                 ),
                 column.sep.width = "1pt",single.row = F,keep = c("logfdsovindexexdomCDS","scaledshare",
                                                                  "KKSCDS"),
                 style="aer")

xtab2<-stargazer(sovbufCDS,sovbufCDSP,sovbufCDSC,keep = c("logfdsovindexexdomCDS","scaledshare",
                                                          "KKSCDS"))
n <- 9
sq1 <- seq(16, 16 + (n-1)*3, by=3)
sq2 <- seq(16, 16 + (n-1)*3, by=3)
xtab2[sq2]<-gsub("\\(","\\[",xtab2[sq2])
xtab2[sq2]<-gsub("\\)","\\]",xtab2[sq2])
xtab[sq2] <- paste(xtab2[sq1], " & & \\\\\n", sep="\n")
xtab <- gsub("\\begin{tabular}","\\resizebox{0.9\\textwidth}{!}{\\begin{tabular}", xtab,fixed=T)
xtab <- gsub("\\end{tabular}","\\end{tabular}}", xtab,fixed=T)
sink("SovereignBufferSTA.tex", append=FALSE, split=FALSE)
cat(xtab, sep="\n")
sink()
