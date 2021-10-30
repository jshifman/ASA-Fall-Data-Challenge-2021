# Transportation Visualizations
library(tidyverse)
library(readxl)

travel <- read_csv("data/faps_household_puf.csv")
travel <- travel %>%
  select(rural, targetgroup, pctpovguidehh_r, anyvehicle, vehiclenum,
         caraccess, snapnowhh, adltfscat, primstoretravelmode) %>%
  transform(rural = as.factor(rural), anyvehicle = as.factor(anyvehicle),
            vehiclenum = as.factor(vehiclenum), caraccess = as.factor(caraccess), snapnowhh = as.factor(snapnowhh),
            adltfscat = as.factor(adltfscat))
travel$targetgroup <- factor(travel$targetgroup, levels = c("4","1","2","3"))
travel$primstoretravelmode[travel$primstoretravelmode > 3] <- 4
travel$primstoretravelmode <- as.factor(travel$primstoretravelmode)

allStatesDataFrame <- read_xlsx("data/This is Statistics Fall Data Challenge 2021 Dataset_ Food Access Research Atlas Data 2019-ASAFDC 2021.xlsx", 3)
allStatesDataFrame <- transform(allStatesDataFrame, CensusTract = as.numeric(CensusTract),
                                State = as.factor(State), County = as.factor(County), LA1and10 = as.factor(LA1and10),
                                MedianFamilyIncome = as.numeric(MedianFamilyIncome), LILATracts_1And10 = as.factor(LILATracts_1And10),
                                LAPOP1_10 = as.numeric(LAPOP1_10), LALOWI1_10 = as.numeric(LALOWI1_10),
                                lapop1 = as.numeric(lapop1), lapop1share = as.numeric(lapop1share),
                                lalowi1 = as.numeric(lalowi1), lalowi1share = as.numeric(lalowi1share),
                                lakids1 = as.numeric(lakids1), lakids1share = as.numeric(lakids1share),
                                laseniors1 = as.numeric(laseniors1), laseniors1share = as.numeric(laseniors1share),
                                lawhite1 = as.numeric(lawhite1), lawhite1share = as.numeric(lawhite1share),
                                lablack1 = as.numeric(lablack1), lablack1share = as.numeric(lablack1share),
                                laasian1 = as.numeric(laasian1), laasian1share = as.numeric(laasian1share),
                                lanhopi1 = as.numeric(lanhopi1), lanhopi1share = as.numeric(lanhopi1share),
                                laaian1 = as.numeric(laaian1), laaian1share = as.numeric(laaian1share),
                                laomultir1 = as.numeric(laomultir1), laomultir1share = as.numeric(laomultir1share),
                                lahisp1 = as.numeric(lahisp1), lahisp1share = as.numeric(lahisp1share),
                                lahunv1 = as.numeric(lahunv1), lahunv1share = as.numeric(lahunv1share),
                                lasnap1 = as.numeric(lasnap1), lasnap1share = as.numeric(lasnap1share),
                                lapop10 = as.numeric(lapop10), lapop10share = as.numeric(lapop10share),
                                lalowi10 = as.numeric(lalowi10), lalowi10share = as.numeric(lalowi10share),
                                lakids10 = as.numeric(lakids10), lakids10share = as.numeric(lakids10share),
                                laseniors10 = as.numeric(laseniors10), laseniors10share = as.numeric(laseniors10share),
                                lawhite10 = as.numeric(lawhite10), lawhite10share = as.numeric(lawhite10share),
                                lablack10 = as.numeric(lablack10), lablack10share = as.numeric(lablack10share),
                                laasian10 = as.numeric(laasian10), laasian10share = as.numeric(laasian10share),
                                lanhopi10 = as.numeric(lanhopi10), lanhopi10share = as.numeric(lanhopi10share),
                                laaian10 = as.numeric(laaian10), laaian10share = as.numeric(laaian10share),
                                laomultir10 = as.numeric(laomultir10), laomultir10share = as.numeric(laomultir10share),
                                lahisp10 = as.numeric(lahisp10), lahisp10share = as.numeric(lahisp10share),
                                lahunv10 = as.numeric(lahunv10), lahunv10share = as.numeric(lahunv10share),
                                lasnap10 = as.numeric(lasnap10), lasnap10share = as.numeric(lasnap10share),
                                HUNVFlag = as.factor(HUNVFlag))
ggplot(travel) +
  geom_bar(aes(x = targetgroup, fill = primstoretravelmode), position = "fill") +
  labs(title = "Share of Transportation Methods", x = "", y = "% of Households Using Travel Mode", fill = "Travel Method") +
  scale_fill_discrete(labels = c("Drive Own Car", "Use Someone Else's Car", "Someone Else Drives Me", "Walk, Bike, Public Transport, or Other")) +
  scale_x_discrete(labels = str_wrap(c("4" = "SNAP", "1" = "Non-SNAP, Income below 100% of poverty", "2" = "Non-SNAP, Income 100-185% of poverty", "3" = "Non-SNAP, Income above 185% of poverty"), width = 10))

ggplot(allStatesDataFrame) +
  geom_point(aes(x = MedianFamilyIncome, y = lahunv1share), alpha = .2) +
  geom_point(aes(x = MedianFamilyIncome, y = lahunv10share), alpha = .2) +
  labs(title = "Share of Tract Population Without a Car and Far From a Supermarket", x = "Median Family Income", y = "")

