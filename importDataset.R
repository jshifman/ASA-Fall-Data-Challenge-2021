library("readxl")
library(tidyverse)
library(corrplot)
allStatesDataFrame <- read_xlsx("This is Statistics Fall Data Challenge 2021 Dataset_ Food Access Research Atlas Data 2019-ASAFDC 2021.xlsx", 3)
californiaDataFrame <- read_xlsx("This is Statistics Fall Data Challenge 2021 Dataset_ Food Access Research Atlas Data 2019-ASAFDC 2021.xlsx", 8)
allStatesDataFrame <- transform(allStatesDataFrame, CensusTract = as.numeric(CensusTract),
                                State = as.factor(State), County = as.factor(County),
                                MedianFamilyIncome = as.numeric(MedianFamilyIncome),
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
                                lasnap10 = as.numeric(lasnap10), lasnap10share = as.numeric(lasnap10share))
res <- cor(allStatesDataFrame, use = "complete.obs")
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)