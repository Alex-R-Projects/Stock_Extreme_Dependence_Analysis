###Abstract##
This paper investigates the evolving patterns of risk and dependence between US equities in the S\&P 500 Index, focusing on three major sectors. Communication Services, Consumer Discretionary, and Consumer Staples. Using GARCH-filtered log returns, we compare three complementary dependence measures, Spearman correlation, mutual information, and tail dependence coefficient, under both stable market conditions and the COVID-19-induced downturn. Our findings reveal that while traditional measures capture general co-movement, they often underestimate hidden systemic risks that emerge during crises. In particular, tail dependence analysis uncovers critical vulnerabilities that are not apparent through correlation or information-based metrics alone. Furthermore, cross-sector analysis highlights how diversification benefits erode when market stress escalates, as correlations and joint crash risks rise across traditionally independent industries. This study emphasizes the need for dynamic and tail-sensitive approaches in portfolio construction and risk management, providing practical insights for investors navigating increasingly volatile markets.

This github is the code that performed the analysis for the paper "Beyond Correlation:
An Analysis of Risk in the S&P 500 Index". Find the full paper here: 


Any question reguarding codes, or question feel free to contact, information found on my profile. Thank you.

#Requirements#
library(tidyverse)
library(tidyselect)
library(tidyquant)
library(quantmod)
library(timetk)
library(copula)
library(VineCopula)
library(cowplot)
library(infotheo)
library(GGally)
library(nortest)
library(car)
library(knitr)
library(kableExtra)
library(pander)
library(patchwork)
library(rugarch)
library(FinTS) 
library(reshape2)
library(viridis)
library(tibble)
library(ggpubr)
