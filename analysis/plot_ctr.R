setwd("/")
library(ggplot2)
library(dplyr)
library(xtable)

df = read.csv("data/notification_logs.csv")
meta = read.csv("data/method.csv")
options(max.print=25000)

df = na.omit(df)

df %>% summarize(collected = n())

(df.count_by_method <- df %>% 
    group_by(method) %>% 
    summarize(ctr = ((sum(clicked)/sum(sent))*100)) %>%
    arrange(desc(method)))

specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

(plot_ctr <- ggplot(df.count_by_method, aes(x=method, y=ctr, fill="ctr"))+
    labs(x="Method",y="Click-through rate (CTR) [%]")  +
    theme(plot.title = element_text(hjust = 0.5),
          text=element_text(size=12,family="ArialMT"),
          legend.position="none") +
    scale_fill_brewer(palette="Paired")+
    geom_bar(stat = "identity",position=position_dodge())+
    geom_text(aes( label = specify_decimal(ctr,1),
                   y=ctr), 
              stat= "identity", vjust = 1.6,position = position_dodge(0.9), 
              size=5, family="ArialMT"))
ggsave('fig/plot_ctr.pdf',plot = plot_ctr,device = "pdf")
