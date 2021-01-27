setwd("/")
library(ggplot2)
library(dplyr)
library(xtable)

df = read.csv("data/accuracy.csv")
meta = read.csv("data/method.csv")
options(max.print=25000)

df = df %>% left_join(meta, by = c("day"="day", "user"="user"))
df = na.omit(df)

df %>% 
  group_by(method) %>% 
  summarize(recall = mean(recall)) %>%
  arrange(desc(recall))

df.fscore <- df %>% 
  group_by(method) %>% 
  summarize(result = mean(fscore)) %>%
  arrange(desc(result))

df %>% 
  group_by(method) %>% 
  summarize(precision = mean(precision)) %>%
  arrange(desc(precision))

(df.mean_user <- df %>% 
    group_by(user,method) %>% 
    summarise(fscore = mean(fscore), 
              recall = mean(recall), 
              precision = mean(precision)))
xtable(df.mean_user)

df.mean <- df %>% 
  group_by(method) %>% 
  summarise(fscore = mean(fscore), 
            recall = mean(recall), 
            precision = mean(precision))

df.result <- data.frame(method=c(rep("Proposed",3),
                                 rep("Traditional",3), 
                                 rep("Without",3)),
                        measure=c("F1-Score","Recall","Precision"),
                        result=c(df.mean$fscore[1],df.mean$recall[1],df.mean$precision[1],
                                 df.mean$fscore[2],df.mean$recall[2],df.mean$precision[2],
                                 df.mean$fscore[3],df.mean$recall[3],df.mean$precision[3]))
scaleFUN <- function(x) sprintf("%.2f", x)

specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

(plot_performace <- ggplot(df.result, aes(x=measure, y=result, fill=method))+
    labs(x="Performance metric",y="Performance score", fill="Method")  +
    theme(plot.title = element_text(hjust = 0.5),
          text=element_text(size=12,family="ArialMT"),
          legend.position="bottom") +
    scale_fill_brewer(palette="Paired")+
    geom_bar(stat = "identity",position=position_dodge())+
    geom_text(aes( label = specify_decimal(result,4),
                   y=result), 
              stat= "identity", vjust = 1.6,position = position_dodge(0.9), 
              size=3, family="ArialMT"))
ggsave('fig/plot_performace.pdf',plot = plot_performace,device = "pdf")
