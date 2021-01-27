setwd("/")
library(ggplot2)
library(dplyr)
library(xtable)

df = read.csv("data/accuracy.csv")
meta = read.csv("data/method.csv")
options(max.print=25000)

df = df %>% left_join(meta, by = c("day"="day", "user"="user"))
df = na.omit(df)

scaleFUN <- function(x) sprintf("%.2f", x)

specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

(df.mean_user <- df %>% 
    group_by(user,method) %>% 
    summarise(fscore = sprintf("%0.3f",mean(fscore)), 
              precision = sprintf("%0.3f",mean(precision)),
              recall = sprintf("%0.3f",mean(recall)))
)
xtable(df.mean_user)

(df.recall <- df %>% 
  group_by(method,model) %>% 
  summarize(recall = mean(recall)) %>%
  arrange(desc(model)))

(plot_recall <- ggplot(df.recall, aes(x=model, y=recall, fill=method))+
    labs(x="Classification model",y="Recall", fill="Method")  +
    theme(plot.title = element_text(hjust = 0.5),
          text=element_text(size=12,family="ArialMT"),
          legend.position="bottom") +
    scale_fill_brewer(palette="Paired")+
    geom_bar(stat = "identity",position=position_dodge())+
    geom_text(aes( label = specify_decimal(recall,3),
                   y=recall), 
              stat= "identity", vjust = 1.6,position = position_dodge(0.9), 
              size=3.5, family="ArialMT"))
ggsave('fig/plot_recall.pdf',plot = plot_recall,device = "pdf")

#######################################################3

(df.fscore <- df %>% 
    group_by(method,model) %>% 
    summarize(fscore = mean(fscore)) %>%
    arrange(desc(model)))

(plot_fscore <- ggplot(df.fscore, aes(x=model, y=fscore, fill=method))+
    labs(x="Classification model",y="F1-score", fill="Method")  +
    theme(plot.title = element_text(hjust = 0.5),
          text=element_text(size=12,family="ArialMT"),
          legend.position="bottom") +
    scale_fill_brewer(palette="Paired")+
    geom_bar(stat = "identity",position=position_dodge())+
    geom_text(aes( label = specify_decimal(fscore,3),
                   y=fscore), 
              stat= "identity", vjust = 1.6,position = position_dodge(0.9), 
              size=3.5, family="ArialMT"))
ggsave('fig/plot_fscore.pdf',plot = plot_fscore,device = "pdf")

#######################################################3

(df.precision <- df %>% 
    group_by(method,model) %>% 
    summarize(precision = mean(precision)) %>%
    arrange(desc(model)))

(plot_precision <- ggplot(df.precision, aes(x=model, y=precision, fill=method))+
    labs(x="Classification model",y="Precision", fill="Method")  +
    theme(plot.title = element_text(hjust = 0.5),
          text=element_text(size=12,family="ArialMT"),
          legend.position="bottom") +
    scale_fill_brewer(palette="Paired")+
    geom_bar(stat = "identity",position=position_dodge())+
    geom_text(aes( label = specify_decimal(precision,3),
                   y=precision), 
              stat= "identity", vjust = 1.6,position = position_dodge(0.9), 
              size=3.5, family="ArialMT"))
ggsave('fig/plot_precision.pdf',plot = plot_precision,device = "pdf")
