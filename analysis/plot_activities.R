setwd("/")
library(ggplot2)
library(dplyr)
library(xtable)

df = read.csv("data/labels.csv")
meta = read.csv("data/method.csv")
options(max.print=25000)

df = df %>% left_join(meta, by = c("day"="day", "user_id"="user"))
df = na.omit(df)

(df.count_by_method_activity_type <- df %>% 
  group_by(activity_type, method) %>% 
  summarize(collected = n()) %>%
  arrange(desc(activity_type)))

df.count_by_method = df %>% 
  group_by(method) %>% 
  summarize(collected = n()) %>%
  arrange(desc(collected))

df.count_by_user_day = df %>% 
  group_by(user_id, day) %>% 
  summarize(collected = n()) %>%
  arrange(desc(user_id))
print.data.frame(df.count_by_user_day)

df.count_by_classes = df %>%
  group_by(activity_type, method) %>%
  summarize(collected = n()) %>%
  arrange(desc(activity_type))
print.data.frame(df.count_by_user_class)

(plot_collected <- ggplot(df.count_by_classes,
       aes(x=factor(method),y=collected,fill=factor(activity_type)),
       cex = 0.75) +
  labs(x="Method",y="Activity labels",fill = "Activity class")  +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette="Paired")+
  theme(plot.title = element_text(hjust = 0.5),
        text=element_text(size=12,family="ArialMT"),
        legend.position="left"))
ggsave('fig/plot_collected.pdf',plot = plot_collected,device = "pdf")
