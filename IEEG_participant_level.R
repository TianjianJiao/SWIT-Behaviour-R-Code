library("dplyr")
library("readr")
library(ggplot2)
library(car)
library(tidyr)
library(rlang)
library(tibble)
library(gt)


#Set Colour for later use
okabe_ito = c("#000000","#E69F00","#56B4E9","#009E73","#CC79A7",
              "#0072B2","#D55E00","#F0E442")

location = "D:/Mike/Cogitate Dataset/MEG/20231231_cog_exp1_raw_ecog_2be446c1-aaf3-43b7-a0e5-0e0fe482144d/mnt/beegfs/workspace/2023-0385-Cogitatedatarelease/RELEASE/COG_ECOG_EXP1_RELEASE"

csv_list = list.files(
  path = location, 
  pattern = "\\.csv$", 
  recursive = TRUE, 
  full.names = TRUE
)
filter_criteria = c("Beh_1_RawDurR1.csv","Beh_1_RawDurR2.csv","Beh_1_RawDurR3.csv","Beh_1_RawDurR4.csv","Beh_1_RawDurR5.csv")
combined_criteria= paste(filter_criteria, collapse = "|")
filtered_list = csv_list[grepl(combined_criteria, csv_list)]

read_and_mutate.csv = function(path){
  file_name = basename(path)%>%strsplit(split = "_")%>%unlist()
  id = file_name[[1]]
  output = read.csv(path)%>%mutate(id = id)
  output
}

rawdata = lapply(filtered_list,read_and_mutate.csv)%>%do.call(what =rbind)
individual_data = do.call(rbind,rawdata)%>%split(rawdata$id)
labelled_data = rawdata %>%group_by(id,block,miniBlock,trial)%>%
  mutate(responded = case_when(
    "Response" %in% eventType ~ 1,
    .default =  0
  ),correct = case_when(
    dsrdResponse == responded ~ 1,
    .default =  0
  ))%>% 
  mutate(correct = as.factor(correct)) %>%
  ungroup()%>%
  mutate(resp_type = case_when(
    responded == 0 & correct ==1 ~ "correct_reject",
    responded == 1 & correct ==1 ~ "hit",
    responded == 0 & correct ==0 ~ "miss",
    responded == 1 & correct ==0 ~ "fa"
  ))
rt_df = labelled_data %>% filter(responded==1)%>%
  group_by(id,block,miniBlock,trial)%>% 
  mutate(rt = time[eventType == "Response"]-time[eventType == "Stimulus"])%>%
  group_by(id,block,miniBlock,trial,resp_type)%>% 
  summarize(rt = mean(rt))

ggplot(rt_df, aes(x = rt,colour = resp_type)) +
  # Map y to the density stat so scales align
  stat_ecdf(geom = "step", linewidth = 0.9)+
  theme_minimal()+
  labs(x = "Reaction Time (s)",
      y = "Cumulative Proportion")+
  scale_color_discrete(
    name = "",                 
    labels = c("Hit", "False Alarm")
  )

rt_df_individual = rt_df%>%
  group_by(id,resp_type)%>%
  summarise(mean_rt = mean(rt))

ggplot(rt_df_individual,aes(x = factor(resp_type),y=mean_rt,fill = resp_type))+
  geom_boxplot(alpha =0.3,outliers = FALSE)+
  geom_point(aes(colour = resp_type),position = position_jitter(width = 0.1))+
  scale_fill_discrete(
    name = "",                 
    labels = c("False Alarm","Hit")
  )+
  scale_colour_discrete(
    name = "",                 
    labels = c("False Alarm","Hit")
  )+
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )+
  labs(y = "Mean Reaction Time per Participant")


accuracy_df = labelled_data%>% 
  group_by(id, dsrdResponse)%>% 
  summarise(correct_reject = mean(resp_type == "correct_reject"),
            hit = mean(resp_type == "hit"),
            miss = mean(resp_type == "miss"),
            fa = mean (resp_type == "fa"))%>%
  group_by(id)%>%summarise(across(.cols = c(correct_reject,hit,miss,fa), .fns = sum))

accuracy_df_pivoted = accuracy_df%>%pivot_longer(cols=c(correct_reject,hit,miss,fa),
                                                 names_to = "resp_type",
                                                 values_to = "percentage")

ggplot(accuracy_df_pivoted%>%filter(resp_type=="hit"|resp_type=="fa"),
       aes(x = factor(resp_type),y=percentage,fill = resp_type))+
  geom_boxplot(alpha =0.3,outliers = FALSE)+
  geom_point(aes(colour = resp_type),position = position_jitter(width = 0.1))+
  scale_fill_discrete(
    name = "",                 
    labels = c("False Alarm","Hit")
  )+
  scale_colour_discrete(
    name = "",                 
    labels = c("False Alarm","Hit")
  )+
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )+
  labs(y = "Percentage")

dprime_df = accuracy_df%>%
  mutate(
    adjusted_hit = case_when(
      hit == 1 ~ 0.99,
      .default = hit
    ),
    adjusted_fa = case_when(
      fa == 0 ~ 0.01,
      .default = fa
    ),
    dprime = qnorm(adjusted_hit)-qnorm(adjusted_fa),
    dummy = factor(1))

ggplot(dprime_df,aes(x=dummy, y=dprime))+
  geom_violin(alpha=0.5)+
  geom_boxplot(width=0.4, outliers = FALSE)+
  geom_point(position = position_jitter(width = 0.15))+
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )+
  labs(y="d`")
