# libraries ----
install.packages("tidyverse")
install.packages("corrplot")
install.packages("formattable")
install.packages("gmodels")
install.packages("ggpubr")
library(tidyverse)
library(corrplot)
library(formattable)
library(gmodels)
library(ggpubr)
library(RColorBrewer)
setwd("")
getwd()

# data ----

data0 = read.csv("data.csv", sep = "")
data1 = read.csv("data_age_miss.csv", sep = ",")

view(data1)

summary(data1)

str(data1)

performance = readRDS("performance.rds")

view(performance)

data = cbind(performance, data1)

view(data)

str(data)
summary(data) # I noticed some nonsense in age col, some values were in year, those I transformed to age(I did the math
              #   based on the date of the dataset), some were just totaly random numbers, those I deleted, there was only a few
              # also, I noticed a lot of "(nu" in country col, did not know what it meant, I just left it there

which(is.na(data), arr.ind = T)

data = na.omit(data) # I do not know the tradition, but removing 18 obs. in 19ths+row-dataset is statisticaly okay for me 

unique(data$age)


# performance x personality ----

E_O = data %>% select(E1:O10)

cor.test(data$performance, data$C8)

cor_tab = function(x, y, m = NA) { # function for correlation table
  
  cor = c()
  for(i in x) {
    res = cor.test(y, i, method = m)
    cor = c(cor, round(res$estimate, 5), round(res$p.value, 5))
    
  }
  
  tab = matrix(c(cor), ncol = 2, nrow = length(cor)/2, byrow = T)
  colnames(tab) = c("cor w/ perf.", "p-value")
  
  tab = as.data.frame(tab)
  out = tab
  
  return(out)
}

perf_pers = cor_tab(E_O, data$performance, m ="k")    # correlation between every single question and performance, 
                                                      #   I would say it's kind of overkill

row.names(perf_pers) = c(
              # E
              "I am the life of the party.", "I don't talk a lot.", "I feel comfortable around people.", 
              "I keep in the background.", "I start conversations.", "I have little to say.", 
              "I talk to a lot of different people at parties.", "I don't like to draw attention to myself.",
              "I don't mind being the center of attention.", "I am quiet around strangers.",
              # N
              "I get stressed out easily.", "I am relaxed most of the time.", "I worry about things.", 
              "I seldom feel blue.", "I am easily disturbed.", "I get upset easily.", "I change my mood a lot.",
              "I have frequent mood swings.", "I get irritated easily.", "I often feel blue.",
              # A
              "I feel little concern for others.", "I am interested in people.", "I insult people.",
              "I sympathize with others' feelings.", "I am not interested in other people's problems.", 
              "I have a soft heart.", "I am not really interested in others.", "I take time out for others.",
              "I feel others' emotions.", "I make people feel at ease.",
              # C
              "I am always prepared.", "I leave my belongings around.", "I pay attention to details.",
              "I make a mess of things.", "I get chores done right away.", 
              "I often forget to put things back in their proper place.", "I like order.", "I shirk my duties.",
              "I follow a schedule.", "I am exacting in my work.",
              # O
              "I have a rich vocabulary.", "I have difficulty understanding abstract ideas.", "I have a vivid imagination.",
              "I am not interested in abstract ideas.", "I have excellent ideas.", "I do not have a good imagination.",
              "I am quick to understand things.", "I use difficult words.", "I spend time reflecting on things.",
              "I am full of ideas.")
perf_pers
perf_pers = rownames_to_column(perf_pers)
colnames(perf_pers) = c("Questions", "Correlation", "Sig")
formattable(perf_pers, align = c("l", "r", "r"), list(Sig = formatter("span", style = x ~ ifelse(x <= 0.05, "color:red", NA))))

# indices ----

                  # just to make it more readible and make the meaning of the values of the questions clear, 
                  #   cause they go in both directions, I know I am losing some information here and every person can interpret
                  #     the meaning diferently, but I would say it does make a sense

idx = data %>% mutate(extro = (E1 + E3 + E5 + E7 + E9)/5, 
                      intro = (E2 + E4 + E6 + E8 + E10)/5, 
                      relaxed = (N2 + N4)/2, 
                      stressed = (N1 + N3 + N5 + N6 + N7 + N8 + N9 + N10)/8, 
                      empathetic = (A2 + A4 + A6 + A8 + A9 + A10)/6,
                      cold = (A1 + A3 + A5 + A7)/4,
                      systematic = (C1 + C3 + C5 + C7 + C9 + C10)/6,
                      chaotic = (C2 + C4 + C6 + C8)/4,
                      literate = (O1 + O3 + O5 + O7 + O8 + O9 + O10)/7,
                      plain = (O2 + O4 + O6)/3)
summary(idx)

idx_sel = idx %>% select(extro:plain)
index_tab = cor_tab(idx_sel, data$performance, m = "k")
row.names(index_tab) = c("Extrovert ('I feel comfortable around people.', 'I don't mind being the center of attention.', ...)",
                         "Introvert ('I keep in the background.', 'I am quiet around strangers.', ...)",
                         "Relaxed ('I am relaxed most of the time.', 'I seldom feel blue.', ...)",
                         "Stressed ('I worry about things.', 'I am easily disturbed.', ...)",
                         "Empathetic ('I am interested in people.', 'I feel others' emotions.', ...)",
                         "Cold ('I feel little concern for others.', 'I insult people.', ...)",
                         "Systematic ('I am always prepared.', 'I like order.', ...)",
                         "Chaotic ('I leave my belongings around.', 'I make a mess of things.', ...)",
                         "Literate ('I have a rich vocabulary.', 'I have a vivid imagination.', ...)",
                         "Plain ('I have difficulty understanding abstract ideas.', 'I do not have a good imagination.', ...)")

index_tab = rownames_to_column(index_tab)
colnames(index_tab) = c("Index", "Corr. w/ performance", "Sig")
index_tab
formattable(index_tab,  align = c("l", "c", "c"), list(Sig = formatter("span", 
                                                   style = x ~ ifelse(x <= 0.05, style(color = "blue", font.weight = "bold"),
                                                    ifelse(x <= 0.1, style(color = "blue"), NA)))))


                                                # I have not seen a lot of real-data correlations of such a huge datasets,
                                                #   but for me it looks like there is almost none

# corrplot of those indices 

cor_plot = function(x, y) {

  cor_mat = cor(x, y)

  plot = corrplot(cor_mat, method = "color", type = "upper", order = "hclust", addCoef.col = "black", 
                  col=brewer.pal(n=8, name="RdBu"), 
                  tl.col = "black", tl.cex = 1, tl.srt = 45, sig.level = 0.01)

  return(plot)
}

cor_plot(idx_sel, idx_sel)                          

                                                # There is some interesting stuff to find, also it's all stat.significant


# plot of the values of those indices 

pers_point_sel = idx %>% select(performance, extro:plain)
pers_point = pivot_longer(pers_point_sel, names_to = "personality", values_to = "val", -performance)
level_order = c("extro", "intro", "relaxed", "stressed", "empathetic", "cold", "systematic", "chaotic", "literate", "plain")
ggplot(pers_point, aes(x = factor(personality, level = level_order), y = val)) + scale_y_discrete(name = "Value", 
                                                                                                  limits = c(1,2,3,4,5)) +
  scale_x_discrete(name = "Personality index") + geom_jitter(size = 0.8, height = 0.25)


# personality x gender ----


gen = data %>% select(gender) %>% filter(gender != 0)
gen = recode(gen$gen, '1' = "Male", '2' = "Female", '3' = "Other")
gen = table(gen)
gen = as.data.frame(gen)
colnames(gen) = c("Gender", "Freq")
formattable(gen, align = c("l", "c"))

pers_gen = idx %>% select(gender, extro:plain) %>% filter(gender != 0)
pers_gen$gender = recode(pers_gen$gender, '1' = "Male", '2' = "Female", '3' = "Other")

pers_gen = pers_gen %>% group_by(gender) %>% summarise_at(vars(extro:plain), list(mean)) %>% as.data.frame()

pers_gen = column_to_rownames(pers_gen, var = "gender")

pers_gen = t(pers_gen)

pers_gen = as.data.frame(pers_gen)

pers_gen = round(pers_gen, 3)

# That definitely was not the most efficient way how to manage this kind of thing, I know

pers_gen = rownames_to_column(pers_gen, var = "Personality")
colnames(pers_gen) = c("Personality", "Mean_of_Women", "Mean_of_Men", "Mean_of_Others")

formattable(pers_gen, align = c("l", "r", "r", "r"), list(area(col = c(Mean_of_Women, Mean_of_Men, Mean_of_Others)) ~ 
                                                            normalize_bar("lightblue")))


# gender x age x performance ----

gen_perf = data %>% select(gender, performance, age) %>% filter(gender != 0)

gen_perf$gender = recode(gen_perf$gender, '1' = "Male", '2' = "Female", '3' = "Other")

ggplot(gen_perf, aes(x = gender, y = performance)) + geom_jitter(size = 0.85, height = 0.4, width = 0.4) # see how regular it is

gen_perf = gen_perf %>% group_by(gender) %>% summarize(mean_age = mean(age), mean_perf = mean(performance))

gen_perf = as.data.frame(gen_perf)

colnames(gen_perf) = c("Gender", "Mean of Performance", "Mean of Age")
formattable(gen_perf, align = c("l", "c", "c"))


