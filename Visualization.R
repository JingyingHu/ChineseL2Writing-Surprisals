#load necessary packages
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(corrplot)

#----Read the data----
data <- read.csv("ChineseL2Writing.csv")
str(data)
colnames(data)


#set as factor
data$cefl_level <- factor(df$cefl_level, levels = c("A2", "B1", "B2", "C1"))
str(data$cefl_level)


#----Fig 1 boxplot----
p1 <- ggplot(data, aes(x = cefl_level, y = bloom7b, fill = cefl_level)) +
  geom_boxplot(alpha = 0.7) +
  stat_compare_means(
    comparisons = list(c("A2", "B1"), c("B1", "B2"), c("B2", "C1"),
                       c("A2", "B2"), c("B1", "C1"), c("A2", "C1")), 
    method = "wilcox.test", label = "p.signif") +
  labs(title = "Mean Surprisal (Bloom)",x = "CEFL Level", y = NULL) +
  theme_minimal()+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        legend.position = "none", 
        plot.title = element_text(size = 11))  
p1


p2 <- ggplot(df, aes(x = cefl_level, y = llamaChinese7b, fill = cefl_level)) +
  geom_boxplot(alpha = 0.7) +
  stat_compare_means(
    comparisons = list(c("A2", "B1"), c("B1", "B2"), c("B2", "C1"),
                       c("A2", "B2"), c("B1", "C1"), c("A2", "C1")), # 成对比较
    method = "wilcox.test", label = "p.signif") +
  labs(title = "Mean Surprisal (Chinese-LLaMA)",x = "CEFL Level", y = NULL) +
  theme_minimal()+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        legend.position = "none", # 隐藏图例
        plot.title = element_text(size = 11)) 
p2


p3 <- ggplot(df, aes(x = cefl_level, y = taiwanllm7b, fill = cefl_level)) +
  geom_boxplot(alpha = 0.7) +
  stat_compare_means(
    comparisons = list(c("A2", "B1"), c("B1", "B2"), c("B2", "C1"),
                       c("A2", "B2"), c("B1", "C1"), c("A2", "C1")), # 成对比较
    method = "wilcox.test", label = "p.signif") +
  labs(title = "Mean Surprisal (Taiwan-LLM)",x = "CEFL Level", y = NULL) +
  theme_minimal()+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        legend.position = "none", # 
        plot.title = element_text(size = 11))  
p3


#combine them together as one figure
LLM_surprisal = grid.arrange(p1, p2, p3,nrow = 1)
LLM_surprisal


#save the image
ggsave('figure1.png',LLM_surprisal,width=7.7,height=4.75,dpi=600)


#----Fig 2 correlation heatmap----
cordata = data[, c("bloom7b","taiwanllm7b","llamaChinese7b",
                 "char_avg_level","SemD","lex_avg_level",
                 "mean_prep_phrases","mean_verb_phrases","mean_noun_phrases", "mean_coord_phrases",
                 "mean_dep_dist","max_parse_tree_height","gram_avg_levels",
                 "MLS","MLC","MLTU","NCPS","NTPS",
                 "first_pronouns"  )]

# get correlation coefficient matrix
corr_matrix <- cor(cordata, method = "spearman")

# get p-value matrix
testRes = cor.mtest(cordata, conf.level = 0.95)

#replace the label names
new_labels <- c(
  "bloom7b" = "Bloom surprisal",
  "taiwanllm7b" = "Taiwan-LLM  surprisal",
  "llamaChinese7b"= "Chinese-LLaMA surprisal",
  "MLS" = "Mean Length of Sentences",
  "MLC" = "Mean Length of Clauses",
  "MLTU" = "Mean Length of T-Units",
  "NCPS" = "Number of Clauses per Sentence",
  "NTPS" = "Number of T-Units per Sentence",
  "char_avg_level" = "Average Character Levels",
  "SemD" = "Lexical Semantic Diversity",
  "lex_avg_level" = "Average Word Levels",
  "mean_dep_dist" = "Mean Dependency Distance",
  "max_parse_tree_height" = "The Height of the Highest Parse Tree",
  "gram_avg_levels" = "Average Grammatical Levels",
  "first_pronouns" = "First Personal Pronouns per Token",
  "mean_coord_phrases" = "Coordinate Phrases per Simple Clause",
  "mean_noun_phrases" = "Noun Phrases per Simple Clause",
  "mean_prep_phrases" = "Prepositional Phrases per Simple Clause",
  "mean_verb_phrases" = "Verb Phrases per Simple Clause")

# Replace row and column names in the correlation matrix
colnames(corr_matrix) <- new_labels[colnames(corr_matrix)]
rownames(corr_matrix) <- new_labels[rownames(corr_matrix)]

# Replace row and column names in the p-value matrix
rownames(testRes$p) <- rownames(corr_matrix)
colnames(testRes$p) <- colnames(corr_matrix)

png("fig2.png", 
    width = 3000, height = 2500,    
    res = 600)                     

par(font = 2) #bold

corrplot(corr_matrix,
         p.mat = testRes$p,
         tl.pos = "td",
         order = "original",
         addrect = 2,
         type = "upper",
         insig = "blank",
         pch.cex = 0.9,
         pch.col = "grey20",
         tl.cex = 0.7,
         tl.col = "black",
         addCoef.col = "black",
         number.cex = 0.7)

dev.off()







