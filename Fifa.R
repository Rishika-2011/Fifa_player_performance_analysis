library(readxl)
file.exists("C:/Users/Admin/OneDrive/Desktop/fifa_player_performance_market_value.xlsx")
fifa<- read_excel("C:/Users/Admin/OneDrive/Desktop/fifa_player_performance_market_value.xlsx")
head(fifa)

# Data Preprocessing- checking missing values,structure, summary, duplicates
sum(is.na(fifa))
summary(fifa)
str(fifa)
sum(duplicated(fifa))
sd(fifa$market_value_million_eur)

# EDA- EXploratory Data Analysis
## 1. Age Distribution
library(ggplot2)

ggplot(fifa,aes(age))+
  geom_histogram(binwidth=2,fill="steelblue")
#Interpretation
####a). Majority of players are between 22–30 years.
####b). Few young and veteran players.

## 2. Position Distribution-- shows which position dominates the dataset
ggplot(fifa,aes(position))+
  geom_bar(fill="blue")

## 3. Market value Distribution-- Shows whether market value are normally distributied or skewed
ggplot(fifa,aes(market_value_million_eur))+
  geom_histogram(fill="steelblue")

## 4. Goal vs Market Value-- Positive slope indicates scoring more goals generally increases market value.
ggplot(fifa,
       aes(goals,
           market_value_million_eur))+
  geom_point(color="red")+
  geom_smooth(method="lm")

## 5. Assists vs Market Value-- Measures the impact of creativity on player valuation.
ggplot(fifa,
       aes(assists,
           market_value_million_eur))+
  geom_point()+
  geom_smooth(method="lm")

## 6. Rating vs Market Value-- usually the strongest positive relationship
ggplot(fifa,
       aes(overall_rating,
           market_value_million_eur))+
  geom_point()+
  geom_smooth(method="lm")

## Correlation Analysis
library(corrplot)
library(dplyr)

numeric_data<-select(fifa,
                     age,
                     overall_rating,
                     potential_rating,
                     goals,
                     assists,
                     minutes_played,
                     market_value_million_eur)

cor_matrix<-cor(numeric_data)

corrplot(cor_matrix,
         method="color")

# INTERPRETATION- High positive correlations indicate variables that increase together, 
##while negative values indicate inverse relationships.

## STATISTICAL ANALYSIS
# 1. Independent t-test--Do Goalkeepers and Strikers have different market values?
library(dplyr)

two_pos<-fifa%>%
  filter(position%in%c("GK","ST"))

t.test(market_value_million_eur~position,
       data=two_pos)
# 2. ANOVA- Do different positions have different market values?
# Ho: goals=assists
#H1: goals<>assists
anova<-aov(
  market_value_million_eur~position,
  data=fifa)
anova
summary(anova)
## Interpretation- Significant p-value indicates at least one position differs.

# REGRESSION- Which Factor influence market value?
model<-lm(
  market_value_million_eur~
    overall_rating+
    potential_rating+
    goals+
    assists+
    age+
    minutes_played,
  data=fifa)

summary(model)
coef_table <- as.data.frame(summary(model)$coefficients)

coef_table$Variable <- rownames(coef_table)

coef_table$Interpretation <- c(
  "Expected market value when all predictors are zero (mathematical constant).",
  "Positive effect, but not statistically significant.",
  "Very small negative effect; not statistically significant.",
  "More goals slightly increase market value, but not statistically significant.",
  "Almost no effect on market value.",
  "Older players tend to have slightly lower market value, but not statistically significant.",
  "Very small positive effect; not statistically significant."
)

coef_table <- coef_table[, c(
  "Variable",
  "Estimate",
  "Pr(>|t|)",
  "Interpretation"
)]

colnames(coef_table) <- c(
  "Variable",
  "Estimate",
  "p-value",
  "Interpretation"
)

View(coef_table)


## Predict Transfer RISK-- BY using Decision tree algorithm 
library(rpart)
library(rpart.plot)

tree <- rpart(
  transfer_risk_level ~ age +
    overall_rating +
    goals +
    assists +
    contract_years_left +
    injury_prone,
  data = fifa,
  method = "class",
  control = rpart.control(
    cp = 0.001,
    minsplit = 10,
    minbucket = 5,
    maxdepth = 6
  )
)

printcp(tree)
rpart.plot(tree, type = 4, extra = 104)


## Clustering
library(cluster)

data_cluster<-scale(select(fifa,
                           overall_rating,
                           goals,
                           assists,
                           market_value_million_eur))

kmeans_result<-kmeans(data_cluster,3)
install.packages("factoextra")
library(factoextra)
fviz_cluster(
  kmeans_result,
  data = data_cluster,
  geom = "point",
  ellipse.type = "convex"
)
##Interpretation After obtaining the cluster centers, we can describe the groups
# Cluster 1: Players with above-average overall rating, more goals, more assists, and higher market value (high-performing players).
#Cluster 2: Players with below-average performance and lower market value (developing or lower-performing players).
#Cluster 3: Players with average performance and moderate market value (mid-level players).

#Overall Business Insights
##The dataset is clean and balanced, making it suitable for analysis.
##Player market value is not strongly explained by the selected performance metrics in this dataset.
##Neither playing position nor the comparison between goalkeepers and strikers shows a statistically significant effect on market value.
##Transfer risk appears to depend on a combination of age, performance, and contract-related factors.
##K-means clustering successfully segments players into high-, medium-, and lower-performance groups despite overlap.

#OVERALL ANALYSIS- The analysis demonstrates that, within this dataset, traditional performance 
## indicators such as goals, assists, overall rating, age, and minutes 
## played do not significantly explain player market value. 
## However, decision tree and clustering analyses reveal useful patterns for 
## transfer risk assessment and player segmentation. 
## These findings suggest that player valuation is likely influenced by additional factors beyond 
## on-field performance, highlighting the importance of incorporating broader financial, contractual, 
## and contextual information into future predictive models.

install.packages("rmarkdown")
install.packages("knitr")
