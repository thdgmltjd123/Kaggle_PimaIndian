library(tidyverse)
library(data.table)
library(gridExtra)
library(ggplot2)
library(RColorBrewer)
library(reshape2)
library(randomForest)
library(caret)

data<-fread("diabetes.csv")

head(data)
summary(data)
#idx 열 추가
data[, idx := seq(1,768)]

scatter_d <- function(df, col1, col2, ...) {
  graph <- ggplot(df, aes(x = get(col1), y = get(col2))) + 
    geom_point(shape = 20, size = 5, aes(colour = factor(get(...)))) + 
    ggtitle(paste("Scatter Plot :",col1,"between",col2)) + 
    labs(colour = paste(...), x = col1, y = col2)
  
  graph + scale_fill_brewer(palette = "Set1")
  return(graph)
}

Boxplot <- function(df, col1) {
  graph <- ggplot(df, aes(x = 1, y = get(col1))) +
    geom_boxplot() +
    ggtitle(paste("Boxplot of",col1)) + 
    labs(x = "value", y = col1)
  
  return(graph)
}

ins_graph_list <- list()
cols <- c("Glucose", "Pregnancies", "BloodPressure",
          "SkinThickness","BMI", "Age")

for (i in 1:length(cols)){
  ins_graph_list[[i]] = scatter_d(data[Insulin!=0 & get(cols[i])!=0], "Insulin", cols[i], "Outcome")
}

do.call("grid.arrange", c(ins_graph_list, ncol = 3, nrow = 2))

sca_graph_list <- list()
cols <- c("Glucose", "Pregnancies", "Insulin", "BloodPressure",
          "SkinThickness")

for (i in 1:length(cols)){
  sca_graph_list[[i]] = scatter_d(data, "idx", cols[i], "Outcome")
}

do.call("grid.arrange", c(sca_graph_list, ncol = 3, nrow = 2))

box_graph_list <- list()
cols <- c("Glucose", "Pregnancies", "Insulin", "BloodPressure",
          "SkinThickness")

for (i in 1:length(cols)){
  box_graph_list[[i]] = Boxplot(data, cols[i])
}

do.call("grid.arrange", c(box_graph_list, ncol = 3, nrow = 2))

#연령별 데이터 추가
data[,AgeGroup := Age %/% 10]

#고령자들 데이터 3개 제거.
data <- data[AgeGroup<7]

# BMI 그룹별로 나누는 변수 추가
data[,BMI_cat := ifelse(BMI < 18.5,"uw",ifelse(BMI<25,"nm",ifelse(BMI<30,"ow","ob")))]

#Glucose 결측치 5개 제거
data<-data[Glucose != 0]
data<-data[SkinThickness != 99]

#상관계수 heatmap 그리기..!!
temp <- data[, c(1,2,3,4,5,6,7,8)]
cormat <- round(cor(temp),2)
cormat

get_lower_tri <- function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)] <- NA
  return(cormat)
}
reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}
upper_tri <- get_upper_tri(cormat)
melted_cormat <- melt(upper_tri,na.rm = TRUE)

heatmap <- function(melted_data){
  graph <- ggplot(data = melted_data, aes(Var2, Var1, fill = value)) +
    geom_tile(color = "white") + 
    scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                         midpoint = 0, limit = c(-1,1), space = "Lab",
                         name = "Pearson\nCorrelation") + 
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) +
    coord_fixed()
  return(graph)
} 

cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
melted_cormat <- melt(upper_tri, na.rm = TRUE)
heatmap(melted_cormat) + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

#train test split
smp_size <- floor(0.7*nrow(data))
set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

train <- data[train_ind, ]
test <- data[-train_ind, ]

#BP 결측치 impute
impute_bp <- function(dat) {
  bp_median <- dat[BloodPressure!=0,median(BloodPressure),AgeGroup][order(AgeGroup)][,"V1"]
  for(i in 2:6){
  dat[BloodPressure == 0 & AgeGroup == i, BloodPressure := bp_median[i-1]]
  }
  return(dat)
}
#BMI 결측치 impute
impute_bmi <- function(dat) {
  bmi_mean <- dat[BMI!=0,mean(BMI),AgeGroup][order(AgeGroup)][,"V1"]
  for(i in 2:6){
    dat[BMI == 0 & AgeGroup == i, BMI := bmi_mean[i-1]]
  }
  return(dat)
}

#Insulin 결측치 impute
impute_ins <- function(dat) {
  ins_median <- dat[Insulin != 0, median(Insulin), by = Outcome][,"V1"]
  for(i in 1:2){
    dat[Insulin == 0 & Outcome == i-1, Insulin:= round(ins_median[i])]
  }
  return(dat)
}

train <- impute_bp(train)
train <- impute_bmi(train)
train <- impute_ins(train)


scatter_d(train[Insulin != 0], "Glucose", "Insulin", "Outcome")
scatter_d(train, "Glucose", "Insulin", "Outcome") + geom_vline(xintercept = 126, col = "green", size = 1)
scatter_d(train[Insulin != 0], "BMI", "Insulin", "Outcome") + geom_vline(xintercept = 30, col = "green", size = 1)

#SkinThickness 결측치 Impute
impute_skin <- function(dat){
  rline <- lm(dat[SkinThickness != 0,SkinThickness]~dat[SkinThickness!=0, BMI])
  dat[SkinThickness == 0 , SkinThickness := round(coef(rline)[1] + coef(rline)[2]*BMI)]
  
  return(dat)
}


scatter_d(train[SkinThickness!=0], "BMI", "SkinThickness", "Outcome") +
  geom_smooth(method = 'lm', formula = y~x, color = "green")

train <- impute_skin(train)

#Impute 후 상관관계 heatmap 그려보기
temp <- train[, c(1,2,3,4,5,6,7,8)]
cormat <- round(cor(temp),2)
cormat

cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
melted_cormat <- melt(upper_tri, na.rm = TRUE)
heatmap(melted_cormat) + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

# 추가변수 도입(D1,D2,D3,D4)
scatter_d(train, "Age","Pregnancies","Outcome") + 
  geom_vline(xintercept = 34.5, col = "green", size = 1) + 
  geom_hline(yintercept = 6.5, col = "green", size = 1)

# 연령별 당뇨병 비율
train[Outcome == 1,.N,by = AgeGroup][order(AgeGroup)][,2] / train[,.N,AgeGroup][order(AgeGroup)][,2]
train[AgeGroup != 8][,.N,AgeGroup][order(AgeGroup)]
train[,D1 := ifelse(AgeGroup %in% c(2,6),0,1)]
# 연령별 BMI 비율
train[Outcome == 1, .N, by = BMI_cat][order(BMI_cat)][,2] / train[,.N,by = BMI_cat][order(BMI_cat)][,2]
train[, .N, by = BMI_cat][order(BMI_cat)]
train[,D2 := ifelse(BMI_cat %in% c("uw","nm"),0,1)]
# 혈당수치 추가변수
train[,D3 := ifelse(Glucose < 126,0,1)]
# 임신 횟수와 나이 고려 추가변수
train[,D4 := ifelse(Age < 35,ifelse(Pregnancies < 7,0,1),1)]
head(train)

setcolorder(train, c(11,1,2,3,4,5,6,7,8,13,14,15,16,9))
train <- train[,c(2:14)]
head(train)

# 추가변수 및 데이터 정리 함수.
add_variables <- function(dat){
  dat[,D1 := ifelse(AgeGroup %in% c(2,6),0,1)]
  dat[,D2 := ifelse(BMI_cat %in% c("uw","nm"),0,1)]
  dat[,D3 := ifelse(Glucose < 126,0,1)]
  dat[,D4 := ifelse(Age < 35,ifelse(Pregnancies < 7,0,1),1)]
  
  setcolorder(dat, c(11,1,2,3,4,5,6,7,8,13,14,15,16,9))
  dat <- dat[,c(2:14)]
  
  return(dat)
}

# whitening
train[,c(1:8) := lapply(.SD,'scale'), .SDcols = c(1:8)]
train[,Outcome:=as.factor(Outcome)]
train

# 테스트 셋에 적용할 파이프라인 만들기
pipeline <- function(dat){
  dat <- impute_bp(dat)
  dat <- impute_bmi(dat)
  dat <- impute_ins(dat)
  dat <- impute_skin(dat)
  dat <- add_variables(dat)
  
  dat[,c(1:8) := lapply(.SD,'scale'), .SDcols = c(1:8)]
  dat[,Outcome := as.factor(Outcome)]
  
  return(dat)
}

test <- pipeline(test)
test

str(train)

# randomForest Model
test_x<-test[,1:12]
test_y <- test[,13]

rf_model1 <- randomForest(Outcome~., train, mtry = 5, importance = TRUE)
print(rf_model1)

y_pred = predict(rf_model1, test_x)
confusionMatrix(y_pred, test_y[,Outcome])
