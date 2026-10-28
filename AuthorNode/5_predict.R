require(tidyverse)
require(caret)
library(caret)
soep = read_csv('Data/InputData/risk_soep.csv')
assos = read_csv('Data/InputData/risk_associations.csv')
nodes = read_csv('Data/IntermediateData/AuthorNode/author_risk_nodes.csv')


# WORD CORRS

words_10 = assos %>% 
  filter(trial %in% 1:5) %>% 
  group_by(to_correct) %>% 
  summarize(n = n()) %>% 
  filter(n>=10)

s = tapply(assos$trial, assos$subject, function(x) all(1:5 %in% x))

lev1 = assos %>% 
  filter(trial %in% (1:5), 
         subject %in% as.numeric(names(s[s])),
         !duplicated(cbind(subject, trial))) %>% 
  select(subject, to_correct, trial) %>% 
  pivot_wider(names_from = trial, 
              values_from = to_correct)

found = sapply(words_10$to_correct, function(x) {
  x == lev1$`1` |
    x == lev1$`2` |
    x == lev1$`3` |
    x == lev1$`4` |
    x == lev1$`5`
})
colnames(found) = paste0("w_",words_10$to_correct)

corrs = tibble(subject = lev1$subject) %>% 
  bind_cols(as_tibble(found)) %>% 
  left_join(soep %>% 
              mutate(subject = as.numeric(subject)) %>% 
              select(-c(2,3))) %>% 
  pivot_longer(c(General, Driving, Financial, Recreational, Occupational, Health, Social),
               names_to = "soep_var", values_to = "soep_val") %>% 
  pivot_longer(starts_with("w_"),
               names_to = "word", values_to = "present", names_prefix = "w_") %>% 
  group_by(soep_var, word) %>% 
  summarize(cor = cor(soep_val, present)) %>% 
  pivot_wider(names_from = soep_var, values_from = cor) %>% 
  left_join(nodes %>% select(asso, eng, cluster), by = c("word" = "asso")) %>% 
  select(eng, everything()) %>% 
  mutate(mean = (General + Driving + Financial + Recreational + Occupational + Health + Social)/7) %>% 
  arrange(desc(mean))






# GET VECTORS TEXT ---------

# ws = assos %>% filter(trial %in% 1:5) %>% pull(to_correct) %>% unique()
# ws = sapply(ws, str_split, pattern = " ") %>% unlist() %>% unique()
# 
# word_set = tibble(word = rep(ws, 2), 
#                   spelling = c(ws, sapply(ws, Hmisc::capitalize)))
# 
# word_mat = matrix(nrow = nrow(word_set), ncol = 300)
# rownames(word_mat) = word_set$spelling
# 
# con = file("0 Notes & Materials/de", "r")
# a = readLines(con, n = 1)
# 
# while(any(is.na(word_mat[,1]))){
#   
#   lin = readLines(con, n = 1) %>% str_split(" ")
#   w = lin[[1]][1]
#   v = lin[[1]][-1] %>% as.numeric()
#   if(w %in% rownames(word_mat)) {
#     word_mat[w,] = v
#     print(paste0("Found '",w,"'"))
#     } 
#   }
# close(con)
# 
# 
# words_lower = unique(word_set$word)
# word_mat_lower = matrix(nrow = length(words_lower), ncol = 300)
# rownames(word_mat_lower) = words_lower
# 
# for(i in 1:length(words_lower)){
#   word_mat_lower[words_lower[i],] = colMeans(word_mat[rownames(word_mat) == words_lower[i],,drop=F],na.rm=T)
#   }
# saveRDS(word_mat_lower, "Data/IntermediateData/fastText.RDS")
word_mat_lower = readRDS("Data/IntermediateData/fastText.RDS")

# PERSON VECTORS ---

# get responses
risk_assos = assos%>%filter(trial%in%1:5)
risk_assos = split(risk_assos$to_correct, risk_assos$subject)

# get vectors
vs = sapply(risk_assos, function(x){
  x = str_split(x, " ")
  x = sapply(x, function(x) colMeans(word_mat_lower[x[x %in% rownames(word_mat_lower)],,drop=F]))
  rowMeans(x, na.rm=T)
}) %>% t()
colnames(vs) = paste0("v_",1:300)
vs = tibble(subject = rownames(vs)) %>% bind_cols(as_tibble(vs))

# PERSON STATS ---

# get risk similarities
jacc_mat = readRDS('Data/IntermediateData/AuthorNode/risk_similarity_mat.RDS')

# count responses per participant
ns = assos %>% 
  filter(trial %in% 1:5) %>% 
  mutate(subject = as.character(subject)) %>% 
  group_by(subject) %>% 
  summarize(n = n())

# combine stats
words <- nodes %>%
  transmute(
    to_correct = asso,
    cluster = cluster
  )   
stats <- assos %>% 
  filter(trial %in% 1:5) %>% 
  select(subject, to_correct, age) %>% 
  mutate(subject = as.character(subject)) %>% 
  left_join(ns) %>% 
  group_by(subject) %>% 
  summarize(cluster_1 = mean(jacc_mat[to_correct[to_correct %in% rownames(jacc_mat)], words$to_correct[!is.na(words$cluster) & words$cluster == 5]], na.rm=T),
            cluster_2 = mean(jacc_mat[to_correct[to_correct %in% rownames(jacc_mat)], words$to_correct[!is.na(words$cluster) & words$cluster == 2]], na.rm=T),
            cluster_3 = mean(jacc_mat[to_correct[to_correct %in% rownames(jacc_mat)], words$to_correct[!is.na(words$cluster) & words$cluster == 4]], na.rm=T),
            cluster_4 = mean(jacc_mat[to_correct[to_correct %in% rownames(jacc_mat)], words$to_correct[!is.na(words$cluster) & words$cluster == 1]], na.rm=T),
            cluster_5 = mean(jacc_mat[to_correct[to_correct %in% rownames(jacc_mat)], words$to_correct[!is.na(words$cluster) & words$cluster == 3]], na.rm=T),
            age = first(age),
            n = first(n)) %>% 
  left_join(soep %>% mutate(subject = as.character(subject))) %>% 
  left_join(vs)


# Statistics ------

# soep dimensions
crits = c("General", "Driving", "Financial", "Recreational", "Occupational", 
          "Health", "Social")

# run regressions
ws = matrix(nrow=length(crits), ncol=7)
ps = matrix(nrow=length(crits), ncol=7)
test = c()
for(i in 1:length(crits)){
  stats$crit = stats[[crits[i]]]
  d = stats %>% 
    filter(n == 5) %>% 
    select(crit, age, gender, starts_with("cluster_")) %>% 
    mutate(gender = as.numeric(gender == "male")) %>% 
    drop_na() %>% 
    mutate_if(is.numeric, function(x) (x - mean(x))/sd(x))
  m0 = lm(crit ~ age + gender, 
          data = d)
  m1 = lm(crit ~ age + gender + cluster_1 + cluster_2 + cluster_3 + cluster_4 + cluster_5, 
          data = d)
  test[i] = anova(m0, m1)$`Pr(>F)`[2]
  ws[i,] = summary(m1)$coef[,"Estimate"][2:8]
  ps[i,] = summary(m1)$coef[,"Pr(>|t|)"][2:8]
}




# Prediction ------

# define variable sets
vars = list("base" = c("age","gender_01"),
            "clust" = names(stats)[str_detect(names(stats), "cluster_")],
            "all" = c("age","gender_01", names(stats)[str_detect(names(stats), "cluster_")]),
            "sem" = c("age","gender_01", names(stats)[str_detect(names(stats), "cluster_")], 
                      names(stats)[str_detect(names(stats), "v_")]),
            "sem_alone" = c(names(stats)[str_detect(names(stats), "v_")]))

# recode gender
stats$gender_01 = as.numeric(stats$gender == "female")

# set lambda
ls = 10 ** seq(-5, 1.5, .1) 

# run elastic net regression
rsqs_elast = tunes_elast = list()
for(i in 1:length(crits)){
  print(i)
  perf_mat = matrix(nrow=1000,ncol=length(vars))
  tune_mat = matrix(nrow=1,ncol=length(vars))
  for(j in 1:length(vars)){
    d = stats %>% 
      filter(n == 5) %>% 
      mutate(crit = .[[crits[[i]]]]) %>% 
      drop_na() %>% 
      select(crit, all_of(unlist(vars[j]))) %>% 
      mutate_if(is.character, as_factor) %>% 
      mutate_if(is.numeric, function(x) (x-mean(x))/sd(x))
    tr = caret::train(crit ~ .,
                      data = d,
                      method = "glmnet",
                      trControl = trainControl(method = "repeatedcv", repeats = 2, p=.95),
                      tuneGrid = expand.grid(alpha = .5, lambda = ls))
    tune_mat[, j] = tr$bestTune$lambda
    perf_mat[, j] = tr$resample$Rsquared
    
  }
  rsqs_elast[[i]] = perf_mat
  tunes_elast[[i]] = tune_mat
}


# plot
png("Output/Results/authornode/7_predict.png",width=12,height=8,res=300,unit='in')

layout(matrix(c(1:2,3,3),ncol = 2, byrow = T), widths=c(3,2), heights=c(2,1.7))
par(mar=c(3, 6, 2, 0))
plot.new();plot.window(xlim=c(.5, 8), ylim=c(.5, 7.5))

mtext("A", side=3, at = -.84, font=2, cex=1.2, line=.5)

pos_x = c(1:2, (3:7)+.5)
pos_y = 7:1
pos = expand.grid(pos_x, pos_y) %>% mutate(b = c(t(ws)), p = c(t(ps)))
w = .5

cls = viridis::cividis(100)[100:20]
max = max(abs(pos$b))
cols = colorRampPalette(cls)(round(1+max * 100) * 2)
pos$cols = cols[round((pos$b + max)*100)+1]


rect(pos[,1] - w, pos[,2] - w, pos[,1] + w, pos[,2] + w, 
     col = ifelse(pos$p < .05, pos$cols, "grey95"), border=NA)
text(pos[,1], pos[,2], cex=.8, labels=round(pos$b, 2), col=ifelse(pos$p <.05, ifelse(pos$b<0,"black","white"), "grey50"))

mtext(c("Age", "Gender", "Fortune", "Activity","Invest.","Threat","Analysis"),
      side=3, at = pos_x, col = c("black", "black", cs = viridis::viridis(5)[c(3,2,1,5,4)] %>% rev()),
      font = c(1, 1, 2 , 2, 2, 2, 2))
mtext(crits, side=2, las=1, adj=1, at = 7:1)


ms = sapply(rsqs_elast, colMeans, na.rm=T)
ms[is.na(ms)] = 0

conf = function(x) sd(x, na.rm=T)*sqrt(1/(length(x)-1) + .05/.95) #* qt(.975,length(x)-1)
ses=sapply(rsqs_elast, function(x) apply(x, 2, conf))

par(mar=c(3, 0, 2, 2))
plot.new();plot.window(xlim=c(0,.1),ylim=c(.5,7.5))
w = .15

mtext("B", side=3, at = -.001, font=2, cex=1.2, line=.5)


cls= viridis::cividis(5)[4:2] 
cls_d = memnet::cmix(cls, "black", .5)

rect(0,pos_y+w,ifelse(ms[2,]!=0,ms[2,],.0002),pos_y+w*3,border=NA,col = cls[1])
rect(0,pos_y-w,ifelse(ms[1,]!=0,ms[1,],.0002),pos_y+w,border=NA,col = cls[2])
rect(0,pos_y-w*3,ms[3,],pos_y-w,border=NA,col = cls[3])
sapply(pos_y, function(x) lines(ms[2,8-x]+ses[2,8-x] *c(1, -1),c(x, x)+2*w, col = cls_d[1], lwd=1))
sapply(pos_y, function(x) lines(ms[1,8-x]+ses[1,8-x] *c(1, -1),c(x, x), col = cls_d[2], lwd=1))
sapply(pos_y, function(x) lines(ms[3,8-x]+ses[3,8-x] *c(1, -1),c(x, x)-2*w, col = cls_d[3], lwd=1))

mtext(seq(0,.1,.02), side=1, at = seq(0,.1,.02), cex=.8)
mtext(expression(paste(R^2," in cross validation")), side=1, line=1.5)

text(.04, 7+w*2, "Risk components", adj = 0, col = cls[1], cex=.8)
text(.04, 7, "Age + Gender", adj = 0, col = cls[2], cex=.8)
text(.04, 7-w*2, "Risk components + Age + Gender", adj = 0, col = cls[3], cex=.8)


par(mar=c(6, 5, 1, 0))
plot.new();plot.window(xlim=c(.5, 91.5), ylim=c(.5, 7.5))

mtext("C", side=3, at = -8.5, font=2, cex=1.2, line=.5)

pos = expand.grid(y =  7:1, 
                  x = 1:91) %>% 
  mutate(v = c(corrs %>% select(General, Driving, Financial, 
                                Recreational, Occupational, Health, 
                                Social) %>% t()))

cls = viridis::cividis(10)[10:2]
max = max(abs(pos$v))
cols = colorRampPalette(cls)(round(1+max * 100) * 2)
pos$cols = cols[round((pos$v + max)*100)+1]

w = .5
rect(pos$x - w, pos$y - w, pos$x + w, pos$y + w, 
     col = pos$cols, border=NA)
text(pos$x, pos$y, labels=ifelse(abs(pos$v)>-1, ifelse(pos$v <0, "-", "+"),""), 
     col="grey90",font=ifelse(abs(pos$v)>.05,2,1),
     cex=ifelse(abs(pos$v)>.05,.8,.6))

mtext(corrs$eng, side=1, adj = 1, las=3, cex=.7, at = 1:91, line=.1)
mtext(crits, side=2, adj = 1, at = 7:1, line=-1, las=1)

dev.off()



