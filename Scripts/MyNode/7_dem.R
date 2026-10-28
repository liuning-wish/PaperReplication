require(tidyverse)

# load data
assos = read_csv('Data/InputData/risk_associations.csv')
nodes = read_csv('Data/IntermediateData/Mynode/my_risk_nodes.csv')
risk_asso = assos %>% filter(from_correct == 'risiko')

# Define colors and labels
cols = memnet:::cmix(viridis::viridis(5)[c(3,2,1,5,4)], "black", c(0,0,0,.11,0))
labs = c('Threat','Investment','Analysis','Activity','Fortune')

# extract node information
clust = nodes %>% pull(cluster, asso) 

# determine average values across cluster and age
groups = c("high school (09)","high school (10)",
           "high school (13)", "university degree")
freq_mat_edu = matrix(0, 5, 4)
for(i in 1:4){
  tmp = risk_asso %>% 
    dplyr::filter(education_level == groups[i],
                  to_correct %in% nodes$asso) %>% 
    left_join(nodes, by = c('to_correct' = "asso"))
  freq_tab = tapply(tmp$to_correct, clust[tmp$to_correct], length)
  freq_mat_edu[,i] = freq_tab / sum(freq_tab)
}

groups = c("high school (09)","high school (10)",
           "high school (13)", "university degree")
freq_mat_edu = matrix(0, 5, 4)
for(i in 1:4){
  tmp = risk_asso %>% 
    dplyr::filter(education_level == groups[i],
                  to_correct %in% nodes$asso) %>% 
    left_join(nodes, by = c('to_correct' = "asso"))
  freq_tab = tapply(tmp$to_correct, clust[tmp$to_correct], length)
  freq_mat_edu[,i] = freq_tab / sum(freq_tab)
}


# get frequent words
words = assos %>% 
  dplyr::filter(trial %in% 1:5) %>% 
  group_by(to_correct) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n)) %>% 
  dplyr::filter(n >= 10) %>% 
  dplyr::pull(to_correct)

# get frequent risk assos
top_asso_risk = assos %>% 
  dplyr::filter(trial %in% 1:5,
                to_correct %in% words)

# word frequency by gender
top_asso_gender = top_asso_risk %>% 
  dplyr::filter(gender %in% c("male","female")) %>% 
  mutate(to_correct = as_factor(to_correct)) %>% 
  group_by(gender, to_correct) %>% 
  summarize(n = n()) %>% 
  group_by(gender) %>% 
  mutate(n = n / sum(n)) %>% 
  pivot_wider(names_from = gender, values_from = n)

# word frequency by age
top_asso_age = top_asso_risk %>% 
  dplyr::filter(age_group %in% unique(assos$age_group)) %>% 
  mutate(to_correct = as_factor(to_correct)) %>% 
  group_by(age_group, to_correct) %>% 
  summarize(n = n()) %>% 
  group_by(age_group) %>% 
  mutate(n = n / sum(n)) %>% 
  pivot_wider(names_from = age_group, values_from = n) %>% 
  mutate_at(-1,function(x) {x[is.na(x)] = 0; x})

# word frequency by education
top_asso_edu = top_asso_risk %>% 
  dplyr::filter(education_level %in% c("high school (09)","high school (10)",
                                       "high school (13)", "university degree")) %>% 
  mutate(to_correct = as_factor(to_correct)) %>% 
  group_by(education_level, to_correct) %>% 
  summarize(n = n()) %>% 
  group_by(education_level) %>% 
  mutate(n = n / sum(n)) %>% 
  pivot_wider(names_from = education_level, values_from = n) %>% 
  mutate_at(-1,function(x) {x[is.na(x)] = 0; x})

# word frequency by relationship status
top_asso_status = top_asso_risk %>% 
  dplyr::filter(marital_status %in% c("single","in relationship","married")) %>% 
  mutate(to_correct = as_factor(to_correct)) %>% 
  group_by(marital_status, to_correct) %>% 
  summarize(n = n()) %>% 
  group_by(marital_status) %>% 
  mutate(n = n / sum(n)) %>% 
  pivot_wider(names_from = marital_status, values_from = n) %>% 
  mutate_at(-1,function(x) {x[is.na(x)] = 0; x})

# word frequency by occputation status
top_asso_occu = top_asso_risk %>% 
  dplyr::filter(occupational_status %in% c("in education","paid job","pension")) %>% 
  mutate(to_correct = as_factor(to_correct)) %>% 
  group_by(occupational_status, to_correct) %>% 
  summarize(n = n()) %>% 
  group_by(occupational_status) %>% 
  mutate(n = n / sum(n)) %>% 
  pivot_wider(names_from = occupational_status, values_from = n) %>% 
  mutate_at(-1,function(x) {x[is.na(x)] = 0; x})

# combine
top_asso = top_asso_gender %>% 
  left_join(top_asso_age) %>% 
  left_join(top_asso_edu) %>% 
  left_join(top_asso_status) %>% 
  left_join(top_asso_occu) %>% 
  left_join(nodes %>% 
              dplyr::select(asso, frequency_wave_1, cluster, eng),
            by = c("to_correct" = "asso")) %>% 
  mutate(cluster = factor(cluster, levels = c(1, 5, 4, 2, 3))) %>% 
  arrange(cluster, desc(frequency_wave_1))


# risk associations
asso_risk = assos %>% 
  dplyr::filter(trial %in% 1:5)

# clusters by gender
clust_gender = asso_risk %>% 
  dplyr::mutate(gender = factor(gender, levels = c("male","female"))) %>% 
  left_join(nodes %>% dplyr::select(asso, cluster), by = c("to_correct" = "asso")) %>%
  dplyr::filter(cluster %in% 1:5) %>% 
  group_by(gender, cluster) %>% 
  summarize(n = n()) %>% 
  group_by(gender) %>% 
  mutate(n = n / sum(n)) %>% 
  pivot_wider(names_from = gender, values_from = n)

# clusters by age
clust_age = asso_risk %>% 
  dplyr::filter(age_group %in% unique(assos$age_group)) %>% 
  left_join(nodes %>% dplyr::select(asso, cluster), by = c("to_correct" = "asso")) %>%
  dplyr::filter(cluster %in% 1:5) %>% 
  group_by(age_group, cluster) %>% 
  summarize(n = n()) %>% 
  group_by(age_group) %>% 
  mutate(n = n / sum(n)) %>% 
  pivot_wider(names_from = age_group, values_from = n)

# clusters by education
clust_edu = asso_risk %>% 
  dplyr::filter(education_level %in% c("high school (09)","high school (10)",
                                       "high school (13)", "university degree")) %>% 
  left_join(nodes %>% dplyr::select(asso, cluster), by = c("to_correct" = "asso")) %>%
  dplyr::filter(cluster %in% 1:5) %>% 
  group_by(education_level, cluster) %>% 
  summarize(n = n()) %>% 
  group_by(education_level) %>% 
  mutate(n = n / sum(n)) %>% 
  pivot_wider(names_from = education_level, values_from = n)

# clusters by marital status
clust_status = asso_risk %>% 
  dplyr::filter(marital_status %in% c("single","in relationship","married")) %>% 
  left_join(nodes %>% dplyr::select(asso, cluster), by = c("to_correct" = "asso")) %>%
  dplyr::filter(cluster %in% 1:5) %>% 
  group_by(marital_status, cluster) %>% 
  summarize(n = n()) %>% 
  group_by(marital_status) %>% 
  mutate(n = n / sum(n)) %>% 
  pivot_wider(names_from = marital_status, values_from = n) 

# clusters by occupation status
clust_occu = asso_risk %>% 
  dplyr::filter(occupational_status %in% c("in education","paid job","pension")) %>% 
  left_join(nodes %>% dplyr::select(asso, cluster), by = c("to_correct" = "asso")) %>%
  dplyr::filter(cluster %in% 1:5) %>% 
  group_by(occupational_status, cluster) %>% 
  summarize(n = n()) %>% 
  group_by(occupational_status) %>% 
  mutate(n = n / sum(n)) %>% 
  pivot_wider(names_from = occupational_status, values_from = n) 

# combine
clust_asso = clust_gender %>% 
  left_join(clust_age) %>% 
  left_join(clust_edu) %>% 
  left_join(clust_status) %>% 
  left_join(clust_occu) %>% 
  mutate(cluster = factor(cluster, levels = c(1, 5, 4, 2, 3))) %>% 
  arrange(cluster) 
#pivot_longer(-1, names_to = "var") %>% 
#pivot_wider(names_from = cluster, values_from = value)



png('Output/Results/Mynode/A3_demographics.png',width=8,height=10,res=300,unit='in')

# set colors
cols = memnet:::cmix(viridis::viridis(5)[c(3,4,5,2,1)], "black", c(0,0,.11,0,0))


# set layout
layout(matrix(1:2, nrow=2), height=rev(c(.9,.17)))

# plot seetings
ys = (91:1)
labs = c('Threat',"Fortune",'Activity','Analysis','Investment')

par(mar=c(0,4,0,0))
plot.new();plot.window(xlim=c(0,5), ylim = c(.5,12.5))

sapply(1:5, function(x) lines(c(0,5),c(x, x),col="grey95",lwd=.5))

mtext(labs, side=2, adj=1, at = 5:1, las=1, cex=.8, col = cols)

vars = list(as.character(1:6),
            c("male","female"),
            c("high school (09)","high school (10)","high school (13)","university degree"),
            c("single","in relationship","married"),
            c("in education","paid job","pension"))

labs = list(c("Age 18-27","Age 28-37","Age 38-47","Age 48-57","Age 58-67","Age 68+"),
            c("Male","Female"),
            c("High school (9 years)","High school (10 years)","High school (13 years)","University degree"),
            c("Single","Relationship","Married"),
            c("In education","Employed","Pensioned"))

for(i in 1:length(vars)){
  
  v = vars[[i]]
  cls = viridis::cividis(length(v))
  
  mtext(seq(0,.4,.1),at=seq(0,.4,.1)*2+(i-1),side=1,cex=.6,line=0)
  sapply(seq(0,.4,.1)*2+(i-1), function(x) lines(c(x, x), c(.8, 5.2),col="grey95",lwd=.5))
  lines(c(i-1, i-1), c(.8,5.2), col = "grey75", lwd=.5)
  
  
  l = labs[[i]]
  cls = viridis::cividis(length(v))
  
  ypos = (5+2):((5+1)+length(v))
  xpos = (i-1)+.1
  yw = .1
  xw = .1
  rect(xpos-xw, ypos-yw,xpos+xw, ypos+yw, border=NA, col=cls)
  text(xpos + xw + .05, ypos, labels = l, cex=.5, adj=0)
  
  for(j in 1:length(v)){
    
    points(clust_asso[[v[j]]]*2 + (i-1), 5:1, col = cls[j], pch=15, cex=1)
    
  }
  
}


par(mar=c(1,4,0,0))
plot.new();plot.window(xlim=c(0,5), ylim = c(0,max(ys)+1))

sapply(ys, function(x) lines(c(0,5),c(x, x),col="grey95",lwd=.5))

mtext(top_asso$eng, side=2, at = ys, las=1, col = cols[top_asso$cluster], cex=.6)

for(i in 1:length(vars)){
  
  v = vars[[i]]
  cls = viridis::cividis(length(v))
  
  mtext(seq(0,.15,.05),at=seq(0,.15,.05)*5+(i-1),side=1,cex=.6,line=-2)
  sapply(seq(0,.15,.05)*5+(i-1), function(x) lines(c(x, x), c(1, max(ys)),col="grey95",lwd=.5))
  lines(c(i-1, i-1), c(1,max(ys)), col = "grey75", lwd=.5)
  
  
  for(j in 1:length(v)){
    
    for(k in levels(top_asso$cluster)){
      pos = top_asso$cluster == k
      lines(top_asso[[v[j]]][pos]*5 + (i-1), ys[pos], col = cls[j])
    }
    
  }
  
}

mtext("Relative frequency", at = 2.5, side=1, line=-.5)

dev.off()