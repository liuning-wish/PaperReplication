   require(tidyverse)
require(igraph)
require(parallel)

Rcpp::sourceCpp('Scripts/Shared/helper.cpp')
source('Scripts/Shared/helper.R')
   
# read data
asso = read_csv('Data/InputData/risk_associations.csv')
subs = asso$subject %>% unique() %>% as.character()
asso_list = asso %>% split(asso$subject)

# --------------------------------------------------------------------
#     CLUSTER STABILITY
# --------------------------------------------------------------------


# reduce to words that occur more than twice
from_tab = table(asso$from_correct)
asso = asso %>%
  filter(from_correct %in% names(from_tab[from_tab > 10]))


# define fun
cl_boot = function(ind, n_boot = 2){
  
  # container
  node_list = list()

  # iterations
  for(i_boot in 1:n_boot){

    # sample
    asso_split = split(asso, asso$from_correct)
    boot = lapply(asso_split, function(x) x[sample(nrow(x),replace=T),])
    asso = do.call(rbind, boot)

    # get all cues and responses
    cues = unique(asso$from_correct)
    resp = unique(asso$to_correct)

    # create association matrix
    asso_mat = matrix(0, nrow=length(cues), ncol = length(resp))
    rownames(asso_mat) = cues
    colnames(asso_mat) = resp

    # fill matrix
    for(i in 1:length(cues)){

      # keep track
      #if(i %% 100 == 0) print(i)

      # fill matrix
      tab = table(asso[asso$from_correct == cues[i], "to_correct"])
      asso_mat[i, names(tab)] = as.numeric(tab)

    }

    # ---- compute weight between cues

    # get logarithm
    log_asso_mat = log(asso_mat + 1)

    # determine jaccard matrix
    jacc_mat = matrix(nrow = length(cues), ncol = length(cues))
    rownames(jacc_mat) = colnames(jacc_mat) = cues
    for(i in 1:(length(cues)-1)){
      #print(i)
      for(j in (i+1):length(cues)){
        jacc = jaccard_cpp(log_asso_mat[i,], log_asso_mat[j,])
        jacc_mat[i,j] = jacc_mat[j,i] = jacc
      }
    }

    # get co-occurrence and jaccard mat
    edg_list = to_edge_list(jacc_mat)

    # create graph
    edges = cbind(cues[edg_list[,1]], cues[edg_list[,2]])
    net = graph_from_edgelist(edges, directed = F)
    net = set_edge_attr(net, "weight", value = edg_list[,3])

    # cluster
    nodes = tibble('asso' = cues, 'cluster' = cluster_louvain(net)$membership)

    if(i_boot == 1) {
      nodes_tbl = nodes
    } else {
      nodes_tbl = nodes_tbl %>% full_join(nodes, by = c("asso"))
    }

  }

  # out
  names(nodes_tbl)[-1] = paste0('clustering_',ind,'_', 1:n_boot)
  nodes_tbl
}


# Bootstrap 串行版本（windows兼容，无需parallel并行）
# 原作者采用fork并行，Windows不支持fork；本方案计算逻辑完全一致
n_rep <- 10
nodes_tbl_result <- vector("list", n_rep)
set.seed(100) # 和原脚本iseed保持一致
for(r in 1:n_rep){
  cat("正在运行bootstrap第 ", r, "/", n_rep, "\n")
  nodes_tbl_result[[r]] <- cl_boot(ind = r, n_boot = 100)
}

 write
 saveRDS(nodes_tbl_result,
        'Data/IntermediateData/Mynode/component_stab.RDS')

nodes_tbl_result = readRDS('Data/IntermediateData/Mynode/component_stab.RDS')

# combine
nodes_tbl = nodes_tbl_result[[1]]
for(i in 2:length(nodes_tbl_result)){
  nodes_tbl = nodes_tbl %>% full_join(nodes_tbl_result[[i]], by = c("asso"))
}

# n clusters ----

tabs = apply(nodes_tbl[,-1],2, 
             function(x) {
               t = table(x)
               t = sort(t,decreasing = T)
               c(t, rep(NA, 205-length(t)))/sum(t)}) %>% t()
colMeans(tabs,na.rm=T)[1:20] %>% abs() %>% plot()

# determine whether words end up in same cluster
same_cl = matrix(0, nrow = 307, ncol = 307)
rownames(same_cl) = colnames(same_cl) = nodes_tbl$asso
for(i in 1:(nrow(nodes_tbl)-1)){
  print(i)
  for(j in (i + 1):nrow(nodes_tbl)){
    same_cl[i,j] = same_cl[j,i] = mean(nodes_tbl[i,-1] %>% unlist() == nodes_tbl[j,-1] %>% unlist(), na.rm=T)
  }
}


# pairwise overlap = 1-instab
nodes = read_csv('Data/IntermediateData/Mynode/my_risk_nodes.csv ')
cls = nodes$cluster
names(cls) = nodes$asso
pgs = nodes$page_rank_cluster
names(pgs) = nodes$asso
pgs = sapply(split(pgs, cls), function(x) x / max(x)) %>% unlist()
names(pgs) = str_sub(names(pgs),3,nchar(names(pgs)))
engs = nodes$eng
names(engs) = nodes$asso

same_cls = matrix(nrow=307,ncol=5)
rownames(same_cls) = rownames(same_cl)
for(i in 1:nrow(same_cl)){
    v = same_cl[i,-i]
    same_cls[i, ] = tapply(v, cls[names(v)], mean)[as.character(1:5)]
    same_cls[i, ] = same_cls[i, ] / sum(same_cls[i, ])
  }

same_cls_list = split(same_cls %>% as.data.frame, cls[rownames(same_cls)])
same_cls_ord = lapply(1:5, function(x) {a = same_cls_list[[x]]; a[order(-a[[x]]),]}) %>% do.call(what = rbind)

pred = apply(same_cls_ord, 1, which.max)
emp = cls[rownames(same_cls_ord)]
sum(pred == emp)
tapply(pred == emp, emp, sum)
tapply(pred, emp, length)

confusion = tapply(pred, emp, function(x) table(x)[as.character(1:5)]) %>% do.call(what = rbind)
confusion[is.na(confusion)] = 0 
colnames(confusion) = 1:5
saveRDS(confusion, 'Data/IntermediateData/Mynode/stability_confusion.RDS   ')

ord = c(5, 4, 2, 1, 3)
confusion[ord, ord]
 
# plot stability ----

colTrans = function(col,perc) {
  rgb(t(grDevices::col2rgb(col)), maxColorValue=255, alpha = perc*255)
  }



labs = c('Threat','Investment','Analysis','Activity','Fortune')
cols = memnet:::cmix(viridis::viridis(5)[c(3,2,1,5,4)], "black", c(0,0,0,0,.05))
w_cols = cols[pred]
names(w_cols) = rownames(same_cls_ord)

png('Output/Results/Mynode/A1_stability.png',res=300,width=8,height=11,unit='in')

pos = 1:307 + (cls[rownames(same_cls_ord)]-1) * 3   

par(mar=c(1.5,14.5,1,1))
plot.new();plot.window(xlim=c(0,5),ylim=c(max(pos)+.5,.5))
sapply(seq(0,5,.1),function(x) lines(c(x, x), c(.5,max(pos)+.5), lwd=.5, col='grey75', lty=3))
mtext(engs[rownames(same_cls_ord)],side=2,las=1,cex=(pgs[rownames(same_cls_ord)]**.4)-.2,at=pos,line=c(0,5,10)-.5,col=w_cols)
for(i in 1:307){
  ps = same_cls_ord[i, ] %>% unlist() 
  for(j in 0:4){
    rect(j,pos[i]+.5,j+ps[j+1],pos[i]-.5,col=colTrans(cols[j+1],ifelse((j+1)==pred[i],.75,.5)),border=NA)
    }
  }
mtext(c(seq(0,.5,.5) %>% rep(5),1),at=seq(0,5,.5),side=1,cex=.8,line=-1.4)
mtext("Probability to share component with member from component",side=1,line=-.2,cex=1)
mtext(labs, at=0:4, side=3, adj=0, col = cols, font=1, line=-1.4)

dev.off()




