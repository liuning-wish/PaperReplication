require(tidyverse)
Rcpp::sourceCpp('Scripts/Shared/helper.cpp')

# load swow risk associates
# retrieved from https://smallworldofwords.org/de/project/research
swow_risk = read_csv('Data/InputData/swow_risk_associates.csv') %>%
  pull(frequency, word)

# load word2vec vectors
# retrieved from https://fasttext.cc/docs/en/english-vectors.html
word2vec_risk = read.table('Data/InputData/word2vec_risk_vectors.txt')

# get word2vec cosines
word2vec_risk_cos = arma_cosine(word2vec_risk[-1] %>% as.matrix())
rownames(word2vec_risk_cos) = colnames(word2vec_risk_cos) = word2vec_risk[[1]]

# determine similarities
sim_word2vec = word2vec_risk_cos['risk', colnames(word2vec_risk_cos) != "risk"] %>% sort(decreasing = T)
sim_swow = log(swow_risk[names(sim_word2vec)]+1) %>% sort(decreasing = T)

# get top 10
sim_word2vec_top10 = sim_word2vec[1:10]
sim_swow_top10 = sim_swow[1:10]

# get sentiment
sent = read_delim('Data/InputData/sentiment_en.txt', 
                  delim='\t', col_names = F) %>% 
  mutate(X1 = str_replace_all(X1, "#[:alpha:]+", "")) %>% 
  pull(X2, X1)

# define colors
cols = colorRampPalette(viridis::cividis(11)[c(2,10)])(141)

pdf('Output/Results/AuthorNode/0_text.pdf',width=5, height=5)

par(mar=c(0,0,2,0))
plot.new();plot.window(xlim=c(.5, 2.5), ylim=c(.5,10.5))

w = rev(sim_word2vec_top10) ; w = 20* w / sum(w)
s = round(sent[names(sim_word2vec_top10) %>% rev()]*100)
text(1, 1:10, labels = rev(names(sim_word2vec_top10)), cex=w, col = cols[s+71], font=2)

w = rev(sim_word2vec_top10) ; w = 20* w / sum(w)
s = round(sent[names(sim_swow_top10) %>% rev()]*100)
text(2, 1:10, labels = rev(names(sim_swow_top10)), cex=w, col = cols[s+71], font=2)

mtext(c(expression(underline("Text-based")),expression(underline("Association-based"))),at=1:2, font=1, cex=1.5)

dev.off()