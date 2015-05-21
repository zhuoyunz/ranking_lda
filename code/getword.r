library(lda)
library(foreach)
# load functions used for ranking documents
source('~/PGM-Project/SHMP/code/tpm_fcn.r')

# read and pre-process corpus for lda modeling
filename_list=list.files(pattern = ".*.txt")
file_list=foreach(file = filename_list)%do%{readLines(file)}
file_word_num=unlist(lapply(file_list, length))
voca_list=do.call(list,lapply(1:length(file_list), function(x)lexicalize(file_list[[x]])$voca))
voca=unlist(voca_list)
documents_list=lexicalize(file_list)$documents

lda.model=lda.collapsed.gibbs.sampler(documents_list,20,voca,5000,0.1,0.1)
dc_sums=lda.model$document_sums
doc_pdf = apply(dc_sums,2,function(x) x/sum(x))
tpc_pdf = apply(dc_sums,1,function(x) x/sum(x))
laplacian = Laplacian_score(doc_pdf,tpc_pdf,1,2)
top_topic = sort(laplacian,decreasing=TRUE,index.return=TRUE)$ix[1]

# re-rank key words
word_pdf = apply(lda.model$topics,1,function(x) x/sum(x+1e-05))
word_pdf = apply(word_pdf,1,function(x) x/sum(x+1e-05))
sort_word_pdf = apply(word_pdf,1,function(x) sort(x,decreasing =TRUE,index.return=TRUE)$ix)
tw_m = foreach(x=top_topic) %do% { colnames(lda.model$topics)[sort_word_pdf[1:200,x]]}

# get the top key words of each top topic
doc_top_word = foreach(i = 1:length(voca_list)) %do% {sapply(tw_m,function(x) x[x %in% voca_list[[i]]])}

foreach(n=seq(1,length(voca_list))) %do%{write.table(unlist(doc_top_word[[n]]),file=paste(n,'.txt'),row.names=FALSE,col.names=FALSE)}
   
