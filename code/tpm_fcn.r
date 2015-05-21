# this file contains functions used in document ranking
# 1. Laplacian_score(): re-ranking topics by laplacian score
#                    tuning parameters: sgm, T
# 2. CV_score(): re-ranking topics by Weighted Topic Converage and Variation
#                    tuning parameters: lambda1(for converage), lambda2(for variation)
# 3. MI_avg_rank(): give estimated documents ranks  
#                    and corresponding variance; ranks are given by Laplacian_score()
#                    or CV_score
#                    argument sgm, t, lambda1, lambda2 are same as Laplacian_score and CV_score
#                    argument fcn='Laplacian' or 'CV_score'

Laplacian_score=function(doc_pdf,tpc_pdf,sgm,t){
    library(proxy)
    document_eu=dist(doc_pdf,by_rows=FALSE,pairwise=TRUE)
    document_similarity=exp(-document_eu^2/(2*sgm^2))
    T_nearest_document=apply(document_similarity,1,function(x) x=ifelse(x>sort(x)[t],0,x))
    D=diag(rowSums(T_nearest_document))
    L=D-T_nearest_document
    tpc_bar=apply(tpc_pdf,2,function(x) x-sum(t(x)%*%D)/sum(D))
    L_score=apply(cbind(diag(t(tpc_bar)%*%L%*%tpc_bar),diag(t(tpc_bar)%*%D%*%tpc_bar)),1,function(x) x=x[1]/x[2])
    return(L_score)
}

CV_score=function(doc_pdf,word_num,lambda1,lambda2){
    mu_zk=doc_pdf%*%word_num/sum(word_num)
    sigma_zk=(apply(doc_pdf,2,function(x) x-mu_zk))^2%*%word_num/sum(word_num)
    cv_score=apply(cbind(mu_zk,sigma_zk),1,function(x) x=(x[1]^lambda1)*(x[2]^lambda2))
    return(cv_score) 
}

MI_avg_rank=function(fcn,word_num,dc_list,tpc_num,vocab,iter_num,alpha=0.1,beta=0.1,lambda1=0.5,lambda2=0.5,sgm=1,t=3){

    lda.model=lda.collapsed.gibbs.sampler(dc_list,tpc_num,vocab,iter_num,alpha,beta)
    dc_sums=lda.model$document_sums
    dc_dist=apply(dc_sums,2,function(x) x/sum(x))
    tpc_dist=apply(dc_sums,1,function(x) x/sum(x))
    if(fcn=='Laplacian'){
        tpc_score=Laplacian_score(dc_dist,tpc_dist,sgm,t)
    }
    else{
        tpc_score=CV_score(dc_dist,word_num,lambda1,lambda2)
    }
    rank=top.topic.documents(lda.model$document_sums,5)[,sort(tpc_score,decreasing=TRUE,index.return=TRUE)$ix[1:5]]
    wgt_rank=rank%*%seq(5,1)/sum(seq(5,1))
    wgt_rank_var=(sum(seq(5,1)^2)/(sum(seq(5,1))^2))*(rowMeans(apply(rank,2,function(x) (x-rowMeans(rank))^2)))
    return(list(wgt_rank,wgt_rank_var))
}
