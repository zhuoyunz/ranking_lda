# function to get precision and recall for ranked results
prec_rec = function(rank,label){
	p_n = sum(label)
	p_r = foreach(i=seq(1:length(rank))) %do% {apply(matrix(c(i,p_n),1,2),2,function(x) sum(label[1:i])/x)}
	p_r = do.call('rbind',lapply(p_r,function(X) X))
	fpr=  unlist(foreach(i=seq(1:(length(rank)-1))) %do% {1-(length(rank[(i+1):length(rank)])-sum(label[(i+1):length(rank)]))/(length(rank)-p_n)})
	fpr=c(fpr,1)
	return(cbind(p_r,fpr))
}

plot_prec_rec = function(dat){
	l=foreach(k=seq(nrow(dat),1)) %do% {dat[k,1]=max(dat[k:nrow(dat),1])}
    prec_rev=rev(unlist(l))
    plot(dat[,2],prec_rev,type='l',lwd=1.5,xlab='Recall',ylab='Precision')
}

plot_fpr_rec = function(fpr,rec){
	# l=unlist(foreach(k=seq(1,length(fpr))) %do% {fpr[k]=max(fpr[1:k])})
	plot(fpr,rec,type='l',lwd=1,xlab='False Positive Rate',ylab='Recall')
}



