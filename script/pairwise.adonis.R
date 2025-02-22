#'@title Pairwise multilevel comparison using adonis
#'
#'@description This is a wrapper function for multilevel pairwise comparison
#' using adonis() from package 'vegan'. The function returns adjusted p-values using p.adjust().
#'
#'@param x Data frame (the community table), or "dist" object (user-supplied distance matrix).
#'
#'@param factors Vector (a column or vector with the levels to be compared pairwise).
#'
#'@param sim.function Function used to calculate the similarity matrix,
#' one of 'daisy' or 'vegdist' default is 'vegdist'. Ignored if x is a distance matrix.
#'
#'@param sim.method Similarity method from daisy or vegdist, default is 'bray'. Ignored if x is a distance matrix.
#'
#'@param p.adjust.m The p.value correction method, one of the methods supported by p.adjust(),
#' default is 'bonferroni'.
#'
#'@param reduce String. Restrict comparison to pairs including these factors. If more than one factor, separate by pipes like  reduce = 'setosa|versicolor'
#'
#'@return Table with the pairwise factors, Df, SumsOfSqs, F-values, R^2, p.value and adjusted p.value.
#'
#'@author Pedro Martinez Arbizu & Sylvain Monteux
#'
#'@examples
#' data(iris)
#' pairwise.adonis(iris[,1:4],iris$Species)
#'
#'
#'#similarity euclidean from vegdist and holm correction
#' pairwise.adonis(x=iris[,1:4],factors=iris$Species,sim.function='vegdist',
#' sim.method='euclidian',p.adjust.m='holm')
#' 
#'#identical example using a distance matrix as an input
#' dist_matrix=vegdist(iris[,1:4],method="euclidean")
#' pairwise.adonis(dist_matrix,factors=iris$Species,
#' p.adjust.m='holm')
#'
#'#similarity manhattan from daisy and bonferroni correction
#' pairwise.adonis(x=iris[,1:4],factors=iris$Species,sim.function='daisy',
#' sim.method='manhattan',p.adjust.m='bonferroni')
#' 
#'#Restrict comparison to only some factors
#'pairwise.adonis(iris[,1:4],iris$Species, reduce='setosa')
#'
#'#for more than one factor separate by pipes
#'pairwise.adonis(iris[,1:4],iris$Species, reduce='setosa|versicolor')
#' 
#'
#'@export pairwise.adonis
#'@importFrom stats p.adjust
#'@importFrom utils combn
#'@importFrom vegan adonis vegdist
#'@importFrom cluster daisy



pairwise.adonis <- function(x,factors, sim.function = 'vegdist', sim.method = 'bray', p.adjust.m ='bonferroni',reduce=NULL,perm=999)
{
  
  co <- combn(unique(as.character(factors)),2)
  pairs <- c()
  Df <- c()
  SumsOfSqs <- c()
  F.Model <- c()
  R2 <- c()
  p.value <- c()
  
  
  for(elem in 1:ncol(co)){
    if(inherits(x, 'dist')){
      x1=as.matrix(x)[factors %in% c(as.character(co[1,elem]),as.character(co[2,elem])),
                      factors %in% c(as.character(co[1,elem]),as.character(co[2,elem]))]
      }
  
    else  (
      if (sim.function == 'daisy'){
            x1 = daisy(x[factors %in% c(co[1,elem],co[2,elem]),],metric=sim.method)
        } 
      else{x1 = vegdist(x[factors %in% c(co[1,elem],co[2,elem]),],method=sim.method)}
    )
    
    x2 = data.frame(Fac = factors[factors %in% c(co[1,elem],co[2,elem])])
    
    ad <- adonis2(x1 ~ Fac, data = x2,
                 permutations = perm);
    pairs <- c(pairs,paste(co[1,elem],'vs',co[2,elem]));
    Df <- c(Df,ad$Df[1])
	SumsOfSqs <- c(SumsOfSqs,ad$SumOfSqs[1])
	F.Model <- c(F.Model,ad$F[1]);
    R2 <- c(R2,ad$R2[1]);
    p.value <- c(p.value,ad$`Pr(>F)`[1])
  }
  p.adjusted <- p.adjust(p.value,method=p.adjust.m)
  
  sig = c(rep('',length(p.adjusted)))
  sig[p.adjusted <= 0.05] <-'.'
  sig[p.adjusted <= 0.01] <-'*'
  sig[p.adjusted <= 0.001] <-'**'
  sig[p.adjusted <= 0.0001] <-'***'
  pairw.res <- data.frame(pairs,Df,SumsOfSqs,F.Model,R2,p.value,p.adjusted,sig)
  
  if(!is.null(reduce)){
    pairw.res <- subset (pairw.res, grepl(reduce,pairs))
    pairw.res$p.adjusted <- p.adjust(pairw.res$p.value,method=p.adjust.m)
    
    sig = c(rep('',length(pairw.res$p.adjusted)))
 	sig[pairw.res$p.adjusted <= 0.1] <-'.'
	sig[pairw.res$p.adjusted <= 0.05] <-'*'
	sig[pairw.res$p.adjusted <= 0.01] <-'**'
	sig[pairw.res$p.adjusted <= 0.001] <-'***'
    pairw.res <- data.frame(pairw.res[,1:7],sig)
  }
  class(pairw.res) <- c("pwadonis", "data.frame")
  return(pairw.res)
} 


### Method summary
summary.pwadonis = function(object, ...) {
  cat("Result of pairwise.adonis:\n")
  cat("\n")
  print(object, ...)
  cat("\n")
  cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
}
## end of method summary


