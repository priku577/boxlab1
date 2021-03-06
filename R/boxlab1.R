#' @title boxlab1: euclidean and dijkstra algorithm
#' 
#' @description conduct euclidean and dikstra algorithm for given input
#' 
#' @author Maintainer: Boxi Zhang bossyzhang@gmail.com
#' 
#' @references \url{https://en.wikipedia.org/wiki/Euclidean_algorithm}
#' \url{https://en.wikipedia.org/wiki/Dijkstra\%27s_algorithm}
#' 
#' @docType package
#' @name boxlab1
NULL






#' @title Euclidian algorithm.
#' @description This function uses dijkstra algorithm to find the shortest connection.
#' @references from https://en.wikipedia.org/wiki/Euclidean_algorithm .
#' @param x the first number.
#' @param y the second number.
#' @export
#' @examples
#' euclidean(x=100, y=1000)
euclidean <- function(x, y)
{if(!is.numeric(x) || !is.numeric(y) || !(x%%1==0) || !(y%%1==0)){stop()}else{
  num1 <- x;
  num2 <- y;
  while (num2 != 0) {
    rem      = num1%%num2; 
    num1    = num2;       
    num2    = rem;}
  return(num1)
  }
}



#' @title dijkstra algorithm.
#' @description This function uses dijkstra algorithm to find the shortest connection.
#' @references adapted from https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm .
#' @param cost the data frame.
#' @param v the start point.
#' @export
dijkstra<-function(cost, v)
{
  if(!is.data.frame(cost) || !all(colnames(cost) %in% c("v1","v2","w"))){stop()}else{
    n=dist=length(table(cost[,"v1"]))
    prev<-numeric(n)
    flag<-numeric(n)
    dist<-numeric(n)
    new_cost<-matrix(nrow=n,ncol=n)
    for (i in 1:length(cost[,"v1"])){new_cost[cost[,"v1"][i],cost[,"v2"][i]]=cost[,"w"][i]}
    bi=sort(cost[,"w"])
    big=bi[length(bi)]
    new_cost[is.na(new_cost)]=big*10
    
    
    for(i in 1:n)
      prev[i] = -1
    
    for(i in 1:n)
      dist[i]<-new_cost[v,i]
    
    count=2
    while(count <= n)
    {
      min=big+1
      for(w in 1:n)
      {
        if(dist[w] < min && !flag[w])
        {
          min=dist[w]
          u=w
        }
      }
      flag[u]=1
      count<-count+1
      for(w in 1:n)
      {
        if((dist[u]+new_cost[u,w] < dist[w]) && !flag[w])
        {
          dist[w]=dist[u]+new_cost[u,w]
          prev[w]=u
        }
      }
    }
    k<-c()
    for(i in 1:n){
      if(i == v){
        k[i]=0} 
      else if (i != v){
        k[i]=dist[i]
      }
    } 
    return(k)
  }
}





#' @title wiki_graph
#' @description wiki_graph
#' \itemize{
#'   \item v1 numeric vector
#'   \item v2 numeric vector
#'   \item w numeric vector
#' }
#' @usage data("wiki_graph")
#' @references \url{https://en.wikipedia.org/wiki/Graph}
#' @examples
#' data(grav)
wiki_graph <-
data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
save(wiki_graph,file='wiki_graph.RData')
