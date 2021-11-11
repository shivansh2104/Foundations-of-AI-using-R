#R version 4.1.0 
#RStudio version 1.4.1717

rm(list = ls())
#to ensure a clean environment before executing the code

dfs <-function(graph,start) {
# function dfs with arguments graph and start
# graph is an adjacency-matrix-representation of the graph where (x,y) is TRUE if the the there is an edge between nodes x and y
# start the node to start from.
# returns an array containing te path from the given start node till it traverses every node in the graph
  
  
  #using a stack to manage the nodes that have yet to be visited, intialized with the start node
  stack = c(start)
  
  #array path to store the path
  path=c()
  
  # A boolean array indicating whether we have already visited a node
  # the start node is already visited
  visited = rep(FALSE, nrow(graph))
  visited[start] = TRUE
  
  
  # while there are nodes yet to visit
  while(length(stack) > 0){
    
    # if there is only one element in the stack then that element is set to be the node and the stack is emptied
    if(length(stack)==1){
      node=stack[1]
      stack=c()
    }
    
    # if there are more than one elements then the last element in the stack is set to be the node and that element is removed from the stack
    else{
      node=tail(stack,-(length(stack)-1))
      stack=head(stack,-1)
    }
    
    
    # the node is added to the path
    # then we check all the neighbouring elements of the node which are yet to be visited and add them to the stack
    path = c(path,node)
    for(i in seq_along(graph[node,])) {
      if(graph[node,i] && !visited[i]){
        visited[i] = TRUE
        stack=c(stack,i)
      }
    }
  }
  # return path
  return(path)
}

#driver code

test_case_1 <- matrix(c(FALSE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE), nrow = 7,byrow = TRUE)
test_case_2 <- matrix(c(FALSE,TRUE,FALSE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,FALSE,TRUE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,TRUE,TRUE,TRUE,TRUE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE), nrow = 6,byrow = TRUE)
path_1=dfs(test_case_1,1)
path_2=dfs(test_case_2,6)
cat("Final path(test_case_1): ", path_1)
cat("Final path(test_case_2): ", path_2)
  
