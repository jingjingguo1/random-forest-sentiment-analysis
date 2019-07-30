
#===============DECISION TREE==================
# build_decision_tree.R
# Input: branch data with 0/1 elements 
# Output: Tree with features selected to max Gini Gain recursively at each node
# 

########## GINI INDEx ###########
gini_index <- function(branch_data){
  if ( length(branch_data) == 0 ){
    GI <- -1
  }else{
    n_t <- length(branch_data)
    n_1 <- sum(branch_data)
    n_0 <- n_t - n_1
    GI <- 1 - ( (n_1/n_t)^2 + (n_0/n_t)^2 )
  }
  return(GI)
}
# #debug
# branch_data <- c(0, 0, 1, 1, 0)
# gini_index(branch_data)


############### GINI GAIN###########
# Input: matrix with first column being "class/label", and second being the i-th "feature"
# Output: scalar 
gini_gain <- function(data){
  N_D <- dim(data)[1] # number of observations
  GI_i <- gini_index(data[,1])
  D_0 <- as.matrix(data[data[,2] == 0,])
  D_1 <- as.matrix(data[data[,2] == 1,])
  
  if( length(D_0)==0 | length(D_1) ==0 ){
    GG <- 1
  }else{
    GG0 <- gini_index(D_0[,1])
    GG1 <- gini_index(D_1[,1])
    GG <- GG1 * ( dim(D_1)[1]/N_D ) + GG0 * ( dim(D_0)[1]/N_D )
  }
  return(GG)
}
# #debug
# data <- matrix(c(0,0,1,1,0,0,1,0,1,1), nrow = 5)
# gini_gain(testData)

############ BEST FEATURE ###############
find_best_feature <- function(data){
  GG = list()
  # if ( dim(data)[2] == 1) {
  #   data <- matrix(data, nrow = 1)
  # }
  n_f = dim(data)[2] - 1
  
  for(feature in 1:n_f){
    test_data <- data[,c(1,feature+1)]
    GG[feature] <- gini_gain(test_data)
  }
  best_feature <- which.min(GG) + 1
  return(best_feature)
}
# #debug
# data <- matrix(c(0,0,1,1,0, 0,1,0,1,1, 1,0,0,0,1, 1,1,1,1,0, 0,0,1,1,0), nrow = 5)
# x<- find_best_feature(data)


########### Build Tree ##############
to_leaf <- function(leafData){
	n_t <- dim(leafData)[1] # nrows
	n_1 <- sum(leafData[,1])
	
	if( 2*n_1 >= n_t ){
		output <- 1
	}else{
		output <- 0
	}
	return(output)
}

# input: matrix
split_table <- function(data){
	attr <- find_best_feature(data)
	n_c <- dim(data)[2]
	n_t <- dim(data)[1]
	n_1 <- sum(data[,attr])
	
	left_dataset <- matrix(data[data[,attr]==1,], ncol=n_c)
	right_dataset <- matrix(data[data[,attr]==0,], ncol=n_c)
	
	# 
	# if( (n_t-n_1) == 1 && n_1 == 1){
	#   left_dataset <- matrix(data[data[,attr]==1,], nrow=1)
	#   right_dataset <- matrix(data[data[,attr]==0,], nrow=1)
	# }else if( (n_t-n_1) ==1){
	#   left_dataset <- as.matrix(data[data[,attr]==1,])
	#   right_dataset <- matrix(data[data[,attr]==0,],nrow=1)
	# }else if(n_1 ==1){
	#   left_dataset <- matrix(data[data[,attr]==0,],nrow=1)
	#   right_dataset <- as.matrix(data[data[,attr]==1,])
	# }else{
	#   left_dataset <- as.matrix(data[data[,attr]==1,])
	#   right_dataset <- as.matrix(data[data[,attr]==0,])
	# }

	output <- list(index = attr, data = list(left=left_dataset, right=right_dataset) )
	return(output)
}
# data <- matrix(c(0,0,1,1,0, 0,1,0,1,1, 1,0,0,0,1, 1,1,1,1,0, 0,0,1,1,0), nrow = 5)
# x <- split_table(data)


# input: 
# output tree
grow_tree <- function(node, maxLevel, minSize, level){
  left <- node$data$left
  right <- node$data$right
  node$data <- NULL
  
  #check for purity
  if( dim(left)[1] == 0 | dim(right)[1] == 0 ){
    node$left <- to_leaf( rbind(left,right) )
    node$right <- to_leaf( rbind(left,right) )
    print(node)
    return()
  }

  if(level >= maxLevel){
    node$left <- to_leaf(left)
    node$right <- to_leaf(right)
    return()
  }

  if(dim(left)[1] <= minSize){
    node$left <- to_leaf(left)
   # return()
  }else{
    node$left <- split_table(left)
    node$left <- grow_tree(node$left, maxLevel, minSize, level+1)
  }

  if(dim(right)[1] <= minSize){
    node$right <- to_leaf(right)
   # return()
  }else{
    node$right <- split_table(right)
    node$right <- grow_tree(node$right, maxLevel, minSize, level+1)
  }
  print("--------------")
  print(node)
}

decision_tree <- function(dataset, maxLevel, minSize){
	root <- split_table(dataset)
	root <- grow_tree(root, maxLevel, minSize, 1)
	return(root)
}

# data <- matrix(c(0,0,1,1,0, 0,1,0,1,1, 1,0,0,0,1, 1,1,1,1,0, 0,0,1,1,0), nrow = 5)
# data1 <- matrix(c(0,0,1,1,0, 0,1,0,1,1, 1,0,0,0,1, 1,1,1,1,0, 0,0,1,1,0, 1,1,1,1,1, 0,1,1,1,0, 1,1,1,0,0), nrow = 8)
#  # x <- split_table(data)
# res <- decision_tree(data1, 10, 2)

#===================PREDICT WITH DECISION TREE===============

decision_tree_error <- function(trainD, testD, maxLevel, minSize){
	treeModel <- decision_tree(trainD, maxLevel, minSize)
	n_t <- dim(testD)[1]
	correctCount <- 0
	for(i in 1:n_t){
	  data_entry <- testD[i,]
	  print("tree and data entry")
	  print(treeModel)
	  print(data_entry)
		y_hat <- predict(treeModel, data_entry)
		y <- data_entry[1]
		#if( is.null(y_hat) ) y_hat <- 0
		# print("y")
		# print(y)
		# print(y_hat)
		if(y == y_hat){
			correctCount = correctCount + 1
		}
	}
	zero_one_loss <- 1 - correctCount/n_t
	return(zero_one_loss)
}

# line by line predict; return just the numeric value 0 or 1.
predict <- function(node, data_entry){
	if(data_entry[node$index] == 1){
		if(class(node$left) == "list"){
			node$left <- predict(node$left, data_entry)
			return(node$left)
		}else{
			return(node$left)
		}
	}else{
		if(class(node$right) == "list"){
			node$right <- predict(node$right, data_entry)
			return(node$right)
		}else{
			return(node$right)
		}
	}
}

#====================RANDOM FOREST==========================


#build Random forest
build_random_forest <- function(train_vectors, sampleSize, maxLevel, minSize){
	n_f <- dim(train_vectors)[2] - 1 # number of features
	n_e <- dim(train_vectors)[1]	# number of examples/entries
	
	k <- sampleSize # number of features to pick for random forest
	sp <- as.integer( sqrt(n_f) )
	trees <- list()

	for(i in 1:10){
	  set.seed(i) # to delete
		indX <- sample(n_e, k, replace = TRUE)
		sampleX <- train_vectors[indX,]
		trees[[i]] <- decision_tree(sampleX,maxLevel,minSize)
	}
	return(trees)
}

# train_vectors <- data
# sampleSize <- 5
# maxLevel <- 10
# minSize <- 2
# forest <- build_random_forest(train_vectors, sampleSize, maxLevel, minSize)

random_forest_error <- function(train_vectors, test_vectors, sampleSize, maxLevel, minSize){
	trees <- build_random_forest(train_vectors, sampleSize, maxLevel, minSize)
	n_trees <- length(trees)
	
	counterError <- 0
	n_tv <- dim(test_vectors)[1]
	for(i in 1:n_tv){
	  line <- test_vectors[i,]
		vote1 <- 0
		for(j in 1:n_trees){
		  print("hello")
		  print(i)
		  print(j)
			if(predict(trees[[j]], line) == 1) vote1<-vote1+1
		}
		if(vote1>=5){
			y_hat <- 1
		}else{
			y_hat <- 0
		}

		if(y_hat != line[1] ) {
		  counterError <- counterError + 1
		}
	}
	return( counterError/(dim(test_vectors)[1]) )
}


#=====================DATA PROCESSING======================
rm(list=ls())
# setwd("/Users/zhouyue/Desktop/STAT545_Final_project/review")
workdir <- "C:\\Users\\hitgj\\Workspaces\\R\\stat545\\projR\\junk"
setwd(workdir)
library("tm")
review<-VCorpus(DirSource(directory = workdir,encoding = "UTF-8"))
review<-tm_map(review,removePunctuation)
review<-tm_map(review, content_transformer(tolower))
review <-tm_map(review, removeWords, stopwords("english"))
# frequency
dtm_1<-DocumentTermMatrix(review)
freq_1 <- sort(colSums(as.matrix(dtm_1)), decreasing=TRUE)
#First_1<-head(freq_1,20)
#wf_1<- data.frame(word=names(freq_1),freq=freq_1)
#wf_1<-wf_1[1:15,1]
#wf_1<-wf_1[,1]
wf_1<-list(word=names(freq_1))


read_data <- function(filename,num_rows,feature){
  library(stringr)
  con <- file(filename, open = "r") # setup a file connection
  data<-matrix(0,num_rows,(length(feature)+1))
  #Initialize output variables
  #data = list(label=list(), review=list())
  for(i in 1:num_rows){
    line <- readLines(con, n = 1, warn = FALSE)
    data_1<-as.integer(str_extract(line, "[[:digit:]]+"))
    data_2<- gsub("[^a-z0-9_ ]+", "", tolower(line))
    data_2<- strsplit(data_2," ")
    data_2<- unlist(data_2[[1]][-1])
    data[i,2:(length(feature)+1)]<-as.integer(feature%in% data_2)
    data[i,1]<-data_1-1
    ##change to Matrix
    ##
  }
  close(con)
  return(data)
}

filename = "test.txt"
feature<-wf_1$word[3:15]
output<-read_data(filename,389,feature)