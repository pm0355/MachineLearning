library(data.table)
library(raster)
library(e1071)
library(deepnet)
insert.at <- function(a, pos, ...){
  dots <- list(...)
  stopifnot(length(dots)==length(pos))
  result <- vector("list",2*length(pos)+1)
  result[c(TRUE,FALSE)] <- split(a, cumsum(seq_along(a) %in% (pos+1)))
  result[c(FALSE,TRUE)] <- dots
  unlist(result)
}
ptm <- proc.time()
  #import class label information and assign to matrix
  
  value=raster("C:/Users/matti/Google Drive/Classes 2016/Fall 2016/Machine Learning/assignment2/dat/evi_stack/cslabel.tif")
  classlabel=as.matrix(value) 

  #import each band/feature to a unique raster
  
  band1=raster("C:/Users/matti/Google Drive/Classes 2016/Fall 2016/Machine Learning/assignment2/dat/evi_stack/evi_stack.tif", band=1)
  band2=raster("C:/Users/matti/Google Drive/Classes 2016/Fall 2016/Machine Learning/assignment2/dat/evi_stack/evi_stack.tif", band=2)
  band3=raster("C:/Users/matti/Google Drive/Classes 2016/Fall 2016/Machine Learning/assignment2/dat/evi_stack/evi_stack.tif", band=3)
  band4=raster("C:/Users/matti/Google Drive/Classes 2016/Fall 2016/Machine Learning/assignment2/dat/evi_stack/evi_stack.tif", band=4)
  band5=raster("C:/Users/matti/Google Drive/Classes 2016/Fall 2016/Machine Learning/assignment2/dat/evi_stack/evi_stack.tif", band=5)
  band6=raster("C:/Users/matti/Google Drive/Classes 2016/Fall 2016/Machine Learning/assignment2/dat/evi_stack/evi_stack.tif", band=6)
  band7=raster("C:/Users/matti/Google Drive/Classes 2016/Fall 2016/Machine Learning/assignment2/dat/evi_stack/evi_stack.tif", band=7)
  band8=raster("C:/Users/matti/Google Drive/Classes 2016/Fall 2016/Machine Learning/assignment2/dat/evi_stack/evi_stack.tif", band=8)
  band9=raster("C:/Users/matti/Google Drive/Classes 2016/Fall 2016/Machine Learning/assignment2/dat/evi_stack/evi_stack.tif", band=9)
  band10=raster("C:/Users/matti/Google Drive/Classes 2016/Fall 2016/Machine Learning/assignment2/dat/evi_stack/evi_stack.tif", band=10)
  band11=raster("C:/Users/matti/Google Drive/Classes 2016/Fall 2016/Machine Learning/assignment2/dat/evi_stack/evi_stack.tif", band=11)
  band12=raster("C:/Users/matti/Google Drive/Classes 2016/Fall 2016/Machine Learning/assignment2/dat/evi_stack/evi_stack.tif", band=12)
  band13=raster("C:/Users/matti/Google Drive/Classes 2016/Fall 2016/Machine Learning/assignment2/dat/evi_stack/evi_stack.tif", band=13)
  band14=raster("C:/Users/matti/Google Drive/Classes 2016/Fall 2016/Machine Learning/assignment2/dat/evi_stack/evi_stack.tif", band=14)
  band15=raster("C:/Users/matti/Google Drive/Classes 2016/Fall 2016/Machine Learning/assignment2/dat/evi_stack/evi_stack.tif", band=15)
  band16=raster("C:/Users/matti/Google Drive/Classes 2016/Fall 2016/Machine Learning/assignment2/dat/evi_stack/evi_stack.tif", band=16)
  band17=raster("C:/Users/matti/Google Drive/Classes 2016/Fall 2016/Machine Learning/assignment2/dat/evi_stack/evi_stack.tif", band=17)
  band18=raster("C:/Users/matti/Google Drive/Classes 2016/Fall 2016/Machine Learning/assignment2/dat/evi_stack/evi_stack.tif", band=18)
  band19=raster("C:/Users/matti/Google Drive/Classes 2016/Fall 2016/Machine Learning/assignment2/dat/evi_stack/evi_stack.tif", band=19)
  band20=raster("C:/Users/matti/Google Drive/Classes 2016/Fall 2016/Machine Learning/assignment2/dat/evi_stack/evi_stack.tif", band=20)
  band21=raster("C:/Users/matti/Google Drive/Classes 2016/Fall 2016/Machine Learning/assignment2/dat/evi_stack/evi_stack.tif", band=21)
  band22=raster("C:/Users/matti/Google Drive/Classes 2016/Fall 2016/Machine Learning/assignment2/dat/evi_stack/evi_stack.tif", band=22)
  band23=raster("C:/Users/matti/Google Drive/Classes 2016/Fall 2016/Machine Learning/assignment2/dat/evi_stack/evi_stack.tif", band=23)

  # assign each of 23 bands to a data frame holding each set of pixel values   
  # uniformly randomize data set svms increase 
  
  df= data.table(values(band1), values(band2),values(band3),values(band4),values(band5),values(band6),values(band7),values(band8),values(band9),values(band10),values(band11),values(band12),values(band13),values(band14),values(band15),values(band16),values(band17),values(band18),values(band19),values(band20),values(band21),values(band22),values(band23))
  index<-1800000:1800250
  indextest<-1:3688354
  trainset<-df[index]
#  mysvm=svm(trainset, classlabel[index],type='C', kernel='radial',gamma=.1, cost=68)
 mysvm=svm(trainset, classlabel[index],type='C', kernel='radial',gamma=.5, cost=4)
  #svm tells us the data is linearly separable at a span of 750 pixels sampled from 23 bands.
  p=predict(mysvm,trainset)
  tab= table(p, classlabel[index])
  print(tab)
  # assign values to 2d arr[[i,j]] to run svm tests later
  dnn1<-dbn.dnn.train(as.matrix(trainset), classlabel[index], hidden = c(5), activationfun = "sigm", learningrate = 0.8,
                momentum = 0.5, learningrate_scale = 1, output = "sigm", numepochs = 5,
                batchsize = 100, hidden_dropout = 0, visible_dropout = 0, cd = 1)
 
  #test our nn clasifier for error rate 

  dnn55<-dbn.dnn.train(as.matrix(trainset), classlabel[index], hidden = c(5,5), activationfun = "sigm", learningrate = 0.8,
                      momentum = 0.5, learningrate_scale = 1, output = "sigm", numepochs = 3,
                      batchsize = 100, hidden_dropout = 0, visible_dropout = 0, cd = 1)
  dnn532<-dbn.dnn.train(as.matrix(trainset), classlabel[index], hidden = c(5,6,3), activationfun = "sigm", learningrate = 0.8,
                       momentum = 0.5, learningrate_scale = 1, output = "sigm", numepochs = 3,
                       batchsize = 100, hidden_dropout = 0, visible_dropout = 0, cd = 1)
  nn.test(dnn1,df[index],classlabel[index])
  nn.test(dnn55,df[index],classlabel[index])
  nn.test(dnn532,df[index],classlabel[index])

  
  
  