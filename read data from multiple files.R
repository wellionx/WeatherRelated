#Read some columns from each file, and rename the colname with file name ####
# Get the files names
files.name = list.files(pattern="*.csv")
files.length=length(files.name)
#extract the file names
library(tools)
# file_ext(files.name[1:files.length])
kernal.names <- file_path_sans_ext(files.name[1:files.length])
head(kernal.names)
newdata=numeric(0)
for(i in 1:files.length){
  tmp=read.csv(files.name[i],head=T,sep=",",skip = 2)
  tmp$name=kernal.names[1]
  newdata=rbind(newdata,tmp)} 
dim(newdata)

#large data read from multiple files ####
#using fread and lapply function
files = list.files(pattern = "*.TXT")
files
library(data.table)
DT = do.call(rbind, lapply(files, fread, header=FALSE))

#read xlsx files #####
files = list.files(pattern="*.xlsx")

# 设置工作空间
# 读取该工作空间下的所有文件名
filenames <- dir()
# 通过正则，获取所有xlsx结尾的文件名
filenames2 <- grep('\\.xlsx', filenames, value = TRUE)
# 初始化数据框，用于后面的数据合并
dta <- data.frame()
#通过循环完成数据合并
for (i in filenames2){
  # 构造数据路径
  path <- paste0(getwd(),'\\',i)
  #res <- c(res,path)
  # 读取并合并数据
  dta <- rbind(dta,read.xlsx(path,sheet = 1))
}