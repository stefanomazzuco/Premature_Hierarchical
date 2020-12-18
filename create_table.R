
lx2dx <- function(lx,age){
    ageint <- age[-1]-age[-length(age)]
    dx <- rep(NA,length(age))
    dx[1:length(age)-1] <- lx[-length(age)]-lx[-1]
    dx[length(age)] <- lx[length(age)]
    dx[is.na(dx)] <- 0
    return(dx)
}

e50 <- NULL
m3065 <- NULL
rel.alpha <- NULL

datamat <- matrix(NA,nrow=19,ncol=length(countrym))
colnames(datamat) <- countrym
rownames(datamat) <- c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85)
for (c in 1:length(countrym)){
    #life table data (NAME_OF_COUNTRY_mltper_5x1.txt) can be downloaded from https://www.ssc.wisc.edu/cdha/latinmortality2/
    lt <- read.table(paste(countrym[c],"_mltper_5x1.txt",sep=""), header = TRUE, na.strings = ".")
    last.Y <- max(lt$Year)
    e50 <- lt$ex[lt$Year==last.Y&lt$Age==50]
    print(list(countrym[c],last.Y,e50))
    l.x <- lt$lx[lt$Year==last.Y]
    Age <-lt$Age[lt$Year==last.Y]
    dx <- lx2dx(l.x,Age)
    datamat[,c] <- dx
    }
    
write.csv(datamat,file=paste("tables_","LAMBDA",".csv",sep=""))
