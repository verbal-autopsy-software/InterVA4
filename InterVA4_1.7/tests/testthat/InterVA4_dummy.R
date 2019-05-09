library(InterVA4)

data("probbase", envir = environment())
probbase <- get("probbase", envir = environment())

probbase[probbase=="I"]<-1
probbase[probbase=="A+"]<-0.8
probbase[probbase=="A"]<-0.5
probbase[probbase=="A-"]<-0.2
probbase[probbase=="B+"]<-0.1
probbase[probbase=="B"]<-0.05
probbase[probbase=="B-"]<-0.02
probbase[probbase=="B -"]<-0.02
probbase[probbase=="C+"]<-0.01
probbase[probbase=="C"]<-0.005
probbase[probbase=="C-"]<-0.002
probbase[probbase=="D+"]<-0.001
probbase[probbase=="D"]<-0.0005
probbase[probbase=="D-"]<-0.0001
probbase[probbase=="E"]<-0.00001
probbase[probbase=="N"]<-0
probbase[probbase==""]<-0

probs <- as.data.frame(probbase[1:nrow(probbase),14:76]) #ignore D group
probs <- apply(probs, 2, as.numeric)

# make row names symptoms
rownames(probs) <- c("Prior", probbase[2:nrow(probbase),2])

data("causetext", envir = environment())
causetext <- get("causetext", envir = environment())

#make ages and genders whatever top indicator is for each cause
picktop <- function(df){
  switchmax <- function(x){
    singlemax <- rep(0, length(x))
    singlemax[which.max(x==max(x))] <- 1
    singlemax
  }
  apply(df, 2, switchmax)
}
likelihood <- 0.5 # strength of connection for symptom x cod to count as yes

probs[2:8,] <- picktop(probs[2:8,])
probs[9:10,] <- picktop(probs[9:10,])

# Actual setting up condition for y/n
#dummydata <- apply(probs,2, function(x) { ifelse(x > x["Prior"]*10 | x >= 0.8, "y", "n")}) %>% t  %>% as.data.frame

dummydata <- as.data.frame( t(  apply(probs,2, function(x) { ifelse(x >= likelihood, "y", ".")}) ), stringsAsFactors=F )

dummydata <- dummydata[, !colnames(dummydata) %in% "Prior"]


rownames(dummydata) <- gsub(".C.7",  "",  colnames(probbase)[14:76])

ids <- merge(data.frame(code=rownames(dummydata), stringsAsFactors=F), as.data.frame(causetext[, c("CAUSETXT.C.40", "CAUSE.C.")], stringsAsFactors=F), by.x="code", by.y="CAUSE.C.")
ids <- ids[match(rownames(dummydata), ids$code), ] #order according to dummydata
ids$ID <- as.character(ids$CAUSETXT.C.40)

dummydata <- cbind(ids$ID, dummydata)
colnames(dummydata)[1] <- "ID"

# convert to strings
rn <- rownames(dummydata)
dummydata <- apply(dummydata, 2, as.character)
rownames(dummydata) <- rn

# cherry picking problems
# IDs  for pregnancy items
dummydata[rownames(dummydata)=="A_NRP", "ID"] <- "nrp   "
dummydata[rownames(dummydata)=="A_PEND_6W", "ID"] <- "pr6w  "
dummydata[rownames(dummydata)=="A_PREG", "ID"] <- "preg  "

# tetanus (11)
id <- dummydata["B_TETAN", "ID"]
dummydata["B_TETAN", ] <- "."
dummydata["B_TETAN", c("male","infant", "rigidity", "umbinf", "ch_fever")] <- "y"
dummydata["B_TETAN", "ID"] <- id

# Other and unspecified external CoD  (54)
dummydata["B_EXT_OU", c("male")] <- "."
dummydata["B_EXT_OU", c("female")] <- "y"

rm(list=setdiff(ls(), "dummydata"))


# write.csv(dummydata,"dummydata_4.csv")
# evaldummy <- function(data){
#   res <- InterVA(data, HIV = "l", Malaria = "l", directory = getwd(), filename="TestVA")
#   causematch <- c()
#   ids <- c()
#   for (i in 1:length(res$VA)){
#     ids <- c(ids, res$VA[[i]]$ID)
#     causematch <- c(causematch, res$VA[[i]]$ID %in% c(names(res$VA[[i]]$PREGLIK),
#                                                        res$VA[[i]]$CAUSE1))
#   }
#   cat(paste("Missing",nrow(dummydata) - length(res$VA),"cod\n"))
#   if (nrow(dummydata) > length(res$VA)) {cat(paste(dummydata[!dummydata[,"ID"] %in% ids, "ID"]))}
#   data.frame(COD = ids, Match = causematch)
# }
# 
# 
# cat("Making dummy test data")
# dummydata$ID <- rownames(dummydata)
# dummydata <- dummydata %>% select(ID, everything())
# evaldummy(dummydata) %>% filter(!Match)
