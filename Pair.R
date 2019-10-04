

for(i in 1:nrow(dataset)){
  for(j in 1:nrow(Domain_Topologies)){
    if(dataset[i,1] == Domain_Topologies[j,1]){
      dataset[i,10] = Domain_Topologies[j,10]
      next()
    }
  }
}



library(stringi)
library(stringr)
library(bio3d)
library(sqldf)

for(i in 1:nrow(dataset)){
  #i = 116
  variable = dataset[83,]
  Topologies = variable[1,17]
  
  
  Number_Pair = as.numeric(stri_count(Topologies, coll = "<"))
  
  Matrix_Pair = matrix(0, nrow = Number_Pair, ncol = 2)
  Vector_Pair = as.numeric(str_extract_all(Topologies, "[0-9]+")[[1]])
  
  j = 1
  for(M in 1:nrow(Matrix_Pair)){
    Matrix_Pair[M,1] = Vector_Pair[j]
    j = j + 1
    Matrix_Pair[M,2] = Vector_Pair[j]
    j = j + 1
  }
  
  
 
  
  ID_Protein = substr(variable$Chain_Name,1,4)
  PDB_1efu = read.pdb(ID_Protein)
  DSSP_1efu <- dssp(PDB_1efu, exefile = "E://dssp")
  
  Sheet_1efu = DSSP_1efu$sheet
  Sheet_1efu = as.data.frame(Sheet_1efu)
  Sheet_1efu$chain = tolower(Sheet_1efu$chain)
  
  Chain_1efu = substr(variable$Chain_Name,5,5)
  
  #restrict based on Chain Number
  Sheet_1efu = Sheet_1efu[(Sheet_1efu$chain == Chain_1efu),]
  
  Sheet_1efu["Sheet_Number"] = 1:nrow(Sheet_1efu)
  
  #restric Based on Start and End Number
  Sheet_1efu = Sheet_1efu[(Sheet_1efu$start >=variable$StartLen & 
                             Sheet_1efu$end <= variable$EndLen),]
  
  Sheet_Min = min(Sheet_1efu[,5])
  Sheet_Max = max(Sheet_1efu[,5])
  
  Matrix_Pair1 = Matrix_Pair
  Matrix_Pair1 = as.data.frame(Matrix_Pair1)
  
  
  #now we must restrict matrix_Pair1 based on Sheet_Min AND Sheet_Max
  
  Matrix_Pair1 = Matrix_Pair1[(Matrix_Pair1$V1 <= Sheet_Max 
                               | Matrix_Pair1$V1 >= Sheet_Min |
                                 Matrix_Pair1$V2 <= Sheet_Max |
                                 Matrix_Pair1$V2 >= Sheet_Min),]
  
  
  rm(Matrix_Pair1)
  rm(Matrix_Pair)
  rm(variable)
  
  
}

write.csv(dataset, "E://Dataset.csv")



library(RODBC)

#Connection
db = odbcConnect("Aimer")

db
#Execute Query and select Chain Tablw
Chain_Dataset = sqlQuery(db, "Select * from Chain_list")

Chain_Dataset$id = tolower(Chain_Dataset$id)



for(i in 1:nrow(dataset)){
  for(j in 1:nrow(Chain_Dataset)){
    if(dataset[i,2] == Chain_Dataset[j,1]){
      dataset[i,10] = Chain_Dataset[j,2]
      next()
    }
  }
}

class(Chain_Dataset$topology)
Chain_Dataset$topology = as.character(Chain_Dataset$topology)


write.csv(dataset, "E://Domain1060_Ntop.csv")
write.csv(Chain_Dataset, "E://Chain_Topology_Correct.csv")








