

library(bio3d)
library(stringr)
library(stringi)

t = 403

for(t in 880:1060){
  
  Variable = dataset[t,]
  ID_Protein = substr(Variable$Chain_Name,1,4)
  PDB_1efu = read.pdb(ID_Protein)
  DSSP_1efu <- dssp(PDB_1efu, exefile = "E://dssp")

  
  #Extract Helix Sheet and Loop
  #combine this 3 matrix
  #helix = 0, sheet = 1 and loop = 2
  #sort based on start
  
  Helix_1efu = DSSP_1efu$helix
  Sheet_1efu = DSSP_1efu$sheet
  Loop_1efu = DSSP_1efu$turn
  
  Helix_1efu = as.data.frame(Helix_1efu)
  Sheet_1efu = as.data.frame(Sheet_1efu)
  Loop_1efu = as.data.frame(Loop_1efu)
  
  if(nrow(Helix_1efu)!=0){
    Helix_1efu = Helix_1efu[,-5]
    Helix_1efu$chain = tolower(Helix_1efu$chain)
  }
  
  
  Sheet_1efu$chain = tolower(Sheet_1efu$chain)
  Loop_1efu$chain = tolower(Loop_1efu$chain)
  
  #Extract chain of protein
  Chain_1efu = substr(Variable$Chain_Name,5,5)
  
  #Add Column
  
  if(nrow(Helix_1efu)!=0){
    Helix_1efu["type"] = 0
  }
  
  Sheet_1efu["type"] = 1
  Loop_1efu["type"] = 2
  
  #combine 3 data frame
  
  
  if(nrow(Helix_1efu) == 0){
    SS_1efu = rbind(Sheet_1efu,Loop_1efu)
  } else if(nrow(Sheet_1efu) == 0){
    SS_1efu = rbind(Helix_1efu,Loop_1efu)
  } else if(nrow(Loop_1efu) == 0){
    SS_1efu = rbind(Helix_1efu,Sheet_1efu)
  } else{
    SS_1efu = rbind(Helix_1efu,Sheet_1efu,Loop_1efu)
  }

  SS_1efu = SS_1efu[order(SS_1efu$start),]
  
  Chain_1efu = tolower(Chain_1efu)
  #Extract record based on chain name
  
  SS_1efu = SS_1efu[(SS_1efu$chain == Chain_1efu),]
  Start_Test = as.numeric(Variable$StartLen)
  End_Test = as.numeric(Variable$EndLen)
  
  p2 = 1
  SS_1efu1 = as.data.frame(SS_1efu[1,])
  for(t2 in 1:nrow(SS_1efu)){
    if(SS_1efu[t2,1] >= Start_Test & SS_1efu[t2,2] <= End_Test){
      SS_1efu1[p2,] = SS_1efu[t2,]
      p2 = p2 + 1
    }
  }
  SS_1efu = SS_1efu1
  
  
  Vec = Variable$Seq_Domain
  Vec = unlist(str_split(Vec, pattern = ""))
  L = 1
  
  for(L in 1:nrow(SS_1efu)){
    if(SS_1efu[L,5] == 0){
      S_EX = SS_1efu[L,1] - Start_Test + 1
      E_EX = SS_1efu[L,2] - Start_Test + 1
      Vec[S_EX:E_EX] = "H"
    }
    else if(SS_1efu[L,5] == 1){
      S_EX = SS_1efu[L,1] - Start_Test + 1
      E_EX = SS_1efu[L,2] - Start_Test + 1
      Vec[S_EX:E_EX] = "E"
    }
    else{
      S_EX = SS_1efu[L,1] - Start_Test + 1
      E_EX = SS_1efu[L,2] - Start_Test + 1
      Vec[S_EX:E_EX] = "L"
    }
  }
  
  
  Vec = replace(Vec, (Vec!= "H" & Vec!= "L"& Vec != "E") , "L")
  
  Vec = paste(Vec, collapse = "")
  
  
 
  Variable$SS_1efu = Vec
  dataset[t,] = Variable
  
  rm(Helix_1efu)
  rm(Sheet_1efu)
  rm(Loop_1efu)
  rm(SS_1efu)
  rm(Chain_1efu)
  rm(DSSP_1efu)
  rm(End_1efu)
  rm(i)
  rm(ID_Protein)
  rm(ID_Protein)
  rm(PDB_1efu)
  rm(SS_1efu1)
  rm(Start_1efu)
  rm(Temp_1efu)
  rm(temp)
  rm(V1)
  rm(Vector_1efu)
  rm(Vector_Test)
  #rm(t)
  rm(End_1efu)
  rm(Start_1efu)
  rm(SS_1efu1)
  rm(t2)
  rm(L)
  rm(E_EX)
  rm(S_EX)
  rm(p2)
  rm(Vec)
  rm(End_Test)
  rm(Start_Test)
}

print("*******************************")
print("Done!!!!!!!!!!!!!!!!!!!!!1")
print("*******************************")

write.csv(dataset,"E://dataset.csv")

dataset$V17 = dataset$SS_1efu
rm(t)
