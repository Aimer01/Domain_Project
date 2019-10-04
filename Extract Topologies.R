
install.packages("RODBC")
library(RODBC)

#Connection
db = odbcConnect("Aimer")

db
#Execute Query and select Chain Tablw
Chain_Dataset = sqlQuery(db, "Select * from Chain_list")

Chain_Dataset$id = tolower(Chain_Dataset$id)


#Extract Topologies of Domain

Domain_Dataset["Topology"] = "0"
Domain_Dataset$Topology = as.character(Domain_Dataset$Topology)
Chain_Dataset$id = as.character(Chain_Dataset$id)
Chain_Dataset$topology = as.character(Chain_Dataset$topology)
Chain_Dataset$id = as.character(Chain_Dataset$id)
class(Chain_Dataset$id)



i = 1
j = 1
for(i in 1:nrow(Chain_Dataset)){
  Chain_Name = Chain_Dataset[i,2]
  Topology = Chain_Dataset[i,6]
  for(j in 1:nrow(Domain_Dataset)){
    if(Domain_Dataset[j,2] == Chain_Name)
      Domain_Dataset[j,11] = Topology
  }
}


write.csv(Domain_Dataset, "E://Domain_Topologies.csv")

Test_Dataset = Domain_Dataset[3,]

rm(i)
rm(j)
rm(db)
rm(Topology)
rm(Chain_Name)


