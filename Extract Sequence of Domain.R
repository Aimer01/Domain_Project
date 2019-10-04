dataset$Seq_Domain = NA


for(i in 1:1047){
  dataset[i,7] = substr(
    dataset[i,8], dataset[i,15], dataset[i,16]
  )
}

dataset["start"] = 0
dataset["End"] = 0

for(i in 1:1047){
  dataset[i,15] = dataset[i,11] - dataset[i,13] + 1
  
  dataset[i,16] = dataset[i,12] - dataset[i,13] + 1
}
