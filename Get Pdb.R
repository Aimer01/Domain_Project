

library(bio3d)
for(i in 1:916){
  get.pdb(substr(Chain_Dataset[i,1],1,4), path = "F://New")
}

print("Finished!!!!")