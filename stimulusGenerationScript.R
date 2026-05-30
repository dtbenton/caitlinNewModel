##############################################
# Stimulus Generation Script for Bellbottoms #
##############################################

###################################
# GENERATE VECTORS FOR SIMULATION #
###################################

############
## OBJECT ##
############

people_stim = diag(24)
people_stim_full = as.data.frame(people_stim)

names(people_stim_full) = NULL
rownames(people_stim_full) = NULL
row.names(people_stim_full) = NULL

people_group_1  = people_stim_full[1:6,]
people_group_2  = people_stim_full[7:12,]
people_group_3  = people_stim_full[13:18,]
people_group_4  = people_stim_full[19:24,]



###################
## CLOTHING ITEM ##
###################

clothing_item = diag(6)
clothing_item = as.data.frame(clothing_item)
names(clothing_item) = NULL

# TOP_1: 1 0 0 0 0 0
# TOP_2: 0 1 0 0 0 0
# TOP_3: 0 0 1 0 0 0
# BOTTOM_1: 0 0 0 1 0 0
# BOTTOM_2: 0 0 0 0 1 0
# BOTTOM_3: 0 0 0 0 0 1


######################
## CONSTANT FEATURE ##
######################

constant = diag(6)
constant = as.data.frame(constant)
names(constant) = NULL

# CONSTANT_1: 1 0 0 0 0 0
# CONSTANT_2: 0 1 0 0 0 0
# CONSTANT_3: 0 0 1 0 0 0
# CONSTANT_4: 0 0 0 1 0 0
# CONSTANT_5: 0 0 0 0 1 0
# CONSTANT_6: 0 0 0 0 0 1

###################
## SLEEVE_LENGTH ##
###################
sleeve_length = diag(6)
sleeve_length = as.data.frame(sleeve_length)
names(sleeve_length) = NULL

# Sleeve Length 1: 1 0 0 0 0 0
# Sleeve Length 2: 0 1 0 0 0 0
# Sleeve Length 3: 0 0 1 0 0 0
# Sleeve Length 4: 0 0 0 1 0 0
# Sleeve Length 5: 0 0 0 0 1 0
# Sleeve Length 6: 0 0 0 0 0 1

# shift the sleeve for test
sleeve_shifted = sleeve_length[c(2,5,4,6,1,3), ]

####################
## OVERALL_LENGTH ##
####################
overall_length = diag(6)
overall_length = as.data.frame(overall_length)
names(overall_length) = NULL

# overall Length 1: 1 0 0 0 0 0
# overall Length 2: 0 1 0 0 0 0
# overall Length 3: 0 0 1 0 0 0
# overall Length 4: 0 0 0 1 0 0
# overall Length 5: 0 0 0 0 1 0
# overall Length 6: 0 0 0 0 0 1

# shift the overall for test
overall_shifted = overall_length[c(6, 1:5), ]


# shifted versions for unmodified training patterns
# These prevent topUnModified examples from having the same combined
# Clothing_Item + Sleeve_Length + Overall_Length + Constant pattern
# as the topModified examples.
clothing_item_unmodified = clothing_item
sleeve_length_unmodified = sleeve_length[c(2, 3, 4, 5, 6, 1), ]
overall_length_unmodified = overall_length[c(3, 4, 5, 6, 1, 2), ]
constant_unmodified = constant[c(4, 5, 6, 1, 2, 3), ]

# shifted versions for unmodified test patterns
# These preserve the same named_match / named_mismatch / neither logic
# used in testing.ex, but apply it to the unmodified feature mappings.

sleeve_shifted_unmodified = sleeve_length_unmodified[c(2, 5, 4, 6, 1, 3), ]
overall_shifted_unmodified = overall_length_unmodified[c(6, 1:5), ]


################
## LABEL TYPE ##
################
label = data.frame(x = c('0', '1'))
label = as.data.frame(label)
names(label) = NULL

# unmodified: 0
# modified: 1

##############
## TRAINING ##
##############
#setwd("C:/Users/bentod2/Documents/projects/current/caitlinVModel")
#setwd("C:/Users/bentod2/Documents/projects/current/Bell_Bottoms_Computational_Model")

l = 1
l1 = 1
l2 = 1
l3 = 1
l4 = 1


sink('training.ex')

# BLOCK 1
for(i in 1:nrow(people_group_1)){
  cat(paste("name: topModified_A_group_1_", i, "\n", sep=""))
  cat(
    paste0(
      "proc: {setLinkValues learningRate $::lr_salient -type salientFeature \n",
      "setLinkValues learningRate $::lr_nonsalient -type nonsalientFeature \n",
      "setLinkValues weightDecay $::wd_salient -type salientFeature \n",
      "setLinkValues weightDecay $::wd_nonsalient -type nonsalientFeature}",
      "\n"
    )
  )  
  # INPUT
  cat(paste("I:", "\n", sep="\t"))
  
  # Person
  cat(paste("(Person_IN)", sep="\t"))
  print(people_group_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Clothing Item
  cat(paste("(Clothing_Item_IN)", sep="\t"))
  print(clothing_item[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Sleeve Length
  cat(paste("(Sleeve_Length_IN)", sep="\t"))
  print(sleeve_length[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Overall Length
  cat(paste("(Overall_Length_IN)", sep="\t"))
  print(overall_length[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Constant
  cat(paste("(Constant_IN)", sep="\t"))
  print(constant[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Label
  cat(paste("(Label_IN)", sep="\t"))
  print(label[2,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # OUTPUT
  cat(paste("T:", "\n", sep="\t"))
  
  # Person
  cat(paste("(Person_OUT)", sep="\t"))
  print(people_group_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Clothing Item
  cat(paste("(Clothing_Item_OUT)", sep="\t"))
  print(clothing_item[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Sleeve Length
  cat(paste("(Sleeve_Length_OUT)", sep="\t"))
  print(sleeve_length[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Overall Length
  cat(paste("(Overall_Length_OUT)", sep="\t"))
  print(overall_length[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Constant
  cat(paste("(Constant_OUT)", sep="\t"))
  print(constant[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Label
  cat(paste("(Label_OUT)", sep="\t"))
  print(label[2,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  cat(paste(";", sep="\t"))
  cat("\n")
}


# PEOPLE GROUP 2
for(i in 1:nrow(people_group_2)){
  cat(paste("name: topModified_B_group_2_", i, "\n", sep=""))
  cat(
    paste0(
      "proc: {setLinkValues learningRate $::lr_salient -type salientFeature \n",
      "setLinkValues learningRate $::lr_nonsalient -type nonsalientFeature \n",
      "setLinkValues weightDecay $::wd_salient -type salientFeature \n",
      "setLinkValues weightDecay $::wd_nonsalient -type nonsalientFeature}",
      "\n"
    )
  )   
  # INPUT
  cat(paste("I:", "\n", sep="\t"))
  
  # Person
  cat(paste("(Person_IN)", sep="\t"))
  print(people_group_2[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Clothing Item
  cat(paste("(Clothing_Item_IN)", sep="\t"))
  print(clothing_item[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Sleeve Length
  cat(paste("(Sleeve_Length_IN)", sep="\t"))
  print(sleeve_length[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Overall Length
  cat(paste("(Overall_Length_IN)", sep="\t"))
  print(overall_length[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Constant
  cat(paste("(Constant_IN)", sep="\t"))
  print(constant[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Label
  cat(paste("(Label_IN)", sep="\t"))
  print(label[2,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # OUTPUT
  cat(paste("T:", "\n", sep="\t"))
  
  # Person
  cat(paste("(Person_OUT)", sep="\t"))
  print(people_group_2[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Clothing Item
  cat(paste("(Clothing_Item_OUT)", sep="\t"))
  print(clothing_item[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Sleeve Length
  cat(paste("(Sleeve_Length_OUT)", sep="\t"))
  print(sleeve_length[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Overall Length
  cat(paste("(Overall_Length_OUT)", sep="\t"))
  print(overall_length[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Constant
  cat(paste("(Constant_OUT)", sep="\t"))
  print(constant[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Label
  cat(paste("(Label_OUT)", sep="\t"))
  print(label[2,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
}

# PEOPLE GROUP 3
for(i in 1:nrow(people_group_3)){
  cat(paste("name: topUnModified_C_group_3_", i, "\n", sep=""))   
  # INPUT
  cat(paste("I:", "\n", sep="\t"))
  
  # Person
  cat(paste("(Person_IN)", sep="\t"))
  print(people_group_3[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Clothing Item
  cat(paste("(Clothing_Item_IN)", sep="\t"))
  print(clothing_item_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Sleeve Length
  cat(paste("(Sleeve_Length_IN)", sep="\t"))
  print(sleeve_length_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Overall Length
  cat(paste("(Overall_Length_IN)", sep="\t"))
  print(overall_length_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Constant
  cat(paste("(Constant_IN)", sep="\t"))
  print(constant_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Label
  cat(paste("(Label_IN)", sep="\t"))
  print(label[1,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # OUTPUT
  cat(paste("T:", "\n", sep="\t"))
  
  # Person
  cat(paste("(Person_OUT)", sep="\t"))
  print(people_group_3[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Clothing Item
  cat(paste("(Clothing_Item_OUT)", sep="\t"))
  print(clothing_item_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Sleeve Length
  cat(paste("(Sleeve_Length_OUT)", sep="\t"))
  print(sleeve_length_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Overall Length
  cat(paste("(Overall_Length_OUT)", sep="\t"))
  print(overall_length_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Constant
  cat(paste("(Constant_OUT)", sep="\t"))
  print(constant_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Label
  cat(paste("(Label_OUT)", sep="\t"))
  print(label[1,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
}



# PEOPLE GROUP 4
for(i in 1:nrow(people_group_4)){
  cat(paste("name: topUnModified_D_group_4_", i, "\n", sep=""))
  # INPUT
  cat(paste("I:", "\n", sep="\t"))
  
  # Person
  cat(paste("(Person_IN)", sep="\t"))
  print(people_group_4[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Clothing Item
  cat(paste("(Clothing_Item_IN)", sep="\t"))
  print(clothing_item_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Sleeve Length
  cat(paste("(Sleeve_Length_IN)", sep="\t"))
  print(sleeve_length_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Overall Length
  cat(paste("(Overall_Length_IN)", sep="\t"))
  print(overall_length_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Constant
  cat(paste("(Constant_IN)", sep="\t"))
  print(constant_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Label
  cat(paste("(Label_IN)", sep="\t"))
  print(label[1,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # OUTPUT
  cat(paste("T:", "\n", sep="\t"))
  
  # Person
  cat(paste("(Person_OUT)", sep="\t"))
  print(people_group_4[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Clothing Item
  cat(paste("(Clothing_Item_OUT)", sep="\t"))
  print(clothing_item_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Sleeve Length
  cat(paste("(Sleeve_Length_OUT)", sep="\t"))
  print(sleeve_length_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Overall Length
  cat(paste("(Overall_Length_OUT)", sep="\t"))
  print(overall_length_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Constant
  cat(paste("(Constant_OUT)", sep="\t"))
  print(constant_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Label
  cat(paste("(Label_OUT)", sep="\t"))
  print(label[1,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
}


# BLOCK 2
# BLOCK 1
for(i in 1:nrow(people_group_1)){
  cat(paste("name: topModified_A_group_1_", i, "\n", sep=""))
  cat(
    paste0(
      "proc: {setLinkValues learningRate $::lr_salient -type salientFeature \n",
      "setLinkValues learningRate $::lr_nonsalient -type nonsalientFeature \n",
      "setLinkValues weightDecay $::wd_salient -type salientFeature \n",
      "setLinkValues weightDecay $::wd_nonsalient -type nonsalientFeature}",
      "\n"
    )
  )   
  # INPUT
  cat(paste("I:", "\n", sep="\t"))
  
  # Person
  cat(paste("(Person_IN)", sep="\t"))
  print(people_group_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Clothing Item
  cat(paste("(Clothing_Item_IN)", sep="\t"))
  print(clothing_item[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Sleeve Length
  cat(paste("(Sleeve_Length_IN)", sep="\t"))
  print(sleeve_length[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Overall Length
  cat(paste("(Overall_Length_IN)", sep="\t"))
  print(overall_length[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Constant
  cat(paste("(Constant_IN)", sep="\t"))
  print(constant[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Label
  cat(paste("(Label_IN)", sep="\t"))
  print(label[2,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # OUTPUT
  cat(paste("T:", "\n", sep="\t"))
  
  # Person
  cat(paste("(Person_OUT)", sep="\t"))
  print(people_group_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Clothing Item
  cat(paste("(Clothing_Item_OUT)", sep="\t"))
  print(clothing_item[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Sleeve Length
  cat(paste("(Sleeve_Length_OUT)", sep="\t"))
  print(sleeve_length[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Overall Length
  cat(paste("(Overall_Length_OUT)", sep="\t"))
  print(overall_length[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Constant
  cat(paste("(Constant_OUT)", sep="\t"))
  print(constant[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Label
  cat(paste("(Label_OUT)", sep="\t"))
  print(label[2,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
}


# PEOPLE GROUP 2
for(i in 1:nrow(people_group_2)){
  cat(paste("name: topModified_B_group_2_", i, "\n", sep=""))
  cat(
    paste0(
      "proc: {setLinkValues learningRate $::lr_salient -type salientFeature \n",
      "setLinkValues learningRate $::lr_nonsalient -type nonsalientFeature \n",
      "setLinkValues weightDecay $::wd_salient -type salientFeature \n",
      "setLinkValues weightDecay $::wd_nonsalient -type nonsalientFeature}",
      "\n"
    )
  )   
  # INPUT
  cat(paste("I:", "\n", sep="\t"))
  
  # Person
  cat(paste("(Person_IN)", sep="\t"))
  print(people_group_2[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Clothing Item
  cat(paste("(Clothing_Item_IN)", sep="\t"))
  print(clothing_item[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Sleeve Length
  cat(paste("(Sleeve_Length_IN)", sep="\t"))
  print(sleeve_length[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Overall Length
  cat(paste("(Overall_Length_IN)", sep="\t"))
  print(overall_length[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Constant
  cat(paste("(Constant_IN)", sep="\t"))
  print(constant[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Label
  cat(paste("(Label_IN)", sep="\t"))
  print(label[2,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # OUTPUT
  cat(paste("T:", "\n", sep="\t"))
  
  # Person
  cat(paste("(Person_OUT)", sep="\t"))
  print(people_group_2[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Clothing Item
  cat(paste("(Clothing_Item_OUT)", sep="\t"))
  print(clothing_item[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Sleeve Length
  cat(paste("(Sleeve_Length_OUT)", sep="\t"))
  print(sleeve_length[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Overall Length
  cat(paste("(Overall_Length_OUT)", sep="\t"))
  print(overall_length[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Constant
  cat(paste("(Constant_OUT)", sep="\t"))
  print(constant[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Label
  cat(paste("(Label_OUT)", sep="\t"))
  print(label[2,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
}

# PEOPLE GROUP 3
for(i in 1:nrow(people_group_3)){
  cat(paste("name: topUnModified_C_group_3_", i, "\n", sep=""))
  # INPUT
  cat(paste("I:", "\n", sep="\t"))
  
  # Person
  cat(paste("(Person_IN)", sep="\t"))
  print(people_group_3[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Clothing Item
  cat(paste("(Clothing_Item_IN)", sep="\t"))
  print(clothing_item_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Sleeve Length
  cat(paste("(Sleeve_Length_IN)", sep="\t"))
  print(sleeve_length_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Overall Length
  cat(paste("(Overall_Length_IN)", sep="\t"))
  print(overall_length_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Constant
  cat(paste("(Constant_IN)", sep="\t"))
  print(constant_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Label
  cat(paste("(Label_IN)", sep="\t"))
  print(label[1,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # OUTPUT
  cat(paste("T:", "\n", sep="\t"))
  
  # Person
  cat(paste("(Person_OUT)", sep="\t"))
  print(people_group_3[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Clothing Item
  cat(paste("(Clothing_Item_OUT)", sep="\t"))
  print(clothing_item_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Sleeve Length
  cat(paste("(Sleeve_Length_OUT)", sep="\t"))
  print(sleeve_length_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Overall Length
  cat(paste("(Overall_Length_OUT)", sep="\t"))
  print(overall_length_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Constant
  cat(paste("(Constant_OUT)", sep="\t"))
  print(constant_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Label
  cat(paste("(Label_OUT)", sep="\t"))
  print(label[1,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
}



# PEOPLE GROUP 4
for(i in 1:nrow(people_group_4)){
  cat(paste("name: topUnModified_D_group_4_", i, "\n", sep=""))
  # INPUT
  cat(paste("I:", "\n", sep="\t"))
  
  # Person
  cat(paste("(Person_IN)", sep="\t"))
  print(people_group_4[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Clothing Item
  cat(paste("(Clothing_Item_IN)", sep="\t"))
  print(clothing_item_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Sleeve Length
  cat(paste("(Sleeve_Length_IN)", sep="\t"))
  print(sleeve_length_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Overall Length
  cat(paste("(Overall_Length_IN)", sep="\t"))
  print(overall_length_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Constant
  cat(paste("(Constant_IN)", sep="\t"))
  print(constant_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Label
  cat(paste("(Label_IN)", sep="\t"))
  print(label[1,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # OUTPUT
  cat(paste("T:", "\n", sep="\t"))
  
  # Person
  cat(paste("(Person_OUT)", sep="\t"))
  print(people_group_4[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Clothing Item
  cat(paste("(Clothing_Item_OUT)", sep="\t"))
  print(clothing_item_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Sleeve Length
  cat(paste("(Sleeve_Length_OUT)", sep="\t"))
  print(sleeve_length_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Overall Length
  cat(paste("(Overall_Length_OUT)", sep="\t"))
  print(overall_length_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Constant
  cat(paste("(Constant_OUT)", sep="\t"))
  print(constant_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Label
  cat(paste("(Label_OUT)", sep="\t"))
  print(label[1,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
}

# BLOCK 3
# BLOCK 1
for(i in 1:nrow(people_group_1)){
  cat(paste("name: topModified_A_group_1_", i, "\n", sep=""))
  cat(
    paste0(
      "proc: {setLinkValues learningRate $::lr_salient -type salientFeature \n",
      "setLinkValues learningRate $::lr_nonsalient -type nonsalientFeature \n",
      "setLinkValues weightDecay $::wd_salient -type salientFeature \n",
      "setLinkValues weightDecay $::wd_nonsalient -type nonsalientFeature}",
      "\n"
    )
  )   
  # INPUT
  cat(paste("I:", "\n", sep="\t"))
  
  # Person
  cat(paste("(Person_IN)", sep="\t"))
  print(people_group_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Clothing Item
  cat(paste("(Clothing_Item_IN)", sep="\t"))
  print(clothing_item[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Sleeve Length
  cat(paste("(Sleeve_Length_IN)", sep="\t"))
  print(sleeve_length[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Overall Length
  cat(paste("(Overall_Length_IN)", sep="\t"))
  print(overall_length[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Constant
  cat(paste("(Constant_IN)", sep="\t"))
  print(constant[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  cat(paste("(Label_IN)", sep="\t"))
  print(label[2,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # OUTPUT
  cat(paste("T:", "\n", sep="\t"))
  
  # Person
  cat(paste("(Person_OUT)", sep="\t"))
  print(people_group_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Clothing Item
  cat(paste("(Clothing_Item_OUT)", sep="\t"))
  print(clothing_item[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Sleeve Length
  cat(paste("(Sleeve_Length_OUT)", sep="\t"))
  print(sleeve_length[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Overall Length
  cat(paste("(Overall_Length_OUT)", sep="\t"))
  print(overall_length[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Constant
  cat(paste("(Constant_OUT)", sep="\t"))
  print(constant[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Label
  cat(paste("(Label_OUT)", sep="\t"))
  print(label[2,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
}


# PEOPLE GROUP 2
for(i in 1:nrow(people_group_2)){
  cat(paste("name: topModified_B_group_2_", i, "\n", sep=""))
  cat(
    paste0(
      "proc: {setLinkValues learningRate $::lr_salient -type salientFeature \n",
      "setLinkValues learningRate $::lr_nonsalient -type nonsalientFeature \n",
      "setLinkValues weightDecay $::wd_salient -type salientFeature \n",
      "setLinkValues weightDecay $::wd_nonsalient -type nonsalientFeature}",
      "\n"
    )
  )    
  # INPUT
  cat(paste("I:", "\n", sep="\t"))
  
  # Person
  cat(paste("(Person_IN)", sep="\t"))
  print(people_group_2[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Clothing Item
  cat(paste("(Clothing_Item_IN)", sep="\t"))
  print(clothing_item[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Sleeve Length
  cat(paste("(Sleeve_Length_IN)", sep="\t"))
  print(sleeve_length[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Overall Length
  cat(paste("(Overall_Length_IN)", sep="\t"))
  print(overall_length[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Constant
  cat(paste("(Constant_IN)", sep="\t"))
  print(constant[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Label
  cat(paste("(Label_IN)", sep="\t"))
  print(label[2,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # OUTPUT
  cat(paste("T:", "\n", sep="\t"))
  
  # Person
  cat(paste("(Person_OUT)", sep="\t"))
  print(people_group_2[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Clothing Item
  cat(paste("(Clothing_Item_OUT)", sep="\t"))
  print(clothing_item[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Sleeve Length
  cat(paste("(Sleeve_Length_OUT)", sep="\t"))
  print(sleeve_length[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Overall Length
  cat(paste("(Overall_Length_OUT)", sep="\t"))
  print(overall_length[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Constant
  cat(paste("(Constant_OUT)", sep="\t"))
  print(constant[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Label
  cat(paste("(Label_OUT)", sep="\t"))
  print(label[2,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
}

# PEOPLE GROUP 3
for(i in 1:nrow(people_group_3)){
  cat(paste("name: topUnModified_C_group_3_", i, "\n", sep="")) 
  # INPUT
  cat(paste("I:", "\n", sep="\t"))
  
  # Person
  cat(paste("(Person_IN)", sep="\t"))
  print(people_group_3[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Clothing Item
  cat(paste("(Clothing_Item_IN)", sep="\t"))
  print(clothing_item_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Sleeve Length
  cat(paste("(Sleeve_Length_IN)", sep="\t"))
  print(sleeve_length_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Overall Length
  cat(paste("(Overall_Length_IN)", sep="\t"))
  print(overall_length_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Constant
  cat(paste("(Constant_IN)", sep="\t"))
  print(constant_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Label
  cat(paste("(Label_IN)", sep="\t"))
  print(label[1,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # OUTPUT
  cat(paste("T:", "\n", sep="\t"))
  
  # Person
  cat(paste("(Person_OUT)", sep="\t"))
  print(people_group_3[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Clothing Item
  cat(paste("(Clothing_Item_OUT)", sep="\t"))
  print(clothing_item_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Sleeve Length
  cat(paste("(Sleeve_Length_OUT)", sep="\t"))
  print(sleeve_length_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Overall Length
  cat(paste("(Overall_Length_OUT)", sep="\t"))
  print(overall_length_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Constant
  cat(paste("(Constant_OUT)", sep="\t"))
  print(constant_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Label
  cat(paste("(Label_OUT)", sep="\t"))
  print(label[1,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
}



# PEOPLE GROUP 4
for(i in 1:nrow(people_group_4)){
  cat(paste("name: topUnModified_D_group_4_", i, "\n", sep=""))
  # INPUT
  cat(paste("I:", "\n", sep="\t"))
  
  # Person
  cat(paste("(Person_IN)", sep="\t"))
  print(people_group_4[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Clothing Item
  cat(paste("(Clothing_Item_IN)", sep="\t"))
  print(clothing_item_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Sleeve Length
  cat(paste("(Sleeve_Length_IN)", sep="\t"))
  print(sleeve_length_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Overall Length
  cat(paste("(Overall_Length_IN)", sep="\t"))
  print(overall_length_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Constant
  cat(paste("(Constant_IN)", sep="\t"))
  print(constant_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Label
  cat(paste("(Label_IN)", sep="\t"))
  print(label[1,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # OUTPUT
  cat(paste("T:", "\n", sep="\t"))
  
  # Person
  cat(paste("(Person_OUT)", sep="\t"))
  print(people_group_4[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Clothing Item
  cat(paste("(Clothing_Item_OUT)", sep="\t"))
  print(clothing_item_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Sleeve Length
  cat(paste("(Sleeve_Length_OUT)", sep="\t"))
  print(sleeve_length_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Overall Length
  cat(paste("(Overall_Length_OUT)", sep="\t"))
  print(overall_length_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Constant
  cat(paste("(Constant_OUT)", sep="\t"))
  print(constant_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Label
  cat(paste("(Label_OUT)", sep="\t"))
  print(label[1,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
}

# BLOCK 4
# BLOCK 1
for(i in 1:nrow(people_group_1)){
  cat(paste("name: topModified_A_group_1_", i, "\n", sep=""))
  cat(
    paste0(
      "proc: {setLinkValues learningRate $::lr_salient -type salientFeature \n",
      "setLinkValues learningRate $::lr_nonsalient -type nonsalientFeature \n",
      "setLinkValues weightDecay $::wd_salient -type salientFeature \n",
      "setLinkValues weightDecay $::wd_nonsalient -type nonsalientFeature}",
      "\n"
    )
  )  
  # INPUT
  cat(paste("I:", "\n", sep="\t"))
  
  # Person
  cat(paste("(Person_IN)", sep="\t"))
  print(people_group_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Clothing Item
  cat(paste("(Clothing_Item_IN)", sep="\t"))
  print(clothing_item[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Sleeve Length
  cat(paste("(Sleeve_Length_IN)", sep="\t"))
  print(sleeve_length[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Overall Length
  cat(paste("(Overall_Length_IN)", sep="\t"))
  print(overall_length[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Constant
  cat(paste("(Constant_IN)", sep="\t"))
  print(constant[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Label
  cat(paste("(Label_IN)", sep="\t"))
  print(label[2,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # OUTPUT
  cat(paste("T:", "\n", sep="\t"))
  
  # Person
  cat(paste("(Person_OUT)", sep="\t"))
  print(people_group_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Clothing Item
  cat(paste("(Clothing_Item_OUT)", sep="\t"))
  print(clothing_item[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Sleeve Length
  cat(paste("(Sleeve_Length_OUT)", sep="\t"))
  print(sleeve_length[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Overall Length
  cat(paste("(Overall_Length_OUT)", sep="\t"))
  print(overall_length[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Constant
  cat(paste("(Constant_OUT)", sep="\t"))
  print(constant[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Label
  cat(paste("(Label_OUT)", sep="\t"))
  print(label[2,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
}


# PEOPLE GROUP 2
for(i in 1:nrow(people_group_2)){
  cat(paste("name: topModified_B_group_2_", i, "\n", sep=""))
  cat(
    paste0(
      "proc: {setLinkValues learningRate $::lr_salient -type salientFeature \n",
      "setLinkValues learningRate $::lr_nonsalient -type nonsalientFeature \n",
      "setLinkValues weightDecay $::wd_salient -type salientFeature \n",
      "setLinkValues weightDecay $::wd_nonsalient -type nonsalientFeature}",
      "\n"
    )
  )    
  # INPUT
  cat(paste("I:", "\n", sep="\t"))
  
  # Person
  cat(paste("(Person_IN)", sep="\t"))
  print(people_group_2[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Clothing Item
  cat(paste("(Clothing_Item_IN)", sep="\t"))
  print(clothing_item[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Sleeve Length
  cat(paste("(Sleeve_Length_IN)", sep="\t"))
  print(sleeve_length[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Overall Length
  cat(paste("(Overall_Length_IN)", sep="\t"))
  print(overall_length[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Constant
  cat(paste("(Constant_IN)", sep="\t"))
  print(constant[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Label
  cat(paste("(Label_IN)", sep="\t"))
  print(label[2,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # OUTPUT
  cat(paste("T:", "\n", sep="\t"))
  
  # Person
  cat(paste("(Person_OUT)", sep="\t"))
  print(people_group_2[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Clothing Item
  cat(paste("(Clothing_Item_OUT)", sep="\t"))
  print(clothing_item[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Sleeve Length
  cat(paste("(Sleeve_Length_OUT)", sep="\t"))
  print(sleeve_length[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Overall Length
  cat(paste("(Overall_Length_OUT)", sep="\t"))
  print(overall_length[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Constant
  cat(paste("(Constant_OUT)", sep="\t"))
  print(constant[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Label
  cat(paste("(Label_OUT)", sep="\t"))
  print(label[2,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
}

# PEOPLE GROUP 3
for(i in 1:nrow(people_group_3)){
  cat(paste("name: topUnModified_C_group_3_", i, "\n", sep=""))
  # INPUT
  cat(paste("I:", "\n", sep="\t"))
  
  # Person
  cat(paste("(Person_IN)", sep="\t"))
  print(people_group_3[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Clothing Item
  cat(paste("(Clothing_Item_IN)", sep="\t"))
  print(clothing_item_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Sleeve Length
  cat(paste("(Sleeve_Length_IN)", sep="\t"))
  print(sleeve_length_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Overall Length
  cat(paste("(Overall_Length_IN)", sep="\t"))
  print(overall_length_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Constant
  cat(paste("(Constant_IN)", sep="\t"))
  print(constant_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Label
  cat(paste("(Label_IN)", sep="\t"))
  print(label[1,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # OUTPUT
  cat(paste("T:", "\n", sep="\t"))
  
  # Person
  cat(paste("(Person_OUT)", sep="\t"))
  print(people_group_3[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Clothing Item
  cat(paste("(Clothing_Item_OUT)", sep="\t"))
  print(clothing_item_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Sleeve Length
  cat(paste("(Sleeve_Length_OUT)", sep="\t"))
  print(sleeve_length_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Overall Length
  cat(paste("(Overall_Length_OUT)", sep="\t"))
  print(overall_length_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Constant
  cat(paste("(Constant_OUT)", sep="\t"))
  print(constant_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Label
  cat(paste("(Label_OUT)", sep="\t"))
  print(label[1,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
}



# PEOPLE GROUP 4
for(i in 1:nrow(people_group_4)){
  cat(paste("name: topUnModified_D_group_4_", i, "\n", sep="")) 
  # INPUT
  cat(paste("I:", "\n", sep="\t"))
  
  # Person
  cat(paste("(Person_IN)", sep="\t"))
  print(people_group_4[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Clothing Item
  cat(paste("(Clothing_Item_IN)", sep="\t"))
  print(clothing_item_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Sleeve Length
  cat(paste("(Sleeve_Length_IN)", sep="\t"))
  print(sleeve_length_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Overall Length
  cat(paste("(Overall_Length_IN)", sep="\t"))
  print(overall_length_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Constant
  cat(paste("(Constant_IN)", sep="\t"))
  print(constant_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Label
  cat(paste("(Label_IN)", sep="\t"))
  print(label[1,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # OUTPUT
  cat(paste("T:", "\n", sep="\t"))
  
  # Person
  cat(paste("(Person_OUT)", sep="\t"))
  print(people_group_4[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Clothing Item
  cat(paste("(Clothing_Item_OUT)", sep="\t"))
  print(clothing_item_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Sleeve Length
  cat(paste("(Sleeve_Length_OUT)", sep="\t"))
  print(sleeve_length_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Overall Length
  cat(paste("(Overall_Length_OUT)", sep="\t"))
  print(overall_length_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Constant
  cat(paste("(Constant_OUT)", sep="\t"))
  print(constant_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Label
  cat(paste("(Label_OUT)", sep="\t"))
  print(label[1,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
}
sink()





#########
## TEST #
#########

sink('testing.ex')
cat(paste("defI:-", "\n", sep="\t"))
cat(paste("defT:-", "\n", sep="\t"))
cat(paste(";", "\n", sep="\t"))
# PEOPLE GROUP 1 - TOP NAMED MATCH, NAMED MISMATCH, NEITHER
for(i in 1:nrow(people_group_1)){
  cat(paste("name: topModified_A_named_match_group_1_", i, "\n", sep=""))
  # INPUT
  cat(paste("I:", "\n", sep="\t"))
  
  # Person
  cat(paste("(Person_IN)", sep="\t"))
  print(people_group_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Clothing Item
  cat(paste("(Clothing_Item_IN)", sep="\t"))
  print(clothing_item[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Sleeve Length
  cat(paste("(Sleeve_Length_IN)", sep="\t"))
  print(sleeve_length[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Overall Length
  cat(paste("(Overall_Length_IN)", sep="\t"))
  print(overall_shifted[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Constant
  cat(paste("(Constant_IN)", sep="\t"))
  print(constant[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # OUTPUT
  cat(paste("T:", "\n", sep="\t"))
  
  # Person
  cat(paste("(Person_OUT)", sep="\t"))
  print(people_group_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Clothing Item
  cat(paste("(Clothing_Item_OUT)", sep="\t"))
  print(clothing_item[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Sleeve Length
  cat(paste("(Sleeve_Length_OUT)", sep="\t"))
  print(sleeve_length[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Overall Length
  cat(paste("(Overall_Length_OUT)", sep="\t"))
  print(overall_shifted[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Constant
  cat(paste("(Constant_OUT)", sep="\t"))
  print(constant[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
}


for(i in 1:nrow(people_group_1)){
  cat(paste("name: topModified_A_named_mismatch_group_1_", i, "\n", sep=""))
  # INPUT
  cat(paste("I:", "\n", sep="\t"))
  
  # Person
  cat(paste("(Person_IN)", sep="\t"))
  print(people_group_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Clothing Item
  cat(paste("(Clothing_Item_IN)", sep="\t"))
  print(clothing_item[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Sleeve Length
  cat(paste("(Sleeve_Length_IN)", sep="\t"))
  print(sleeve_shifted[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Overall Length
  cat(paste("(Overall_Length_IN)", sep="\t"))
  print(overall_length[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Constant
  cat(paste("(Constant_IN)", sep="\t"))
  print(constant[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # OUTPUT
  cat(paste("T:", "\n", sep="\t"))
  
  # Person
  cat(paste("(Person_OUT)", sep="\t"))
  print(people_group_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Clothing Item
  cat(paste("(Clothing_Item_OUT)", sep="\t"))
  print(clothing_item[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Sleeve Length
  cat(paste("(Sleeve_Length_OUT)", sep="\t"))
  print(sleeve_shifted[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Overall Length
  cat(paste("(Overall_Length_OUT)", sep="\t"))
  print(overall_length[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Constant
  cat(paste("(Constant_OUT)", sep="\t"))
  print(constant[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
}

for(i in 1:nrow(people_group_1)){
  cat(paste("name: topModified_A_neither_group_1_", i, "\n", sep=""))
  # INPUT
  cat(paste("I:", "\n", sep="\t"))
  
  # Person
  cat(paste("(Person_IN)", sep="\t"))
  print(people_group_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Clothing Item
  cat(paste("(Clothing_Item_IN)", sep="\t"))
  print(clothing_item[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Sleeve Length
  cat(paste("(Sleeve_Length_IN)", sep="\t"))
  print(sleeve_shifted[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Overall Length
  cat(paste("(Overall_Length_IN)", sep="\t"))
  print(overall_shifted[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Constant
  cat(paste("(Constant_IN)", sep="\t"))
  print(constant[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # OUTPUT
  cat(paste("T:", "\n", sep="\t"))
  
  # Person
  cat(paste("(Person_OUT)", sep="\t"))
  print(people_group_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Clothing Item
  cat(paste("(Clothing_Item_OUT)", sep="\t"))
  print(clothing_item[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Sleeve Length
  cat(paste("(Sleeve_Length_OUT)", sep="\t"))
  print(sleeve_shifted[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Overall Length
  cat(paste("(Overall_Length_OUT)", sep="\t"))
  print(overall_shifted[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Constant
  cat(paste("(Constant_OUT)", sep="\t"))
  print(constant[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
}



for(i in 1:nrow(people_group_2)){
  cat(paste("name: topModified_B_named_match_group_2_", i, "\n", sep=""))
  # INPUT
  cat(paste("I:", "\n", sep="\t"))
  
  # Person
  cat(paste("(Person_IN)", sep="\t"))
  print(people_group_2[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Clothing Item
  cat(paste("(Clothing_Item_IN)", sep="\t"))
  print(clothing_item[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Sleeve Length
  cat(paste("(Sleeve_Length_IN)", sep="\t"))
  print(sleeve_length[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Overall Length
  cat(paste("(Overall_Length_IN)", sep="\t"))
  print(overall_shifted[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Constant
  cat(paste("(Constant_IN)", sep="\t"))
  print(constant[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # OUTPUT
  cat(paste("T:", "\n", sep="\t"))
  
  # Person
  cat(paste("(Person_OUT)", sep="\t"))
  print(people_group_2[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Clothing Item
  cat(paste("(Clothing_Item_OUT)", sep="\t"))
  print(clothing_item[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Sleeve Length
  cat(paste("(Sleeve_Length_OUT)", sep="\t"))
  print(sleeve_length[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Overall Length
  cat(paste("(Overall_Length_OUT)", sep="\t"))
  print(overall_shifted[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Constant
  cat(paste("(Constant_OUT)", sep="\t"))
  print(constant[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
}


for(i in 1:nrow(people_group_2)){
  cat(paste("name: topModified_B_named_mismatch_group_2_", i, "\n", sep=""))
  # INPUT
  cat(paste("I:", "\n", sep="\t"))
  
  # Person
  cat(paste("(Person_IN)", sep="\t"))
  print(people_group_2[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Clothing Item
  cat(paste("(Clothing_Item_IN)", sep="\t"))
  print(clothing_item[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Sleeve Length
  cat(paste("(Sleeve_Length_IN)", sep="\t"))
  print(sleeve_shifted[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Overall Length
  cat(paste("(Overall_Length_IN)", sep="\t"))
  print(overall_length[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Constant
  cat(paste("(Constant_IN)", sep="\t"))
  print(constant[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # OUTPUT
  cat(paste("T:", "\n", sep="\t"))
  
  # Person
  cat(paste("(Person_OUT)", sep="\t"))
  print(people_group_2[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Clothing Item
  cat(paste("(Clothing_Item_OUT)", sep="\t"))
  print(clothing_item[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Sleeve Length
  cat(paste("(Sleeve_Length_OUT)", sep="\t"))
  print(sleeve_shifted[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Overall Length
  cat(paste("(Overall_Length_OUT)", sep="\t"))
  print(overall_length[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Constant
  cat(paste("(Constant_OUT)", sep="\t"))
  print(constant[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
}

for(i in 1:nrow(people_group_2)){
  cat(paste("name: topModified_B_neither_group_2_", i, "\n", sep=""))
  # INPUT
  cat(paste("I:", "\n", sep="\t"))
  
  # Person
  cat(paste("(Person_IN)", sep="\t"))
  print(people_group_2[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Clothing Item
  cat(paste("(Clothing_Item_IN)", sep="\t"))
  print(clothing_item[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Sleeve Length
  cat(paste("(Sleeve_Length_IN)", sep="\t"))
  print(sleeve_shifted[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Overall Length
  cat(paste("(Overall_Length_IN)", sep="\t"))
  print(overall_shifted[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Constant
  cat(paste("(Constant_IN)", sep="\t"))
  print(constant[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # OUTPUT
  cat(paste("T:", "\n", sep="\t"))
  
  # Person
  cat(paste("(Person_OUT)", sep="\t"))
  print(people_group_2[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Clothing Item
  cat(paste("(Clothing_Item_OUT)", sep="\t"))
  print(clothing_item[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Sleeve Length
  cat(paste("(Sleeve_Length_OUT)", sep="\t"))
  print(sleeve_shifted[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Overall Length
  cat(paste("(Overall_Length_OUT)", sep="\t"))
  print(overall_shifted[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Constant
  cat(paste("(Constant_OUT)", sep="\t"))
  print(constant[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
}



for(i in 1:nrow(people_group_3)){
  cat(paste("name: topUnModified_C_named_match_group_3_", i, "\n", sep=""))
  # INPUT
  cat(paste("I:", "\n", sep="\t"))
  
  # Person
  cat(paste("(Person_IN)", sep="\t"))
  print(people_group_3[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Clothing Item
  cat(paste("(Clothing_Item_IN)", sep="\t"))
  print(clothing_item_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Sleeve Length
  cat(paste("(Sleeve_Length_IN)", sep="\t"))
  print(sleeve_length_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Overall Length
  cat(paste("(Overall_Length_IN)", sep="\t"))
  print(overall_shifted_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Constant
  cat(paste("(Constant_IN)", sep="\t"))
  print(constant_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # OUTPUT
  cat(paste("T:", "\n", sep="\t"))
  
  # Person
  cat(paste("(Person_OUT)", sep="\t"))
  print(people_group_3[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Clothing Item
  cat(paste("(Clothing_Item_OUT)", sep="\t"))
  print(clothing_item_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Sleeve Length
  cat(paste("(Sleeve_Length_OUT)", sep="\t"))
  print(sleeve_length_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Overall Length
  cat(paste("(Overall_Length_OUT)", sep="\t"))
  print(overall_shifted_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Constant
  cat(paste("(Constant_OUT)", sep="\t"))
  print(constant_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
}


for(i in 1:nrow(people_group_3)){
  cat(paste("name: topUnModified_C_named_mismatch_group_3_", i, "\n", sep=""))
  # INPUT
  cat(paste("I:", "\n", sep="\t"))
  
  # Person
  cat(paste("(Person_IN)", sep="\t"))
  print(people_group_3[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Clothing Item
  cat(paste("(Clothing_Item_IN)", sep="\t"))
  print(clothing_item_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Sleeve Length
  cat(paste("(Sleeve_Length_IN)", sep="\t"))
  print(sleeve_shifted_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Overall Length
  cat(paste("(Overall_Length_IN)", sep="\t"))
  print(overall_length_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Constant
  cat(paste("(Constant_IN)", sep="\t"))
  print(constant_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # OUTPUT
  cat(paste("T:", "\n", sep="\t"))
  
  # Person
  cat(paste("(Person_OUT)", sep="\t"))
  print(people_group_3[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Clothing Item
  cat(paste("(Clothing_Item_OUT)", sep="\t"))
  print(clothing_item_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Sleeve Length
  cat(paste("(Sleeve_Length_OUT)", sep="\t"))
  print(sleeve_shifted_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Overall Length
  cat(paste("(Overall_Length_OUT)", sep="\t"))
  print(overall_length_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Constant
  cat(paste("(Constant_OUT)", sep="\t"))
  print(constant_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
}

for(i in 1:nrow(people_group_3)){
  cat(paste("name: topUnModified_C_neither_group_3_", i, "\n", sep=""))
  # INPUT
  cat(paste("I:", "\n", sep="\t"))
  
  # Person
  cat(paste("(Person_IN)", sep="\t"))
  print(people_group_3[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Clothing Item
  cat(paste("(Clothing_Item_IN)", sep="\t"))
  print(clothing_item_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Sleeve Length
  cat(paste("(Sleeve_Length_IN)", sep="\t"))
  print(sleeve_shifted_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Overall Length
  cat(paste("(Overall_Length_IN)", sep="\t"))
  print(overall_shifted_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Constant
  cat(paste("(Constant_IN)", sep="\t"))
  print(constant_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # OUTPUT
  cat(paste("T:", "\n", sep="\t"))
  
  # Person
  cat(paste("(Person_OUT)", sep="\t"))
  print(people_group_3[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Clothing Item
  cat(paste("(Clothing_Item_OUT)", sep="\t"))
  print(clothing_item_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Sleeve Length
  cat(paste("(Sleeve_Length_OUT)", sep="\t"))
  print(sleeve_shifted_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Overall Length
  cat(paste("(Overall_Length_OUT)", sep="\t"))
  print(overall_shifted_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Constant
  cat(paste("(Constant_OUT)", sep="\t"))
  print(constant_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
}



for(i in 1:nrow(people_group_4)){
  cat(paste("name: topUnModified_D_named_match_group_4_", i, "\n", sep=""))
  # INPUT
  cat(paste("I:", "\n", sep="\t"))
  
  # Person
  cat(paste("(Person_IN)", sep="\t"))
  print(people_group_4[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Clothing Item
  cat(paste("(Clothing_Item_IN)", sep="\t"))
  print(clothing_item_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Sleeve Length
  cat(paste("(Sleeve_Length_IN)", sep="\t"))
  print(sleeve_length_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Overall Length
  cat(paste("(Overall_Length_IN)", sep="\t"))
  print(overall_shifted_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Constant
  cat(paste("(Constant_IN)", sep="\t"))
  print(constant_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # OUTPUT
  cat(paste("T:", "\n", sep="\t"))
  
  # Person
  cat(paste("(Person_OUT)", sep="\t"))
  print(people_group_4[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Clothing Item
  cat(paste("(Clothing_Item_OUT)", sep="\t"))
  print(clothing_item_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Sleeve Length
  cat(paste("(Sleeve_Length_OUT)", sep="\t"))
  print(sleeve_length_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Overall Length
  cat(paste("(Overall_Length_OUT)", sep="\t"))
  print(overall_shifted_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Constant
  cat(paste("(Constant_OUT)", sep="\t"))
  print(constant_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
}


for(i in 1:nrow(people_group_4)){
  cat(paste("name: topUnModified_D_named_mismatch_group_4_", i, "\n", sep=""))
  # INPUT
  cat(paste("I:", "\n", sep="\t"))
  
  # Person
  cat(paste("(Person_IN)", sep="\t"))
  print(people_group_4[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Clothing Item
  cat(paste("(Clothing_Item_IN)", sep="\t"))
  print(clothing_item_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Sleeve Length
  cat(paste("(Sleeve_Length_IN)", sep="\t"))
  print(sleeve_shifted_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Overall Length
  cat(paste("(Overall_Length_IN)", sep="\t"))
  print(overall_length_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Constant
  cat(paste("(Constant_IN)", sep="\t"))
  print(constant_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # OUTPUT
  cat(paste("T:", "\n", sep="\t"))
  
  # Person
  cat(paste("(Person_OUT)", sep="\t"))
  print(people_group_4[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Clothing Item
  cat(paste("(Clothing_Item_OUT)", sep="\t"))
  print(clothing_item_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Sleeve Length
  cat(paste("(Sleeve_Length_OUT)", sep="\t"))
  print(sleeve_shifted_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Overall Length
  cat(paste("(Overall_Length_OUT)", sep="\t"))
  print(overall_length_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Constant
  cat(paste("(Constant_OUT)", sep="\t"))
  print(constant_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
}

for(i in 1:nrow(people_group_4)){
  cat(paste("name: topUnModified_D_neither_group_4_", i, "\n", sep=""))
  # INPUT
  cat(paste("I:", "\n", sep="\t"))
  
  # Person
  cat(paste("(Person_IN)", sep="\t"))
  print(people_group_4[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Clothing Item
  cat(paste("(Clothing_Item_IN)", sep="\t"))
  print(clothing_item_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Sleeve Length
  cat(paste("(Sleeve_Length_IN)", sep="\t"))
  print(sleeve_shifted_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Overall Length
  cat(paste("(Overall_Length_IN)", sep="\t"))
  print(overall_shifted_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Constant
  cat(paste("(Constant_IN)", sep="\t"))
  print(constant_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # OUTPUT
  cat(paste("T:", "\n", sep="\t"))
  
  # Person
  cat(paste("(Person_OUT)", sep="\t"))
  print(people_group_4[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Clothing Item
  cat(paste("(Clothing_Item_OUT)", sep="\t"))
  print(clothing_item_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Sleeve Length
  cat(paste("(Sleeve_Length_OUT)", sep="\t"))
  print(sleeve_shifted_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Overall Length
  cat(paste("(Overall_Length_OUT)", sep="\t"))
  print(overall_shifted_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Constant
  cat(paste("(Constant_OUT)", sep="\t"))
  print(constant_unmodified[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
}
sink()