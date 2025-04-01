#table data for country
table(Atlas_Data$`Country (listed on deposited data)`)
#table data for og histology 
table(Atlas_Data$`Original histology`)
#unique og histology - 67
length(unique(Atlas_Data$`Original histology`))
#Diagnosis 
table(Atlas_Data$Diagnosis)
length(unique(Atlas_Data$Diagnosis))
