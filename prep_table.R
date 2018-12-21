library(googlesheets)
library(tidyverse)
#library(stringr)
library(dplyr)
library(data.table)
#library(DescTools)
library(tidyr)
library(xlsx)

## This is junk now :(   James was speedy with it   Thank James for James 

# Authenticate google
gs_auth(new_user = TRUE)

# Check available sheets
gs_ls()
available <- gs_ls()

# Read in the art extracted spreadsheet
str.connect <- gs_title("DND Monitored Ownership Units for STR")
dnd.house <- gs_read(str.connect)

dnd.address <- unite(dnd.house, Address, 
             c("Unit Street #", "Unit Street Name", "Unit ZIP"), 
             sep = " ", 
             remove = FALSE)

write.xlsx(dnd.address, "dnd_addresses.xlsx")


dnd.address.comma <- unite(dnd.house, Address, 
                     c("Unit Street #", "Unit Street Name", "Unit ZIP"), 
                     sep = ", ", 
                     remove = FALSE)

write.xlsx(dnd.address.comma, "dnd_addresses_comma.xlsx")

dnd.sam <- read.xlsx('dnd_all.xlsx', 1)
dnd.sam.good <- subset(dnd.sam, dnd.sam$Score > 1)

# 2nd geocode run
dnd.geo2 <- read.csv('dnd_geo_2ndrun.csv')
dnd.geo2 <- dnd.geo2[1:81,]
dnd.geo2.good <- subset(dnd.geo2, dnd.geo2$Match.Score > 4)
dnd.geo2.bad <- subset(dnd.geo2, dnd.geo2$Match.Score < 4)

# Pull out relevant columns from dnd.sam.good
dnd.address.geo <- data.table(dnd.sam.good$Project__Project_Name, 
                              dnd.sam.good$Project_Unit__Unit_Name,
                              dnd.sam.good$Unit_Street__,
                              dnd.sam.good$Unit_Street_Name,
                              dnd.sam.good$Unit__,
                              dnd.sam.good$Unit_ZIP,
                              dnd.sam.good$Parcel_Info,
                              dnd.sam.good$Ownership_Units,
                              dnd.sam.good$Subtotal_Restricted_Units__fm_Prjt_,
                              dnd.sam.good$Ref_ID,
                              dnd.sam.good$Address)
colnames(dnd.address.geo) <- c('Project: Project Name', 'Project Unit: Unit_Name',
             'Unit Street #', 'Unit Street Name', 'Unit #', 'Unit ZIP',
             'Parcel Info', 'Ownership Units', 'Subtotal Restricted Units (fm_Prjt)',
             'SAM ID', 'Address')

# Pull out relevant columns from geo2 good
dnd.geo2.good.rdy <- data.table(dnd.geo2.good$Project__Project_Name, 
                                dnd.geo2.good$Project_Unit__Unit_Name,
                                dnd.geo2.good$Unit_Street__,
                                dnd.geo2.good$Unit_Street_Name,
                                dnd.geo2.good$Unit__,
                                dnd.geo2.good$Unit_ZIP,
                                dnd.geo2.good$Parcel_Info,
                                dnd.geo2.good$Ownership_Units,
                                dnd.geo2.good$Subtotal_Restricted_Units__fm_Prjt_,
                                dnd.geo2.good$Match.Id,
                                dnd.geo2.good$Address)
colnames(dnd.geo2.good.rdy) <- c('Project: Project Name', 'Project Unit: Unit_Name',
                               'Unit Street #', 'Unit Street Name', 'Unit #', 'Unit ZIP',
                               'Parcel Info', 'Ownership Units', 'Subtotal Restricted Units (fm_Prjt)',
                               'SAM ID', 'Address')

# Bind with dnd.adress.geo masterlist
dnd.address.geo <- rbind(dnd.address.geo, dnd.geo2.good.rdy)

write.xlsx(dnd.address.geo, "DND_Monitored_Ownership_Units_for_STR_SAMID.xlsx", row.names = FALSE)
write.xlsx(dnd.geo2.bad, "DND_nomatch_with_SAM.xlsx", row.names = FALSE)

rm(dnd.geo2)
rm(dnd.geo2.good)

# 3rd geocode run
dnd.geo3 <- read.csv("dnd_geo_3rdrun.csv")
dnd.geo3.good <- subset(dnd.geo3, dnd.geo3$Match.Score > 1)

dnd.geo3.good.rdy <- data.table(dnd.geo3.good$Project__Project_Name, 
                                dnd.geo3.good$Project_Unit__Unit_Name,
                                dnd.geo3.good$Unit_Street__,
                                dnd.geo3.good$Unit_Street_Name,
                                dnd.geo3.good$Unit__,
                                dnd.geo3.good$Unit_ZIP,
                                dnd.geo3.good$Parcel_Info,
                                dnd.geo3.good$Ownership_Units,
                                dnd.geo3.good$Subtotal_Restricted_Units__fm_Prjt_,
                                dnd.geo3.good$Match.Id,
                                dnd.geo3.good$Address)
colnames(dnd.geo3.good.rdy) <- c('Project: Project Name', 'Project Unit: Unit_Name',
                                 'Unit Street #', 'Unit Street Name', 'Unit #', 'Unit ZIP',
                                 'Parcel Info', 'Ownership Units', 'Subtotal Restricted Units (fm_Prjt)',
                                 'SAM ID', 'Address')

# Bind with dnd.address.geo masterlist
dnd.address.geo <- rbind(dnd.address.geo, dnd.geo3.good.rdy)

# 4th geocode run
dnd.geo4 <- read.csv("dnd_geo_4thrun.csv")
dnd.geo4.good <- subset(dnd.geo4, dnd.geo4$Score > 50)

dnd.geo4.good.rdy <- data.table(dnd.geo4.good$Project__Project_Name, 
                                dnd.geo4.good$Project_Unit__Unit_Name,
                                dnd.geo4.good$Unit_Street__,
                                dnd.geo4.good$Unit_Street_Name,
                                dnd.geo4.good$Unit__,
                                dnd.geo4.good$Unit_ZIP,
                                dnd.geo4.good$Parcel_Info,
                                dnd.geo4.good$Ownership_Units,
                                dnd.geo4.good$Subtotal_Restricted_Units__fm_Prjt_,
                                dnd.geo4.good$Ref_ID,
                                dnd.geo4.good$Address1)
colnames(dnd.geo4.good.rdy) <- c('Project: Project Name', 'Project Unit: Unit_Name',
                                 'Unit Street #', 'Unit Street Name', 'Unit #', 'Unit ZIP',
                                 'Parcel Info', 'Ownership Units', 'Subtotal Restricted Units (fm_Prjt)',
                                 'SAM ID', 'Address')

# Bind with dnd.address.geo masterlist
dnd.address.geo <- rbind(dnd.address.geo, dnd.geo4.good.rdy)
write.xlsx(dnd.address.geo, "DND_Monitored_Ownership_Units_for_STR_SAMID.xlsx", row.names = FALSE)

# Make up the unmatched records to attach
dnd.geo4.bad <- subset(dnd.geo4, dnd.geo4$Score < 50)

dnd.geo4.bad.rdy <- data.table(dnd.geo4.bad$Project__Project_Name, 
                                dnd.geo4.bad$Project_Unit__Unit_Name,
                                dnd.geo4.bad$Unit_Street__,
                                dnd.geo4.bad$Unit_Street_Name,
                                dnd.geo4.bad$Unit__,
                                dnd.geo4.bad$Unit_ZIP,
                                dnd.geo4.bad$Parcel_Info,
                                dnd.geo4.bad$Ownership_Units,
                                dnd.geo4.bad$Subtotal_Restricted_Units__fm_Prjt_,
                                dnd.geo4.bad$Ref_ID,
                                dnd.geo4.bad$Address1)
colnames(dnd.geo4.bad.rdy) <- c('Project: Project Name', 'Project Unit: Unit_Name',
                                 'Unit Street #', 'Unit Street Name', 'Unit #', 'Unit ZIP',
                                 'Parcel Info', 'Ownership Units', 'Subtotal Restricted Units (fm_Prjt)',
                                 'SAM ID', 'Address')
dnd.geo4.bad.rdy$`SAM ID` <- NA

# Bind with dnd.address.geo masterlist
dnd.address.geo.all <- rbind(dnd.address.geo, dnd.geo4.bad.rdy)
write.xlsx(dnd.address.geo.all, "DND_Monitored_Ownership_Units_for_STR_SAMID.xlsx", row.names = FALSE)





