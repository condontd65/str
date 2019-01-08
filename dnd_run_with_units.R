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
                     c("Unit Street #", "Unit Street Name", "Unit #", "Unit ZIP"), 
                     sep = " ", 
                     remove = FALSE)

dnd.address <- data.frame(lapply(dnd.address, function(x) {
  gsub("NA ", "", x)
}))

write.xlsx(dnd.address, "w_unit/dnd_addresses_unit.xlsx", row.names = FALSE)


dnd.sam <- read.xlsx('w_unit/dnd_geo1.xlsx', 1)
dnd.sam.good <- subset(dnd.sam, dnd.sam$Score > 1)
dnd.sam.bad <- subset(dnd.sam, dnd.sam$Score < 1)

write.xlsx(dnd.sam.bad, "w_unit/dnd_geo1_failed.xlsx", row.names = FALSE)

# Read in run 2
dnd.sam.2 <- read.csv("w_unit/dnd_geo2.csv")
dnd.sam.good.2 <- subset(dnd.sam.2, dnd.sam.2$Match.Score > 1)
dnd.sam.bad.2 <- subset(dnd.sam.2, dnd.sam.2$Match.Score < 1)

write.xlsx(dnd.sam.bad.2, "w_unit/dnd_geo2_failed.xlsx", row.names = FALSE)

# Read in run 3
dnd.sam.3 <- read.csv("w_unit/dnd_geo3.csv")
dnd.sam.good.3 <- subset(dnd.sam.3, dnd.sam.3$Match.Score > 1)
dnd.sam.bad.3 <- subset(dnd.sam.3, is.na(dnd.sam.3$Match.Score))
#
# Pull out relevant columns from dnd.sam.good
dnd.sam.good.rdy <- data.table(dnd.sam.good$Project__Project_Name, 
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
colnames(dnd.sam.good.rdy) <- c('Project: Project Name', 'Project Unit: Unit_Name',
                               'Unit Street #', 'Unit Street Name', 'Unit #', 'Unit ZIP',
                               'Parcel Info', 'Ownership Units', 'Subtotal Restricted Units (fm_Prjt)',
                               'SAM ID', 'Address')

# Pull out relevant columns from geo2 good
dnd.sam.good.2.rdy <- data.table(dnd.sam.good.2$Project__Project_Name, 
                                dnd.sam.good.2$Project_Unit__Unit_Name,
                                dnd.sam.good.2$Unit_Street__,
                                dnd.sam.good.2$Unit_Street_Name,
                                dnd.sam.good.2$Unit__,
                                dnd.sam.good.2$Unit_ZIP,
                                dnd.sam.good.2$Parcel_Info,
                                dnd.sam.good.2$Ownership_Units,
                                dnd.sam.good.2$Subtotal_Restricted_Units__fm_Prjt_,
                                dnd.sam.good.2$Match.Id,
                                dnd.sam.good.2$Address)
colnames(dnd.sam.good.2.rdy) <- c('Project: Project Name', 'Project Unit: Unit_Name',
                                 'Unit Street #', 'Unit Street Name', 'Unit #', 'Unit ZIP',
                                 'Parcel Info', 'Ownership Units', 'Subtotal Restricted Units (fm_Prjt)',
                                 'SAM ID', 'Address')

# Pull out relevant columns from geo3 good
dnd.sam.good.3.rdy <- data.table(dnd.sam.good.3$Project__Project_Name, 
                                 dnd.sam.good.3$Project_Unit__Unit_Name,
                                 dnd.sam.good.3$Unit_Street__,
                                 dnd.sam.good.3$Unit_Street_Name,
                                 dnd.sam.good.3$Unit__,
                                 dnd.sam.good.3$Unit_ZIP,
                                 dnd.sam.good.3$Parcel_Info,
                                 dnd.sam.good.3$Ownership_Units,
                                 dnd.sam.good.3$Subtotal_Restricted_Units__fm_Prjt_,
                                 dnd.sam.good.3$Match.Id,
                                 dnd.sam.good.3$Address)
colnames(dnd.sam.good.3.rdy) <- c('Project: Project Name', 'Project Unit: Unit_Name',
                                  'Unit Street #', 'Unit Street Name', 'Unit #', 'Unit ZIP',
                                  'Parcel Info', 'Ownership Units', 'Subtotal Restricted Units (fm_Prjt)',
                                  'SAM ID', 'Address')

dnd.sam.bad.3.rdy <- data.table(dnd.sam.bad.3$Project__Project_Name, 
                                 dnd.sam.bad.3$Project_Unit__Unit_Name,
                                 dnd.sam.bad.3$Unit_Street__,
                                 dnd.sam.bad.3$Unit_Street_Name,
                                 dnd.sam.bad.3$Unit__,
                                 dnd.sam.bad.3$Unit_ZIP,
                                 dnd.sam.bad.3$Parcel_Info,
                                 dnd.sam.bad.3$Ownership_Units,
                                 dnd.sam.bad.3$Subtotal_Restricted_Units__fm_Prjt_,
                                 dnd.sam.bad.3$Match.Id,
                                 dnd.sam.bad.3$Address)
colnames(dnd.sam.bad.3.rdy) <- c('Project: Project Name', 'Project Unit: Unit_Name',
                                  'Unit Street #', 'Unit Street Name', 'Unit #', 'Unit ZIP',
                                  'Parcel Info', 'Ownership Units', 'Subtotal Restricted Units (fm_Prjt)',
                                  'SAM ID', 'Address')


dnd.address.geo.all.units <- rbind(dnd.sam.good.rdy, dnd.sam.good.2.rdy,
                                   dnd.sam.good.3.rdy, dnd.sam.bad.3.rdy)



write.xlsx(dnd.address.geo.all.units, "w_unit/DND_Monitored_Ownership_Units_for_STR_SAMID_units.xlsx", row.names = FALSE)

gs_upload("w_unit/DND_Monitored_Ownership_Units_for_STR_SAMID_units.xlsx",
          sheet_title = "DND Monitored Ownership Units for STR SAMID units")





