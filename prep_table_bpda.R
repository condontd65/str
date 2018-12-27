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

# Read in the str bpda spreadsheet
str.connect <- gs_title("BPDA restricted condos 2018_12_21")
bpda <- gs_read(str.connect)

bpda$unit_id <- as.character(bpda$`Unit #`)
bpda$unit_id <- paste0("#", bpda$unit_id, sep = "")

bpda.address <- unite(bpda, Address, 
                     c("Street #", "Street Name", "Street suffix", "ZIP"), 
                     sep = " ", 
                     remove = FALSE)

bpda.address.unit <- unite(bpda, Address, 
                      c("Street #", "Street Name", "Street suffix", "unit_id", "ZIP"), 
                      sep = " ", 
                      remove = FALSE)

write.csv(bpda.address, "bpda_addresses.csv", row.names = FALSE)
write.csv(bpda.address.unit, "bpda_addresses_unit.csv", row.names = FALSE)

## Run 1

bpda.run1 <- read.xlsx("bpda_run1.xlsx", 1)
bpda.sam.good <- subset(bpda.run1, Score > 60)

# Assign columns that we're interested in
bpda.address.geo <- data.table(bpda.sam.good$Street__,
                               bpda.sam.good$Street_Name,
                               bpda.sam.good$Street_suffix,
                               bpda.sam.good$Unit__,
                               bpda.sam.good$ZIP_1,
                               bpda.sam.good$Ref_ID,
                               bpda.sam.good$Address)
colnames(bpda.address.geo) <- c('Street #', 'Street Name',
                               'Street suffix', 'Unit #', 'ZIP', 'SAM ID',
                               'Address')

## Run 2
bpda.run2 <- read.csv("bpda_run2.csv")
bpda.run2 <- bpda.run2[1:69,1:16]
bpda.sam.good.run2 <- subset(bpda.run2, Match.Score > 0)

bpda.address.geo2 <- data.table(bpda.sam.good.run2$Street__,
                               bpda.sam.good.run2$Street_Name,
                               bpda.sam.good.run2$Street_suffix,
                               bpda.sam.good.run2$Unit__,
                               bpda.sam.good.run2$ZIP_1,
                               bpda.sam.good.run2$Match.Id,
                               bpda.sam.good.run2$Address)
colnames(bpda.address.geo2) <- c('Street #', 'Street Name',
                                'Street suffix', 'Unit #', 'ZIP', 'SAM ID',
                                'Address')

bpda.addres.geo.all <- rbind(bpda.address.geo, bpda.address.geo2)

# Run 3
bpda.run3 <- read.csv('bpda_run3.csv')
bpda.run3 <- bpda.run3[1:43,1:16]
bpda.sam.good.run3 <- subset(bpda.run3, Match.Score > 0)

bpda.address.geo3 <- data.table(bpda.sam.good.run3$Street__,
                                bpda.sam.good.run3$Street_Name,
                                bpda.sam.good.run3$Street_suffix,
                                bpda.sam.good.run3$Unit__,
                                bpda.sam.good.run3$ZIP_1,
                                bpda.sam.good.run3$Match.Id,
                                bpda.sam.good.run3$Address)
colnames(bpda.address.geo3) <- c('Street #', 'Street Name',
                                 'Street suffix', 'Unit #', 'ZIP', 'SAM ID',
                                 'Address')

bpda.address.geo.all <- rbind(bpda.addres.geo.all, bpda.address.geo3)

# Add in final failed geocodes
bpda.sam.bad.run3 <- subset(bpda.run3, Match.Score == 0)

bpda.address.geo3.bad <- data.table(bpda.sam.bad.run3$Street__,
                                bpda.sam.bad.run3$Street_Name,
                                bpda.sam.bad.run3$Street_suffix,
                                bpda.sam.bad.run3$Unit__,
                                bpda.sam.bad.run3$ZIP_1,
                                bpda.sam.bad.run3$Match.Id,
                                bpda.sam.bad.run3$Address)
colnames(bpda.address.geo3.bad) <- c('Street #', 'Street Name',
                                 'Street suffix', 'Unit #', 'ZIP', 'SAM ID',
                                 'Address')

bpda.address.geo3.bad$`SAM ID` <- NA
bpda.address.geo.all <- rbind(bpda.address.geo.all, bpda.address.geo3.bad)

write.xlsx(bpda.address.geo.all, "BPDA_restricted_condos_2018_12_21_SAMID.xlsx", row.names = FALSE)




####### Units included
bpda.run1.units <- read.csv("bpda_run1_units_included.csv")
bpda.run1.units <- bpda.run1.units[1:1053, 1:18]
bpda.run1.units.good <- subset(bpda.run1.units, bpda.run1.units$Match.Score > 4)
bpda.run1.units.bad <- subset(bpda.run1.units, bpda.run1.units$Match.Score == 0)

# Good

bpda.address.units.good <- data.table(bpda.run1.units.good$Street..,
                                    bpda.run1.units.good$Street.Name,
                                    bpda.run1.units.good$Street.suffix,
                                    bpda.run1.units.good$Unit..,
                                    bpda.run1.units.good$ZIP,
                                    bpda.run1.units.good$Match.Id,
                                    bpda.run1.units.good$Address)
colnames(bpda.address.units.good) <- c('Street #', 'Street Name',
                                     'Street suffix', 'Unit #', 'ZIP', 'SAM ID',
                                     'Address')

# Bad

bpda.address.units.bad <- data.table(bpda.run1.units.bad$Street..,
                                      bpda.run1.units.bad$Street.Name,
                                      bpda.run1.units.bad$Street.suffix,
                                      bpda.run1.units.bad$Unit..,
                                      bpda.run1.units.bad$ZIP,
                                      bpda.run1.units.bad$Match.Id,
                                      bpda.run1.units.bad$Address)
colnames(bpda.address.units.bad) <- c('Street #', 'Street Name',
                                       'Street suffix', 'Unit #', 'ZIP', 'SAM ID',
                                       'Address')

# Make sam id na for bad
bpda.address.units.bad$`SAM ID` <- NA

# rbind them together
#bpda.units <- rbind(bpda.address.units.good, bpda.address.units.bad)
write.csv(bpda.units, "BPDA_restricted_condos_2018_12_21_SAMID_units.csv", row.names = FALSE)


# Units run 2
bpda.run2.units <- read.csv("bpda_run2_units_included.csv")
bpda.run2.units <- bpda.run2.units[, 1:18]
bpda.run2.units.good <- subset(bpda.run2.units, bpda.run2.units$Match.Score > 4)
bpda.run2.units.bad <- subset(bpda.run2.units, bpda.run2.units$Match.Score == 0)

# Good

bpda.address.units.good2 <- data.table(bpda.run2.units.good$Street..,
                                      bpda.run2.units.good$Street.Name,
                                      bpda.run2.units.good$Street.suffix,
                                      bpda.run2.units.good$Unit..,
                                      bpda.run2.units.good$ZIP,
                                      bpda.run2.units.good$Match.Id,
                                      bpda.run2.units.good$Address)
colnames(bpda.address.units.good2) <- c('Street #', 'Street Name',
                                       'Street suffix', 'Unit #', 'ZIP', 'SAM ID',
                                       'Address')

# Bind the good data. Add in the bad data after one more run.
bpda.units <- rbind(bpda.address.units.good, bpda.address.units.good2)


# Run 3

bpda.run3.units <- read.csv("bpda_run3_units_included.csv")
bpda.run3.units <- bpda.run3.units[, 1:23]
bpda.run3.units.good <- subset(bpda.run3.units, bpda.run3.units$Score > 50)
bpda.run3.units.bad <- subset(bpda.run3.units, bpda.run3.units$Score < 50)

bpda.address.units.good3 <- data.table(bpda.run3.units.good$Street__,
                                bpda.run3.units.good$Street_Name,
                                bpda.run3.units.good$Street_suffix,
                                bpda.run3.units.good$Unit__,
                                bpda.run3.units.good$ZIP_1,
                                bpda.run3.units.good$Ref_ID,
                                bpda.run3.units.good$Address_Text)
colnames(bpda.address.units.good3) <- c('Street #', 'Street Name',
                                 'Street suffix', 'Unit #', 'ZIP', 'SAM ID',
                                 'Address')
bpda.address.units.bad3 <- data.table(bpda.run3.units.bad$Street__,
                                       bpda.run3.units.bad$Street_Name,
                                       bpda.run3.units.bad$Street_suffix,
                                       bpda.run3.units.bad$Unit__,
                                       bpda.run3.units.bad$ZIP_1,
                                       bpda.run3.units.bad$Ref_ID,
                                       bpda.run3.units.bad$Address_Text)
colnames(bpda.address.units.bad3) <- c('Street #', 'Street Name',
                                        'Street suffix', 'Unit #', 'ZIP', 'SAM ID',
                                        'Address')
bpda.address.units.bad3$`SAM ID` <- NA

# Bind them all together
bpda.units.final <- rbind(bpda.units, bpda.address.units.good3, bpda.address.units.bad3)
write.xlsx(bpda.units.final, "BPDA_restricted_condos_2018_12_21_SAMID_allunits.xlsx", row.names = FALSE)











