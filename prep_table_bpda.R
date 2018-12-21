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
str.connect <- gs_title("BPDA restricted condos 2018_12_21")
bpda <- gs_read(str.connect)

bpda.address <- unite(bpda, Address, 
                     c("Street #", "Street Name", "Street suffix", "ZIP"), 
                     sep = " ", 
                     remove = FALSE)

write.csv(bpda.address, "bpda_addresses.csv", row.names = FALSE)

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











