# fix_records_deletions.R

# Load Requirements ----

# _ Load libraries
library(dplyr)

# _ Load useful globals / functions
source("~/Desktop/config.R")
source("~/Desktop/helpers.R")
rm(BOX_CLIENT_ID); rm(BOX_CLIENT_SECRET); rm(BOX_REDIRECT_URI)
rm(REDCAP_API_TOKEN_MINDSET)
rm(REDCAP_API_TOKEN_UDS2)
rm(REDCAP_API_TOKEN_UDS3a)
rm(REDCAP_DATA_REQUESTS_TOKEN)

# Get UDS 3 non-DDE data ----

# _ Which IDs have deleted records? ----

recs_deld_raw <-
  c(
    "UM00001403",
    "UM00001437",
    "UM00001606",
    "UM00001137",
    "UM00000861",
    "UM00000543",
    "UM00001045",
    "UM00001440",
    "UM00001154",
    "UM00001609",
    "UM00001611",
    "UM00001614",
    "UM00001564",
    "UM00001446",
    "UM00001153",
    "UM00001040",
    "UM00001610"
  )
recs_deld_raw <- sort(recs_deld_raw)
recs_deld <- recs_deld_raw %>% paste(collapse = ",")

# _ Get JSON from REDCAP
json_u3nnoDDE <- rc_api_get(uri      = REDCAP_API_URI,
                             token   = REDCAP_API_TOKEN_UDS3n_noDDE,
                             records = recs_deld)
df_u3nnoDDE <- jsonlite::fromJSON(json_u3nnoDDE) %>% na_if("")

# Process Data

uniq_ids <- df_u3nnoDDE %>% 
  distinct(ptid) %>% 
  pull() %>% 
  sort()

length(recs_deld_raw)
length(uniq_ids)
recs_deld_raw == uniq_ids

# IDs in both record/id vectors
uniq_ids[uniq_ids %in% recs_deld_raw] # all uniq_ids in recs_deld_raw
recs_deld_raw[recs_deld_raw %in% uniq_ids]
# IDs unique to uniq_ids
uniq_ids[!(uniq_ids %in% recs_deld_raw)] # None
# IDs unique to recs_deld_raw
recs_deld_raw[!(recs_deld_raw %in% uniq_ids)] # there are three

# _ Clean data

df_u3nnoDDE_cln <- df_u3nnoDDE %>% 
  filter(!is.na(form_date))

# _ Deselect irrelevant fields

df_u3nnoDDE_cln_sel <- df_u3nnoDDE_cln %>% 
  select(
    -lbssupht,
    -lburgrux, 
    -lbumrglu, 
    -lbumpslh, 
    -lbddrug1, 
    -lbddose1, 
    -lbddose2, 
    -lbsctrt, 
    -consfall, 
    -conswkof, 
    -conslyaw, 
    -conswker, 
    -conslttl, 
    -sccorate, 
    -codsunex, 
    -codssitp, 
    -codswatv, 
    -codstalk, 
    -codsawdy, 
    -codsfldy, 
    -sccofrst, 
    -sccoagen, 
    -sccoaged, 
    -sccocomp, 
    -sccoscvr, 
    -sccooth, 
    -sccoscor, 
    -iv_b9l_complete, 
    -lbnsword, 
    -lbnscolr, 
    -lbnsclwd, 
    -lbnpface, 
    -lbnpnois, 
    -lbnptcor, 
    -lbnppard, 
    -c1l_complete, 
    -lbcdscog, 
    -lbccmem, 
    -lbcclang, 
    -lbccatt, 
    -lbccexde, 
    -lbccvis, 
    -lbcdsmov, 
    -lbcmbrad, 
    -lbcmrigd, 
    -lbcmrtrm, 
    -lbcmptrm, 
    -lbcmatrm, 
    -lbcmmyoc, 
    -lbcmgait, 
    -lbcmpins, 
    -lbcdsbev, 
    -lbcbdep, 
    -lbcbapa, 
    -lbcbanx, 
    -lbcbhall, 
    -lbcbdel, 
    -lbcdsaut, 
    -lbcarem, 
    -lbcaapn, 
    -lbcalgsl, 
    -lbcarsle, 
    -lbcadtsl, 
    -lbcacgfl, 
    -lbcahypt, 
    -lbcacons, 
    -lbcahyps, 
    -lbcafall, 
    -lbcasync, 
    -lbcasnap, 
    -lbcogst, 
    -lbcogdx, 
    -d1l_complete, 
    -lbgnewgn, 
    -lbglrrk2, 
    -lbglrkis, 
    -lbgpark2, 
    -lbgpk2is, 
    -lbgpark7, 
    -lbgpk7is, 
    -lbgpink1, 
    -lbgpnkis, 
    -lbgsnca, 
    -lbgsncis, 
    -lbggba, 
    -lbggbais, 
    -lbgothr, 
    -lbgothis, 
    -lbgothx, 
    -ivp_e1l_complete, 
    -lbismri, 
    -lbismmo, 
    -lbismdy, 
    -lbismyr, 
    -lbismqav, 
    -lbismhip, 
    -lbismavl, 
    -lbismdcm, 
    -lbismfmt, 
    -lbismadn, 
    -lbismver, 
    -lbismman, 
    -lbismom, 
    -lbismstr, 
    -lbismos, 
    -lbifpet, 
    -lbifpmo, 
    -lbifpdy, 
    -lbifpyr, 
    -lbifpqav, 
    -lbifpocc, 
    -lbifptpp, 
    -lbifpisl, 
    -lbifpavl, 
    -lbifpdcm, 
    -lbifpfmt, 
    -lbifpadn, 
    -lbifpver, 
    -lbifpman, 
    -lbifpom, 
    -lbiapet, 
    -lbiapmo, 
    -lbiapdy, 
    -lbiapyr, 
    -lbiapqav, 
    -lbiapavl, 
    -lbiapdcm, 
    -lbiapfmt, 
    -lbiaplig, 
    -lbiapol, 
    -lbiapadn, 
    -lbiapver, 
    -lbiapman, 
    -lbiapom, 
    -lbitpet, 
    -lbitpmo, 
    -lbitpdy, 
    -lbitpyr, 
    -lbitpqav, 
    -lbitpavl, 
    -lbitpdcm, 
    -lbitpfmt, 
    -lbitplig, 
    -lbitpol, 
    -lbitpadn, 
    -lbitpver, 
    -lbitpman, 
    -lbitpom, 
    -lbidats, 
    -lbidsmo, 
    -lbidsdy, 
    -lbidsyr, 
    -lbidsqav, 
    -lbidsabn, 
    -ivp_e2l_complete, 
    -lbopolys, 
    -lboposmo, 
    -lboposdy, 
    -lboposyr, 
    -lbopopos, 
    -lbopoavl, 
    -lbocmibg, 
    -lbocmmo, 
    -lbocmdy, 
    -lbocmyr, 
    -lbocmpos, 
    -lbocmavl, 
    -lboanos, 
    -lboanmo, 
    -lboandy, 
    -lboanyr, 
    -lboanpos, 
    -lboanavl, 
    -lboanver, 
    -lboanoth, 
    -lboeeg, 
    -lboegmo, 
    -lboegdy, 
    -lboegyr, 
    -lboegpos, 
    -lboegavl, 
    -lbomslt, 
    -lbomsmo, 
    -lbomsdy, 
    -lbomsyr, 
    -lbomspos, 
    -lbomsavl, 
    -lbotilt, 
    -lbotlmo, 
    -lbotldy, 
    -lbotlyr, 
    -lbotlpos, 
    -lbotlavl, 
    -lboqsart, 
    -lboqsmo, 
    -lboqsdy, 
    -lboqsyr, 
    -lboqspos, 
    -lbosgavl, 
    -lbotherm, 
    -lbothmo, 
    -lbothdy, 
    -lbothyr, 
    -lbothpos, 
    -lbothavl, 
    -lbocgait, 
    -lbocgmo, 
    -lbocgdy, 
    -lbocgyr, 
    -lbocgpos, 
    -lbocgavl, 
    -ivp_e3l_complete)

# df_u3nnoDDE_cln is what'll get imported to R/C
readr::write_csv(df_u3nnoDDE_cln_sel, "df_u3nnoDDE_cln_sel.csv", na = "")

uniq_ids_cln <- df_u3nnoDDE_cln %>% 
  distinct(ptid) %>% 
  pull() %>% 
  sort()

# What uniq IDs are in the `df_u3nnoDDE_cln`?
uniq_ids_cln 
length(uniq_ids_cln)

# What IDs do we probably not need to worry about?
# ... we should manually check these anyway
recs_deld_raw[!(recs_deld_raw %in% uniq_ids_cln)]




###@    #==--  :  --==#    @##==---==##@##==---==##@    #==--  :  --==#    @###
#==##@    #==-- --==#    @##==---==##@   @##==---==##@    #==-- --==#    @##==#
#--==##@    #==-==#    @##==---==##@   #   @##==---==##@    #==-==#    @##==--#
#=---==##@    #=#    @##==---==##@    #=#    @##==---==##@    #=#    @##==---=#
##==---==##@   #   @##==---==##@    #==-==#    @##==---==##@   #   @##==---==##
#@##==---==##@   @##==---==##@    #==-- --==#    @##==---==##@   @##==---==##@#
#  @##==---==##@##==---==##@    EXTRA  :  SPACE    @##==---==##@##==---==##@  #
#@##==---==##@   @##==---==##@    #==-- --==#    @##==---==##@   @##==---==##@#
##==---==##@   #   @##==---==##@    #==-==#    @##==---==##@   #   @##==---==##
#=---==##@    #=#    @##==---==##@    #=#    @##==---==##@    #=#    @##==---=#
#--==##@    #==-==#    @##==---==##@   #   @##==---==##@    #==-==#    @##==--#
#==##@    #==-- --==#    @##==---==##@   @##==---==##@    #==-- --==#    @##==#
###@    #==--  :  --==#    @##==---==##@##==---==##@    #==--  :  --==#    @###
#==##@    #==-- --==#    @##==---==##@   @##==---==##@    #==-- --==#    @##==#
#--==##@    #==-==#    @##==---==##@   #   @##==---==##@    #==-==#    @##==--#
#=---==##@    #=#    @##==---==##@    #=#    @##==---==##@    #=#    @##==---=#
##==---==##@   #   @##==---==##@    #==-==#    @##==---==##@   #   @##==---==##
#@##==---==##@   @##==---==##@    #==-- --==#    @##==---==##@   @##==---==##@#
#  @##==---==##@##==---==##@    EXTRA  :  SPACE    @##==---==##@##==---==##@  #
#@##==---==##@   @##==---==##@    #==-- --==#    @##==---==##@   @##==---==##@#
##==---==##@   #   @##==---==##@    #==-==#    @##==---==##@   #   @##==---==##
#=---==##@    #=#    @##==---==##@    #=#    @##==---==##@    #=#    @##==---=#
#--==##@    #==-==#    @##==---==##@   #   @##==---==##@    #==-==#    @##==--#
#==##@    #==-- --==#    @##==---==##@   @##==---==##@    #==-- --==#    @##==#
###@    #==--  :  --==#    @##==---==##@##==---==##@    #==--  :  --==#    @###