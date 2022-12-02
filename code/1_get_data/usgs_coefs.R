# Use hydraulic data in mda.streams R package from Alison Appling

appling_sites <- read_tsv('data/morphology/site_data.tsv')

# load site codes and lat/long
met_sites <- read_csv('data/site_locs.csv')

# subset to just the codes
met_codes <- met_sites %>%
  pull(site_no) 

# are our sites in Alison's list?
appling_sites$nwis_id %in% met_codes

# filter the coefficients
usgs_sites_fromAA <- appling_sites %>% 
  filter(nwis_id %in% met_codes) %>% 
  rename(dvqcoefs.d = dvqcoefs.m,
         dvqcoefs.e = dvqcoefs.k)

write_csv(usgs_sites_fromAA,
          'data/morphology/Appling et al. 2018/usgs_sites_fromAA.csv')

write_csv(usgs_sites_fromAA %>%
            select(site_name,nwis_id, starts_with('dvq')),
          'data/morphology/Appling et al. 2018/usgs_sites_coefs.csv')

