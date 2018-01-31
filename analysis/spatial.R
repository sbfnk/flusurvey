library('sf')
library('lwgeom')
library('ggplot2')
library('dplyr')
library('readr')
library('stringi')
library('magrittr')

## directory that holds the shape files
shapefile_dir <- path.expand("~/Research/Shapefiles")

## read NUTS region names
names <- read_csv(file.path(shapefile_dir, "NUTS_2013L_20180130_131122.csv")) %>%
  select(NUTS_ID=`NUTS-Code`, name=Description) %>%
  mutate(name=stri_trans_totitle(name))

## read shape file, select UK NUTS 1 polygons and join region names
uk <- st_read(dsn=file.path(shapefile_dir, "NUTS_2013_01M_SH", "data"), layer="NUTS_RG_01M_2013") %>%
  dplyr::filter(STAT_LEVL_ == 1) %>%
  dplyr::filter(grepl("^UK", NUTS_ID)) %>%
  inner_join(names)

## sample data - from beta distribution
test <- tibble(name=uk$name) %>%
  mutate(abx_usage=rbeta(n(), 1, 10))

## join sample data to regions
uk %<>% left_join(test)

ggplot(uk) +
  geom_sf(aes(fill=abx_usage)) +
  xlab("") + ylab("") +
  coord_sf(datum=NA) +
  theme(axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())
