require(pacman)

p_load(tidyverse,sf,rgeos)

#precinct votes: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/LYWX3D
load("~/gerrymander/house_precincts_2016.rda")

df1 <- df1 %>% dplyr::filter(state %in% c("Pennsylvania","Massachusetts"))
df_ma <- df1 %>% dplyr::filter(state=="Massachusetts")

df2 <- df1 %>% group_by(state,district,party,precinct) %>% summarise(votes=sum(votes))
df2$party <- ifelse(df2$party=="","other",df2$party)

df2 <- df2 %>% pivot_wider(id_cols=c("state","district","precinct"),names_from="party",values_from="votes",values_fill=0)
df2 <- df2 %>% mutate(total_votes = other + democratic + libertarian + unenrolled + republican)


#precinct shapefiles: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/NH5S2I
pa <- st_read("~/gerrymander/shapefiles/PA/pa_2016.shp") %>% st_make_valid()
ma <- st_read("~/gerrymander/shapefiles/MA/ma_2016.shp") %>% st_make_valid()
cd <- st_read("~/gerrymander/shapefiles/cd/cb_2018_us_cd116_500k.shp")
pa1 <- cd %>% dplyr::filter(STATEFP==42) 
ma1 <- cd %>% dplyr::filter(STATEFP==25) 

ma2 <- df2 %>% dplyr::filter(state=="Massachusetts")
pa2 <- df2 %>% dplyr::filter(state=="Pennsylvania")

ma1 <- st_transform(ma1,crs=st_crs(ma))
pa1 <- st_transform(pa1,crs=st_crs(pa))

a <- st_intersects(ma1,ma)

pa$trump <- pa$G16PRERTRU/(pa$G16PREDCLI + pa$G16PRERTRU)
pa$clinton <- pa$G16PREDCLI/(pa$G16PREDCLI + pa$G16PRERTRU)

ma$trump <- ma$G16PRERTRU/(ma$G16PREDCLI + ma$G16PRERTRU)
ma$clinton <- ma$G16PREDCLI/(ma$G16PREDCLI + ma$G16PRERTRU)

ggplot(pa1) +
  geom_sf()

ggplot(ma[which(!is.na(ma$clinton)),]) +
  geom_sf(aes(fill=trump),color=NA,show.legend = T) +
  theme(panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_gradient2(
    high = "#c90d00",
    mid = "#faedff",
    low = "#1400c9",
    midpoint = 0.5,
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "fill"
  ) +
  geom_sf(data=a,alpha=0,color="white")


ggplot(ma[unlist(a[4]),]) +
  geom_sf(aes(fill=trump),color=NA,show.legend = T) +
  theme(panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_gradient2(
    high = "#c90d00",
    mid = "#faedff",
    low = "#1400c9",
    midpoint = 0.5,
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "fill"
  ) +
  geom_sf(data=ma1,alpha=0,color="white")

###### plot a single congressional district ##########

cdnum <- c("01")

ggplot(st_intersection(ma,ma1[which(ma1$CD116FP==cdnum),])) + 
  geom_sf(aes(fill=trump),color=NA,show.legend = T) +
  labs(fill="") +
  theme(panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_steps2(high = "#c90d00",
                    mid = "#faedff",
                    low = "#1400c9",
                     midpoint = 0.5,
                    limits=c(0.001,0.999),
                    breaks=c(0,.3,.4,.5,.6,.7,1)) +
  geom_sf(data=ma1,alpha=0,color="black")


cdnum <- c("18")


ggplot(st_intersection(pa,pa1[which(pa1$CD116FP==cdnum),])) + 
  geom_sf(aes(fill=trump),color=NA,show.legend = T) +
  labs(fill="") +
  theme(panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_blank()) +
  scale_fill_steps2(high = "#c90d00",
                    mid = "#faedff",
                    low = "#1400c9",
                    midpoint = 0.5,
                    limits=c(0.001,0.999),
                    breaks=seq(from=0,to=1,by=1/6))
  geom_sf(data=pa1,alpha=0,color="black")
