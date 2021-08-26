#------------------------------------------------------------------------
require(pacman)
pacman::p_load(RColorBrewer, ggspatial, raster,colorspace, ggpubr, sf,openxlsx)
#------------------------------------------------------------------------
Peru               <- getData('GADM', country='Peru', level=1) %>% st_as_sf()
Cuencas_peru       <- st_read ("SHP/Cuencas_peru.shp")  
Rio_libe           <- st_read ("SHP/RIOS_LA_LIBERTAD_geogpsperu_SuyoPomalia_931381206.shp")  
Cuencas_peru       <- st_transform(Cuencas_peru ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Rio_libe           <- st_transform(Rio_libe ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Cuenca_moche       <- subset(Cuencas_peru , NOMB_UH_N6  == "Moche")
Cuencas_rios       <- st_intersection(Rio_libe, Cuenca_moche)

dem = raster("raster/ASTGTM_S08W079_dem.tif")
dem2 = raster("raster/ASTGTM_S09W079_dem.tif")
dem3 = raster("raster/ASTGTM_S09W080_dem.tif")
DEM_total<- raster::merge(dem, dem2,dem3)

Cuenca_moche_alt     <- crop(DEM_total, Cuenca_moche)
Cuenca_moche_alt     <- Cuenca_moche_alt  <- mask(Cuenca_moche_alt , Cuenca_moche)
plot(Cuenca_moche_alt )
dem.pa         <-  rasterToPoints(aps  )
df_a            <-  data.frame(dem.pa)

aps    = terrain(Cuenca_moche_alt , opt = "aspect", unit= "degrees")
dem.p          <-  rasterToPoints(Cuenca_moche_alt)
df             <-  data.frame(dem.p)
colnames(df) = c("lon", "lat", "alt")
#------------------------------------------------------------------------
Data     <- read.xlsx("Excel/Embrete.xlsx", sheet="Hoja2") 
Data[1,1] <- "MAPA DE ELEVACION DE \nLA CUENCA, \nMOCHE"
Data[2,1] <- "Elaboradpor: \nGorky Florez Castillo"
Data[3,1] <- "Escala: Indicadas"
Data[4,1] <- "Sistemas de Coordenadas UTM \nZona 18S \nDatum:WGS84"
colnames(Data ) <- c("Mapa elaborado \nen  RStudio")
Tabla.p <- ggtexttable(Data, rows = NULL,theme =ttheme( base_size =6, "lBlackWhite"))
#------------------------------------------------------------------------
ggplot()+
  geom_raster(data = df, aes(lon,lat, fill = alt) )+
  geom_sf(data=Cuencas_rios, color="blue")+
  scale_fill_distiller(palette   = "RdYlGn",
                       na.value = 'white')
  
A<-ggplot()+
  geom_raster(data = df, aes(lon,lat, fill = alt) )+
  geom_sf(data=Cuencas_rios, color="blue")+ 
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(n = 8, name = "Greys"), 
                       na.value = 'white', limits = c(0, 360), breaks = seq(0, 360, 50)) +
  theme_bw()

summary(df$alt) 
A<-ggplot()+ 
    geom_raster(data = df, aes(lon,lat,fill = alt),alpha=0.75) + 
    geom_raster(data = df_a, aes(x=x, y=y, alpha=aspect),fill="gray20")+
    scale_alpha(guide=FALSE,range = c(0,1.00))   +
    scale_fill_gradientn(name="Elevacion \n(m.s.n.m)",colours = terrain.colors(100),
                         labels = c("[1 - 270] ","[270-400]", "[400-700]", "[700-1200]", "[1200-1700]",
                                    "[1700-2200]", "[2200-3500]", "[3500-3700]", "[3700-4100]", "[4100-4280]"),
                         breaks = c(0, 270, 400,700,1200,1700,2200,3500,3700,4100))+
    theme_bw()+coord_equal()+
    guides(fill = guide_legend(title.position = "top",direction = "vertical",
                             title.theme = element_text(angle = 0, size = 9, colour = "black"),
                             barheight = .5, barwidth = .95,
                             title.hjust = 0.5, raster = FALSE,
                             title = 'Elevacion \n(m.s.n.m)'))+
    geom_sf(data=Cuencas_rios, color="blue", size=0.5)+
    scale_x_continuous(name=expression(paste("Longitude (",degree,")"))) +
    scale_y_continuous(name=expression(paste("Latitude (",degree,")")))+
    annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
    ggspatial::annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
    theme(legend.position = c(0.90,0.20),
        panel.grid.major = element_line(color = gray(.5),
                                        linetype = "dashed", size = 0.5),
        legend.background = element_blank(),
        plot.title=element_text(color="#666666", size=12, vjust=1.25,  family="Raleway",  hjust = 0.5),
        legend.box.just = "left",
        legend.text = element_text(size=9,face=2),
        legend.title = element_text(size=9,face=2))+
  guides(fill = guide_legend(nrow = 5, ncol=2))+
  annotation_custom(ggplotGrob(Tabla.p ),
                    xmin = -79.02, xmax = -79,ymin = -8,ymax = -7.9)+
  labs(title="MAPA DE ELEVACIONES CUENCA MOCHE", 
       subtitle="Elevacion con aspecto con ggplot", 
       caption="Fuente: https://www.geogpsperu.com/2018/08/descargar-imagenes-aster-gdem-aster.html", 
       color=NULL)+
  annotate(geom = "text", x = -78.7, y = -8.18, 
           label = "Quieres desarrollar mapas de este tipo \n inscribete a \nAPRENDE R DESDE CERO PARA SIG ", 
           fontface = "italic", color = "black", size = 3)


B <- ggplot()+
  geom_sf(data= Peru, fill="white")+
  geom_sf(data= Cuenca_moche, fill="red")+
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_rect(colour= "black", size= 1))
B.grob        <- ggplotGrob(B)

map.bound.inset <- A + 
  annotation_custom(grob= B.grob, xmin = -78.5, xmax = -78.1, ymin =-7.9, ymax=-7.77)


map.final <- map.bound.inset +
  geom_segment(aes(x=-78.3, xend=-78.7, y=-7.84, yend=-7.8),  linetype = "solid", color = "grey20", size = 0.3) +
  geom_segment(aes(x=-78.3, xend=-78.4, y=-7.84, yend=-7.87), linetype = "solid", color = "grey20", size = 0.3)

ggsave(plot =map.final ,"Mpas/MOCHE_elevacion.png", units = "cm", width = 29,height = 21, dpi = 900) 

