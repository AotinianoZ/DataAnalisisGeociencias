deg2num<-function(lat_deg, lon_deg, zoom){
  lat_rad <- lat_deg * pi /180
  n <- 2.0 ^ zoom
  xtile <- floor((lon_deg + 180.0) / 360.0 * n)
  ytile = floor((1.0 - log(tan(lat_rad) + (1 / cos(lat_rad))) / pi) / 2.0 * n)
  return( c(xtile, ytile))
  #  return(paste(paste("https://a.tile.openstreetmap.org", zoom, xtile, ytile, sep="/"),".png",sep=""))
}

deg2num(-3.866,-80.295, 10)


# Returns data frame containing detailed info for all zooms
deg2num.all<-function(lat_deg, lon_deg){
  nums <- as.data.frame(matrix(ncol=6,nrow=21))
  colnames(nums) <- c('zoom', 'x', 'y', 'mapquest_osm', 'mapquest_aerial', 'osm')
  rownames(nums) <- 0:20
  for (zoom in 0:20) {
    num <- deg2num(lat_deg, lon_deg, zoom)
    nums[1+zoom,'zoom'] <- zoom
    nums[1+zoom,'x'] <- num[1]
    nums[1+zoom,'y'] <- num[2]
    nums[1+zoom,'mapquest_osm'] <- paste('http://otile1.mqcdn.com/tiles/1.0.0/map/', zoom, '/', num[1], '/', num[2], '.jpg', sep='')
    nums[1+zoom,'mapquest_aerial'] <- paste('http://otile1.mqcdn.com/tiles/1.0.0/sat/', zoom, '/', num[1], '/', num[2], '.jpg', sep='')
    nums[1+zoom,'osm'] <- paste('https://a.tile.openstreetmap.org/', zoom, '/', num[1], '/', num[2], '.png', sep='')
  }
  return(nums)
}
deg2num.all(-3.866,-80.295)
