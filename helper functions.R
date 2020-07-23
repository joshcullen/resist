df.to.list = function(dat, ind) {  #ind must be in quotes
  id<- unique(dat[,ind]) %>% dplyr::pull()
  n=length(id)
  dat.list<- vector("list", n)
  names(dat.list)<- id
  
  for (i in 1:length(id)) {
    tmp<- which(dat[,ind] == id[i])
    dat.list[[i]]<- dat[tmp,]
  }
  dat.list
}
#----------------------------

# match_time = function(path_df, track_df, id) {  #matches covariate time series with track observations
#   path.list<- df.to.list(dat = path_df, ind = id)
#   track.list<- df.to.list(dat = track_df, ind = id)
#   
#   for (j in 1:length(path.list)) {
#     ind<- vector()
#     
#     for (i in 2:(nrow(path.list[[j]]) - 1)) {
#       if (path.list[[j]]$cell[i] == path.list[[j]]$cell[i+1])
#         ind<- c(ind, i)
#     }
#     ind<- c(1, ind, nrow(path.list[[j]]))
#     track.list[[j]]$time1<- ind
#   }
#   
#   track_df1<- bind_rows(track.list, .id="id")
#   track_df1
# }

#----------------------------
extract.covars = function(dat, layers) {
  
  path<- list()
  ind<- unique(dat$id)
  
  for (i in 1:n_distinct(dat$id)) {
    
    #Subset and prep data
    tmp<- dat %>% 
      dplyr::filter(id == ind[i]) %>% 
      dplyr::mutate(dt = difftime(date, dplyr::lag(date, 1), units = "secs")) %>% 
      dplyr::mutate_at("dt", {. %>% 
          as.numeric() %>%
          round()})
    tmp$dt<- c(purrr::discard(tmp$dt, is.na), NA)
    
    extr.covar<- data.frame()
    
    #Extract values from each line segment
    for (j in 2:nrow(tmp)) {
      segment<- tmp[(j-1):j, c("x","y")] %>%
        as.matrix() %>% 
        st_linestring() %>% 
        st_sfc(crs = projection(layers)) %>% 
        st_sf()
      
      tmp1<- raster::extract(layers, segment, along = TRUE, cellnumbers = FALSE) %>% 
        purrr::map(., ~matrix(., ncol = nlayers(layers))) %>% 
        map_dfr(., as_data_frame, .id = "seg.id") %>% 
        mutate(seg.id = j-1, dt = NA, id = ind[i], date = tmp$date[j-1])
      tmp1[nrow(tmp1),"dt"]<- as.numeric(tmp$dt[j-1])
      names(tmp1)[2:(1 + nlayers(layers))]<- names(layers)
      
      extr.covar<- rbind(extr.covar, tmp1)
    }
    
    #Store results from each ID
    path[[i]]<- extr.covar
  }
  
  names(path)<- ind
  if (length(path) > 1) {  #adjust seg.id so each is unique across all IDs
    for (i in 2:length(path)) {
      path[[i]]$seg.id<- path[[i]]$seg.id + max(path[[i-1]]$seg.id)
    }
  }
  path<- bind_rows(path)
  
  
  path
}
