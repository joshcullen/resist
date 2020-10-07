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
extract.covars.internal = function(dat, layers, state.col) {
  ## dat = data frame containing at least the id, coordinates (x,y), and date-time
  ## layers = a RasterStack or RasterBrick object containing environ covars
  ## state.col = character. The name of the column that contains behavioral states w/in
  ##             dat (if present)
  
  
    #Subset and prep data
    tmp<- dat %>% 
      # dplyr::filter(id == ind[i]) %>% 
      dplyr::mutate(dt = difftime(date, dplyr::lag(date, 1), units = "secs")) %>% 
      dplyr::mutate_at("dt", {. %>% 
          as.numeric() %>%
          round()})
      # tmp$dt<- c(purrr::discard(tmp$dt, is.na), NA)
    
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
        purrr::map_dfr(., as_data_frame, .id = "seg.id") %>% 
        dplyr::mutate(seg.id = j-1, dt = NA, id = unique(dat$id), date = tmp$date[j-1],
               state = tmp[j-1,state.col]) %>% 
        as.data.frame()
      
      tmp1[nrow(tmp1),"dt"]<- as.numeric(tmp$dt[j-1])
      names(tmp1)[2:(1 + nlayers(layers))]<- names(layers)
      
      extr.covar<- rbind(extr.covar, tmp1)
    }
    
    extr.covar
}

#----------------------------
extract.covars = function(data, layers, state.col) {
  ## data must be a data frame with "id" column, coords labeled "x" and "y" and datetime as POSIXct labeled "date"; optionally can have column that specifies behavioral state
  
  dat.list<- bayesmove::df_to_list(data, "id")
  
  tictoc::tic()
  path<- furrr::future_map(dat.list, ~extract.covars.internal(dat = .x, layers = layers,
                                                       state.col = "state"),
                           .progress = TRUE, .options = future_options(seed = TRUE))
  tictoc::toc()
  
  
  if (length(path) > 1) {  #adjust seg.id so each is unique across all IDs
    for (i in 2:length(path)) {
      path[[i]]$seg.id<- path[[i]]$seg.id + max(path[[i-1]]$seg.id)
    }
  }
  
  path<- dplyr::bind_rows(path)
  
  path
}
