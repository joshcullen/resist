extract.covars.internal = function(dat, layers, state.col, which_cat, dyn_names, ind) {
  ## dat = data frame containing at least the id, coordinates (x,y), and date-time (date)
  ## layers = a raster object (Raster, RasterStack, RasterBrick) object containing environ covars
  ## state.col = character. The name of the column that contains behavioral states w/in
  ##             dat (if present)
  ## which_cat = vector of names or numeric positions of discrete raster layers; NULL by default 
  ## dyn_names = vector of names dynamic raster layers (in same order as layers); NULL by default
  ## ind = character/integer. The name or column position of the indicator column of dat to be
  ##       matched w/ names of a dynamic raster
  
  
  #Subset and prep data
  tmp<- dat %>% 
    # dplyr::filter(id == ind[i]) %>% 
    dplyr::mutate(dt = difftime(date, dplyr::lag(date, 1), units = "secs")) %>% 
    dplyr::mutate_at("dt", {. %>% 
        as.numeric() %>%
        round()})
  tmp$dt<- c(purrr::discard(tmp$dt, is.na), NA)
  
  if (!is.null(dyn_names) & !is.factor(tmp[,ind])) stop("The `ind` column must be a factor.")
  
  
  
  #Identify levels of categorical layer (if available)
  if (!is.null(which_cat)) lev<- layers[[which_cat]]@data@attributes[[1]][,1]
  
  extr.covar<- data.frame()
  
  #Extract values from each line segment
  for (j in 2:nrow(tmp)) { 
    # print(j)
    segment<- tmp[(j-1):j, c("x","y")] %>%
      as.matrix() %>% 
      st_linestring() %>% 
      st_sfc(crs = projection(layers)) %>% 
      st_sf()
    
    tmp1<- raster::extract(layers, segment, along = TRUE, cellnumbers = FALSE) %>% 
      purrr::map(., ~matrix(., ncol = nlayers(layers))) %>% 
      purrr::map_dfr(., as.data.frame, .id = "seg.id") %>% 
      dplyr::mutate(seg.id = j-1, dt = NA, id = unique(dat$id), date = tmp$date[j-1],
                    state = tmp[j-1,state.col]) %>% 
      as.data.frame()
    
    tmp1[nrow(tmp1),"dt"]<- as.numeric(tmp$dt[j-1])
    names(tmp1)[2:(1 + nlayers(layers))]<- names(layers)
    
    #subset to only include time-matched vars (by some indicator variable)
    cond<- tmp[j-1, ind]
    cond2<- levels(cond)[which(cond != levels(cond))]
    tmp1<- tmp1[,!stringr::str_detect(names(tmp1), paste(cond2, collapse="|"))]
    
    ind1<- stringr::str_which(names(tmp1), as.character(cond))
    names(tmp1)[ind1]<- dyn_names
      
    extr.covar<- rbind(extr.covar, tmp1)
  }
  
  # p()  #plot progress bar
  
  extr.covar
}

#----------------------------
extract.covars = function(data, layers, state.col = NULL, which_cat = NULL, dyn_names = NULL,
                          ind) {
  ## data must be a data frame with "id" column, coords labeled "x" and "y" and datetime as POSIXct labeled "date"; optionally can have column that specifies behavioral state
  
  dat.list<- bayesmove::df_to_list(data, "id")
  
  #set up progress bar
  p<- progressr::progressor(steps = length(dat.list))
  
  
  tictoc::tic()
  path<- furrr::future_map(dat.list,
                           ~extract.covars.internal(dat = .x, layers = layers,
                                                    state.col = state.col,
                                                    which_cat = which_cat,
                                                    dyn_names = dyn_names, ind = ind),
                           .options = furrr_options(seed = TRUE))
  tictoc::toc()

  
  if (length(path) > 1) {  #adjust seg.id so each is unique across all IDs
    for (i in 2:length(path)) {
      path[[i]]$seg.id<- path[[i]]$seg.id + max(path[[i-1]]$seg.id)
    }
  }
  
  path<- dplyr::bind_rows(path)
  
  path
}
