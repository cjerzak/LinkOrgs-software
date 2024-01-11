#!/usr/bin/env Rscript
#' RestoreML
#'
#' A function, primarily for internal used, used to initialize the machine learning models used in the record linkage algorithms of Libgober and Jerzak.
#'
#' @usage
#'
#' RestoreML(...)
#'
#'
#'
#' @export
#'
#' @md


RestoreML <- function(){
  UseDropboxVersion <- T
  
  print("Parameter sample at initialization:")
  print(  head( as.array(trainingVars[[length(trainingVars)-1]])) )
  print(  head( as.array(trainingVars[[length(trainingVars)]])) )

  # make temporary file 
  temp2 <- tempfile(pattern = pattern_temp2 <- "tmp14323512321423231961")
  tempdir <- tempdir("tmp14323512321423231961_dir")
  
  #if("tf_saved_checkpoint" %in% list.files("./")){ 
  if(!UseDropboxVersion){ 
    #tf_manager_path <- "./tf_saved_checkpoint"
    tf_otherstates_path <- "./tf_saved_checkpoint_otherstates"
  }
  
  #if(!"tf_saved_checkpoint" %in% list.files("./")){ 
  if(UseDropboxVersion){ 
    #raw_url_tf <- "https://www.dropbox.com/s/wy5924kvq057ph0/tf_saved_checkpoint.zip?dl=0"
    raw_url_tf <-  'https://www.dropbox.com/s/i68dw2zcd4akvy3/tf_checkpoints_jan252023.zip?dl=0'
    raw_url_tf <- gsub(raw_url_tf, pattern="https://www.dropbox.com",
                       replace="https://dl.dropboxusercontent.com")
    
    # download .zip contain checkpoint and unzip
    download.file(raw_url_tf, destfile = temp2)
    unzipped <- unzip(temp2,
          junkpaths = T,
          exdir = tempdir)
          #exdir = sprintf("%s/extracted",temp2) )
    #list.files(tempdir)
    list.files("/var/folders/60/8z8nvk5x4sxf3lqx2_6w748w0000gn/T//RtmpoyHTzu/")

    #tf_manager_path <- paste(tempdir,"/tf_saved_checkpoint",sep="")
    tf_otherstates_path <- tempdir#paste(tempdir,"/tf_saved_checkpoint_otherstates",sep="")
    #tf_manager_path <- paste(pattern_temp2,"/tf_saved_checkpoint",sep="")
    #tf_otherstates_path <- paste(pattern_temp2,"/tf_saved_checkpoint_otherstates",sep="")
    ls()
  }
  
  # set up manager that handles restoration 
  #tf_manager = tf$train$CheckpointManager(tf_ckpt, directory = tf_manager_path, max_to_keep = 1)
  
  # restore 
  #tf_manager$restore_or_initialize()
  
  # (re) initialize variable states 
  for(i_ in 1:length(trainingVars)){ 
    try(eval(parse(text = sprintf("
            try(load('%s/TV_%s.Rdata'),T)", 
                                  tf_otherstates_path, i_))),T)
    trainingVars[[i_]]$assign(saveArray)
  } 
  
  # (re) initialize other states 
  for(BN_layer in BN_layers){
    try(eval(parse(text = sprintf("state_ <- 
            try(read.csv(file = '%s/%s.csv'),T)",tf_otherstates_path, BN_layer))),T)
    
    if(class(state_)!= "try-error"){ 
      reinit_mean <- state_$moving_mean
      reinit_var <- state_$moving_variance
      if(length(reinit_var)==1){ 
        reinit_mean <- tf$expand_dims(reinit_mean,0L)
        reinit_var <- tf$expand_dims(state_$moving_variance,0L)
      }
      eval(parse(text=sprintf("try(%s$moving_mean$assign( reinit_mean    ),T)", BN_layer)))
      eval(parse(text=sprintf("try(%s$moving_variance$assign( reinit_var    ),T)",BN_layer)))
    }
  }

  print("Parameter sample post-training:")
  print(  head( as.array(trainingVars[[length(trainingVars)]]))  )
}
