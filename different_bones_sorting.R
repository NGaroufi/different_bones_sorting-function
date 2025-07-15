## Copyright (C) 2023 Nefeli Garoufi <nefeligar@biol.uoa.gr>


# Package installing and library loading
list.of.packages <- c("readr", "caret", "e1071", "dplyr")
install.packages(list.of.packages, quiet = TRUE)

suppressMessages(suppressWarnings(invisible(lapply(list.of.packages, require, 
                                                   character.only = TRUE))))

db_sorting <- function(bone_1, bone_2, side, distance, threshold_value, ground_truth)
{
  # Setting working director
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  
  if (ground_truth == TRUE)
  {
  
    # Data loading and prep
    data_1 <- read.csv(file.choose(new=TRUE))
    data_2 <- read.csv(file.choose(new=TRUE))
    
    if (bone_1 == "femur" && bone_2 == "tibia" || bone_1 == "tibia" & bone_2 == "femur")
    {
      data_1 <- data_1[,-c(3:7, 12, 15, 20, 23, 31, 39, 47)]
      data_2 <- data_2[,-c(3:7, 12, 15, 20, 23, 31, 39, 47)]
    } else if (bone_1 == "femur" && bone_2 == "humerus" || bone_1 == "humerus" & bone_2 == "femur")
    {
      data_1 <- data_1[,-c(3:7, 15, 23, 31, 36, 47)] 
      data_2 <- data_2[,-c(3:7, 15, 23, 31, 36, 47)] 
    } else if (bone_1 == "tibia" && bone_2 == "humerus" || bone_1 == "humerus" & bone_2 == "tibia")
    {
      data_1 <- data_1[,-c(3:7, 15, 31, 47)]
      data_2 <- data_2[,-c(3:7, 15, 31, 47)]
    }
    
    
    # Welcoming message
    print("Hello!")
    print(paste0("You are working with ", nrow(data_1), " ", bone_1, " and ",
                  nrow(data_2), " ", bone_2, " ", "bones."))
    
    # Functions needed
    which.mins <- function(x, mins=6) {
      head(order(x), mins)
    }
    mae1 <- function(o,p,m=T) {
      mean(abs(o-p),na.rm=m)
    }
    
    # Dataset prep
    
    cols <- ncol(data_1)
    
    var_names <- colnames(data_1[2:cols])
    
    sample_ids_1 <- data_1[,1]
    sample_ids_2 <- data_2[,1]
    
    single_1 <- setdiff(substr(sample_ids_1, 1, 6), substr(sample_ids_2, 1, 6))
    single_2 <- setdiff(substr(sample_ids_2, 1, 6), substr(sample_ids_1, 1, 6))
    
    y <- 0
    x <- 0
    
    for (i in 1:(cols-1))
    {
      y[i] <- paste0("Y",i)
      x[i] <- paste0("X",i)
    }
    
    colnames(data_1)[2:cols] <- y
    colnames(data_2)[2:cols] <- x
    
    print("Stop 1")
    
    # Predicting the bone_1 variables
    class_1 <- readRDS(paste0("./files/", bone_1, "_", bone_2, "_", side, "_model.rds"))
    print("Stop 1.5")
    vars <- y
    predY <- 0
       for (i in 1:(cols-1))
          {
            mlm1 <- class_1[[i]]
            print(i)
            pred <- predict(mlm1, newdata = data_2[-1])
            predY <- cbind(predY, pred)
          }
    
    pred_1 <- predY[,-1]
    colnames(pred_1) <- vars
          
    remove(predY, vars)
    
    print("Stop 2")
    
    # Predicting the bone_2 variables
    class_2 <- readRDS(paste0("./files/", bone_2, "_", bone_1, "_", side, "_model.rds"))
    vars <- x
    predY <- 0
      for (i in 1:(cols-1))
          {
            mlm2 <- class_2[[i]]
            
            pred <- predict(mlm2, newdata = data_1[-1])
            predY <- cbind(predY, pred)
          }
          
    pred_2 <- predY[,-1]
    colnames(pred_2) <- vars
    
    print("Stop 3")
    
    # Loading the thresholds
    thresholds <- read.csv(paste0("./files/", bone_1, "_", bone_2, "_", side, "_thr_", threshold_value, ".csv"))
    u_thr_1 <- as.numeric(thresholds[1,-1])
    l_thr_1 <- as.numeric(thresholds[2,-1])
    u_thr_2 <- as.numeric(thresholds[3,-1])
    l_thr_2 <- as.numeric(thresholds[4,-1])
    
    print("Stop 4")
    
    #Sorting
            
    # Bone 1 - Minimum five
    pr_label<-0
    pr_idx <-0
    excluded <- 0
    false_neg <- 0
    
    plausible <- matrix(0, nrow=nrow(data_1), ncol=nrow(data_1)-5)
            
    five_pr<-matrix(0, nrow=nrow(data_1), ncol=6)
            
    for (i in 1:nrow(data_1))
        {
      
          pr_sample <- 0
          pr_mm <- 0
          el <- 0
          mism <- 0
          fn <- 0
          
          dif_1 <- matrix(0, nrow=nrow(pred_1), ncol=(cols-1))
          for (d in 1:nrow(pred_1))
          {
            x <- abs(data_1[i, 2:cols] - pred_1[d,])
            dif_1[d, ] <- as.numeric(x)
          }
          
          row.names(dif_1) <- row.names(pred_1)
          
          for (k in 1:nrow(dif_1))
          {
            y_idx <-0
            #print(k)
            for (j in 1:(cols-1))
            {
              if (between(dif_1[k,j], l_thr_1[j], u_thr_1[j]))
              {
                y_idx <- y_idx + 1
              }
            }
            if (y_idx == (cols-1))
            {
              pr_idx <- as.numeric(row.names(dif_1)[k])
              pr_sample[k] <- sample_ids_2[pr_idx]
              el[k] <- k
            } else { 
              pr_idx_mm <- as.numeric(row.names(dif_1)[k])
              pr_mm[k] <- sample_ids_2[pr_idx_mm]
              mism <- mism + 1
            }
          }
          
          p_id <- as.numeric(row.names(data_1)[i])
          
          pr_mm_id <- substr(pr_mm, 1, 6)
          
          if (substr(sample_ids_1[p_id], 1, 6) %in% pr_mm_id)
          {
            fn <- fn + 1
            true_neg <- mism - 1
          } else {true_neg <- mism}
          
          pr_sample <- pr_sample[!is.na(pr_sample)]
          el <- el[!is.na(el)]
          
          el_pred_1 <- pred_1[el, ]
          
          true <- data_1[i, 2:cols]
          name <- rownames(true)
          rownames(true) <- c("true")
          
          vec <- rbind(true, el_pred_1)
          y <- as.matrix(dist(vec, method = distance, p=1.5))
          
          g <- which.mins(y[,1])
          
          suppressWarnings(pr_idx <- as.numeric(names(y[g,1])))
          
          name <- c(name, rownames(y[-1,]))
          
          excluded[i] <- true_neg
          false_neg[i] <- fn
          
          plausible[i,] <- c(pr_sample[-g], 
                             rep(0, times=ncol(plausible)-length(pr_sample[-g])))
          
          
          if (length(five_pr[i,]) == length(sample_ids_2[pr_idx]))
          {
            five_pr[i,] <-sample_ids_2[pr_idx]
          } else {five_pr[i,] <- c(sample_ids_2[pr_idx], 
                                   rep(0, times=6-length(sample_ids_2[pr_idx])))}
          
          remove(true, vec, y)
    }
    
    stats <- c(nrow(data_1), sum(excluded),
               sum(excluded)/(nrow(pred_1)*(nrow(data_1)-1))*100, sum(false_neg))
    names(stats) <- c("Sample size", "# of Excluded",
                      "TNR", "# of False Negatives")
    
    write.csv(stats, paste0("stats_", bone_1, "_", distance ,
                            "_", theshold_value,".csv"))
            
    five_pr<-cbind(data_1[,1], five_pr)
            
    five_pr <- five_pr[,-2]
    colnames(five_pr)<-c("Observation #", "1st Choice", "2nd Choice", 
                          "3rd Choice", "4th Choice", "5th Choice")
            
    five_pr_1 <- five_pr
    
    write.csv(five_pr_1, paste0("pred_", bone_1, "_", distance ,
                                "_", theshold_value,".csv"))
    
    plausible <- cbind(data_1[,1], plausible)
    write.csv(plausible, paste0("plausible_", bone_1, "_", distance ,
                                "_", theshold_value,".csv"))
    
    write.csv(five_pr_1, paste0("pred_", bone_1, "_", distance ,
                                "_", theshold_value,".csv"))
    
    single_elements <- five_pr_1[five_pr_1[,2]==0,1]
    se_1 <- sum(single_elements %in% single_1)
    
    print("Stop 5")
    
    # Bone 2 - Minimum five
    pr_label<-0
    pr_idx <-0
    excluded <- 0
    false_neg <- 0
            
    five_pr<-matrix(0, nrow=nrow(data_2), ncol=6)
    
    plausible <- matrix(0, nrow=nrow(data_2), ncol=nrow(data_2)-5)
    
    for (i in 1:nrow(data_2))
        {
      pr_sample <- 0
      pr_mm <- 0
      el <- 0
      mism <- 0
      fn <- 0
      
      dif_2 <- matrix(0, nrow=nrow(pred_2), ncol=(cols-1))
      for (d in 1:nrow(pred_2))
      {
        x <- abs(data_2[i, 2:cols] - pred_2[d,])
        dif_2[d, ] <- as.numeric(x)
      }
      
      row.names(dif_2) <- row.names(pred_2)
      
      for (k in 1:nrow(dif_2))
      {
        y_idx <-0
        #print(k)
        for (j in 1:(cols-1))
        {
          if (between(dif_2[k,j], l_thr_2[j], u_thr_2[j]))
          {
            y_idx <- y_idx + 1
          }
        }
        if (y_idx == (cols-1))
        {
          pr_idx <- as.numeric(row.names(dif_2)[k])
          pr_sample[k] <- sample_ids_1[pr_idx]
          el[k] <- k
        } else { 
          pr_idx_mm <- as.numeric(row.names(dif_2)[k])
          pr_mm[k] <- sample_ids_1[pr_idx_mm]
          mism <- mism + 1
        }
      }
      
      p_id <- as.numeric(row.names(data_2)[i])
      
      pr_mm_id <- substr(pr_mm, 1, 6)
      
      if (substr(sample_ids_2[p_id], 1, 6) %in% pr_mm_id)
      {
        fn <- fn + 1
        true_neg <- mism - 1
      } else {true_neg <- mism}
      
      pr_sample <- pr_sample[!is.na(pr_sample)]
      el <- el[!is.na(el)]
      
      el_pred_2 <- pred_2[el, ]
      
      true <- data_2[i, 2:cols]
      name <- rownames(true)
      rownames(true) <- c("true")
      
      vec <- rbind(true, el_pred_2)
      y <- as.matrix(dist(vec, method = distance, p=1.5))
      
      g <- which.mins(y[,1])
      
      suppressWarnings(pr_idx <- as.numeric(names(y[g,1])))
      
      name <- c(name, rownames(y[-1,]))
      
      excluded[i] <- true_neg
      false_neg[i] <- fn
      
      plausible[i,] <- c(pr_sample[-g], 
                         rep(0, times=ncol(plausible)-length(pr_sample[-g])))
      
      
      if (length(five_pr[i,]) == length(sample_ids_1[pr_idx]))
      {
        five_pr[i,] <-sample_ids_1[pr_idx]
      } else {five_pr[i,] <- c(sample_ids_1[pr_idx], 
                               rep(0, times=6-length(sample_ids_1[pr_idx])))}
      
      remove(true, vec, y)
    }
            
    stats <- c(nrow(data_2), sum(excluded),
               sum(excluded)/(nrow(pred_2)*(nrow(data_2)-1))*100, sum(false_neg))
    names(stats) <- c("Sample size", "# of Excluded",
                      "TNR", "# of False Negatives")
    
    write.csv(stats, paste0("stats_", bone_2, "_", distance ,
                            "_", theshold_value,".csv"))
    
    five_pr<-cbind(data_2[,1], five_pr)
    
    five_pr <- five_pr[,-2]
    colnames(five_pr)<-c("Observation #", "1st Choice", "2nd Choice", 
                         "3rd Choice", "4th Choice", "5th Choice")
    
    five_pr_2 <- five_pr
    
    write.csv(five_pr_2, paste0("pred_", bone_2, "_", distance ,
                                "_", theshold_value,".csv"))
    
    plausible <- cbind(data_2[,1], plausible)
    write.csv(plausible, paste0("plausible_", bone_2, "_", distance ,
                                "_", theshold_value,".csv"))
    
    single_elements <- five_pr_2[five_pr_2[,2]==0,1]
    se_2 <- sum(single_elements %in% single_1)
    
    plausible <- cbind(data_2[,1], plausible)
    write.csv(plausible, paste0("plausible_", bone_2, "_", distance ,
                                "_", theshold_value,".csv"))
        
    write.csv(five_pr_2, paste0("pred_", bone_2, "_", distance ,
                                "_", theshold_value,".csv"))
    
    # Single Elements
    
    singles <- cbind(se_1, se_2)
    colnames(singles) <- c(bone_1, bone_2)
    
    write.csv(singles, paste0("singles_", bone_1, "_", bone_2, "_", distance, 
                              "_", theshold_value, ".csv"))
    
  } else if (ground_truth == FALSE)
    {
      data_1 <- read.csv(file.choose(new=TRUE))
      data_2 <- read.csv(file.choose(new=TRUE))
      
      if (bone_1 == "femur" && bone_2 == "tibia" || bone_1 == "tibia" & bone_2 == "femur")
      {
        data_1 <- data_1[,-c(3:7, 12, 15, 20, 23, 31, 39, 47)]
        data_2 <- data_2[,-c(3:7, 12, 15, 20, 23, 31, 39, 47)]
      } else if (bone_1 == "femur" && bone_2 == "humerus" || bone_1 == "humerus" & bone_2 == "femur")
      {
        data_1 <- data_1[,-c(3:7, 15, 23, 31, 36, 47)] 
        data_2 <- data_2[,-c(3:7, 15, 23, 31, 36, 47)] 
      } else if (bone_1 == "tibia" && bone_2 == "humerus" || bone_1 == "humerus" & bone_2 == "tibia")
      {
        data_1 <- data_1[,-c(3:7, 15, 31, 47)]
        data_2 <- data_2[,-c(3:7, 15, 31, 47)]
      }
      
      
      # Welcoming message
      print("Hello!")
      print(paste0("You are working with ", nrow(data_1), " ", bone_1, " and ",
                   nrow(data_2), " ", bone_2, " ", "bones."))
      
      # Functions needed
      which.mins <- function(x, mins=6) {
        head(order(x), mins)
      }
      mae1 <- function(o,p,m=T) {
        mean(abs(o-p),na.rm=m)
      }
      
      # Dataset prep
      
      cols <- ncol(data_1)
      
      var_names <- colnames(data_1[2:cols])
      
      sample_ids_1 <- data_1[,1]
      sample_ids_2 <- data_2[,1]
      
      # single_1 <- setdiff(sample_ids_1, sample_ids_2)
      # single_2 <- setdiff(sample_ids_2, sample_ids_1)
      
      y <- 0
      x <- 0
      
      for (i in 1:(cols-1))
      {
        y[i] <- paste0("Y",i)
        x[i] <- paste0("X",i)
      }
      
      colnames(data_1)[2:cols] <- y
      colnames(data_2)[2:cols] <- x
      
      # Predicting the bone_1 variables
      class_1 <- readRDS(paste0("./files/", bone_1, "_", bone_2, "_", side, "_model.rds"))
      vars <- y
      predY <- 0
      for (i in 1:(cols-1))
      {
        mlm1 <- class_1[[i]]
        
        pred <- predict(mlm1, newdata = data_2[-1])
        predY <- cbind(predY, pred)
      }
      
      pred_1 <- predY[,-1]
      colnames(pred_1) <- vars
      
      remove(predY, vars)
      
      # Predicting the bone_2 variables
      class_2 <- readRDS(paste0("./files/", bone_2, "_", bone_1, "_", side, "_model.rds"))
      vars <- x
      predY <- 0
      for (i in 1:(cols-1))
      {
        mlm2 <- class_2[[i]]
        
        pred <- predict(mlm2, newdata = data_1[-1])
        predY <- cbind(predY, pred)
      }
      
      pred_2 <- predY[,-1]
      colnames(pred_2) <- vars
      
      
      # Loading the thresholds
      thresholds <- read.csv(paste0("./files/", bone_1, "_", bone_2, "_", side, "_thr_", threshold_value, ".csv"))
      u_thr_1 <- as.numeric(thresholds[1,-1])
      l_thr_1 <- as.numeric(thresholds[2,-1])
      u_thr_2 <- as.numeric(thresholds[3,-1])
      l_thr_2 <- as.numeric(thresholds[4,-1])
      
      
      #Sorting
      
      # Bone 1 - Minimum five
      pr_label<-0
      pr_idx <-0
      excluded <- 0
      #false_neg <- 0
      
      plausible <- matrix(0, nrow=nrow(data_1), ncol=nrow(data_1)-5)
      
      five_pr<-matrix(0, nrow=nrow(data_1), ncol=6)
      
      for (i in 1:nrow(data_1))
      {
        
        pr_sample <- 0
        #pr_mm <- 0
        el <- 0
        mism <- 0
        #fn <- 0
        
        dif_1 <- matrix(0, nrow=nrow(pred_1), ncol=(cols-1))
        for (d in 1:nrow(pred_1))
        {
          x <- abs(data_1[i, 2:cols] - pred_1[d,])
          dif_1[d, ] <- as.numeric(x)
        }
        
        row.names(dif_1) <- row.names(pred_1)
        
        for (k in 1:nrow(dif_1))
        {
          y_idx <-0
          #print(k)
          for (j in 1:(cols-1))
          {
            if (between(dif_1[k,j], l_thr_1[j], u_thr_1[j]))
            {
              y_idx <- y_idx + 1
            }
          }
          if (y_idx == (cols-1))
          {
            pr_idx <- as.numeric(row.names(dif_1)[k])
            pr_sample[k] <- sample_ids_2[pr_idx]
            el[k] <- k
          } else { 
            pr_idx_mm <- as.numeric(row.names(dif_1)[k])
            #pr_mm[k] <- sample_ids_2[pr_idx_mm]
            mism <- mism + 1
          }
        }
        
        p_id <- as.numeric(row.names(data_1)[i])
        
        # pr_mm_id <- substr(pr_mm, 1, 6)
        # 
        # if (substr(sample_ids_1[p_id], 1, 6) %in% pr_mm_id)
        # {
        #   fn <- fn + 1
        #   true_neg <- mism - 1
        # } else {true_neg <- mism}
        
        pr_sample <- pr_sample[!is.na(pr_sample)]
        el <- el[!is.na(el)]
        
        el_pred_1 <- pred_1[el, ]
        
        true <- data_1[i, 2:cols]
        name <- rownames(true)
        rownames(true) <- c("true")
        
        vec <- rbind(true, el_pred_1)
        y <- as.matrix(dist(vec, method = distance, p=1.5))
        
        g <- which.mins(y[,1])
        
        suppressWarnings(pr_idx <- as.numeric(names(y[g,1])))
        
        name <- c(name, rownames(y[-1,]))
        
        #excluded[i] <- true_neg
        #false_neg[i] <- fn
        excluded[i] <- mism
        
        plausible[i,] <- c(pr_sample[-g], 
                           rep(0, times=ncol(plausible)-length(pr_sample[-g])))
        
        
        if (length(five_pr[i,]) == length(sample_ids_2[pr_idx]))
        {
          five_pr[i,] <-sample_ids_2[pr_idx]
        } else {five_pr[i,] <- c(sample_ids_2[pr_idx], 
                                 rep(0, times=6-length(sample_ids_2[pr_idx])))}
        
        remove(true, vec, y)
      }
      
      stats <- c(nrow(data_1), sum(excluded))
      names(stats) <- c("Sample size", "# of Excluded")
      
      write.csv(stats, paste0("stats_", bone_1, "_", distance ,
                              "_", theshold_value,".csv"))
      
      five_pr<-cbind(data_1[,1], five_pr)
      
      five_pr <- five_pr[,-2]
      colnames(five_pr)<-c("Observation #", "1st Choice", "2nd Choice", 
                           "3rd Choice", "4th Choice", "5th Choice")
      
      five_pr_1 <- five_pr
      
      write.csv(five_pr_1, paste0("pred_", bone_1, "_", distance ,
                                  "_", theshold_value,".csv"))
      
      plausible <- cbind(data_1[,1], plausible)
      write.csv(plausible, paste0("plausible_", bone_1, "_", distance ,
                                  "_", theshold_value,".csv"))
      
      write.csv(five_pr_1, paste0("pred_", bone_1, "_", distance ,
                                  "_", theshold_value,".csv"))
      
      single_elements_1 <- five_pr_1[five_pr_1[,2]==0,1]
      write.csv(single_elements_1, paste0("singles_", bone_1, "_", distance ,
                                          "_", theshold_value, ".csv"))
      #se_1 <- sum(single_elements %in% single_1)
      
      # Bone 2 - Minimum five
      pr_label<-0
      pr_idx <-0
      excluded <- 0
      #false_neg <- 0
      
      five_pr<-matrix(0, nrow=nrow(data_2), ncol=6)
      
      plausible <- matrix(0, nrow=nrow(data_2), ncol=nrow(data_2)-5)
      
      for (i in 1:nrow(data_2))
      {
        pr_sample <- 0
        #pr_mm <- 0
        el <- 0
        mism <- 0
        #fn <- 0
        
        dif_2 <- matrix(0, nrow=nrow(pred_2), ncol=(cols-1))
        for (d in 1:nrow(pred_2))
        {
          x <- abs(data_2[i, 2:cols] - pred_2[d,])
          dif_2[d, ] <- as.numeric(x)
        }
        
        row.names(dif_2) <- row.names(pred_2)
        
        for (k in 1:nrow(dif_2))
        {
          y_idx <-0
          #print(k)
          for (j in 1:(cols-1))
          {
            if (between(dif_2[k,j], l_thr_2[j], u_thr_2[j]))
            {
              y_idx <- y_idx + 1
            }
          }
          if (y_idx == (cols-1))
          {
            pr_idx <- as.numeric(row.names(dif_2)[k])
            pr_sample[k] <- sample_ids_1[pr_idx]
            el[k] <- k
          } else { 
            pr_idx_mm <- as.numeric(row.names(dif_2)[k])
            #pr_mm[k] <- sample_ids_1[pr_idx_mm]
            mism <- mism + 1
          }
        }
        
        p_id <- as.numeric(row.names(data_2)[i])
        
       # pr_mm_id <- substr(pr_mm, 1, 6)
        
        # if (substr(sample_ids_2[p_id], 1, 6) %in% pr_mm_id)
        # {
        #   fn <- fn + 1
        #   true_neg <- mism - 1
        # } else {true_neg <- mism}
        
        pr_sample <- pr_sample[!is.na(pr_sample)]
        el <- el[!is.na(el)]
        
        el_pred_2 <- pred_2[el, ]
        
        true <- data_2[i, 2:cols]
        name <- rownames(true)
        rownames(true) <- c("true")
        
        vec <- rbind(true, el_pred_2)
        y <- as.matrix(dist(vec, method = distance, p=1.5))
        
        g <- which.mins(y[,1])
        
        suppressWarnings(pr_idx <- as.numeric(names(y[g,1])))
        
        name <- c(name, rownames(y[-1,]))
        
        #excluded[i] <- true_neg
        #false_neg[i] <- fn
        excluded[i] <- mism
        
        plausible[i,] <- c(pr_sample[-g], 
                           rep(0, times=ncol(plausible)-length(pr_sample[-g])))
        
        
        if (length(five_pr[i,]) == length(sample_ids_1[pr_idx]))
        {
          five_pr[i,] <-sample_ids_1[pr_idx]
        } else {five_pr[i,] <- c(sample_ids_1[pr_idx], 
                                 rep(0, times=6-length(sample_ids_1[pr_idx])))}
        
        remove(true, vec, y)
      }
      
      stats <- c(nrow(data_2), sum(excluded))
      names(stats) <- c("Sample size", "# of Excluded")
      
      write.csv(stats, paste0("stats_", bone_2, "_", distance ,
                              "_", theshold_value,".csv"))
      
      five_pr<-cbind(data_2[,1], five_pr)
      
      five_pr <- five_pr[,-2]
      colnames(five_pr)<-c("Observation #", "1st Choice", "2nd Choice", 
                           "3rd Choice", "4th Choice", "5th Choice")
      
      five_pr_2 <- five_pr
      
      write.csv(five_pr_2, paste0("pred_", bone_2, "_", distance ,
                                  "_", theshold_value,".csv"))
      
      plausible <- cbind(data_2[,1], plausible)
      write.csv(plausible, paste0("plausible_", bone_2, "_", distance ,
                                  "_", theshold_value,".csv"))
      
      single_elements_2 <- five_pr_2[five_pr_2[,2]==0,1]
      write.csv(single_elements_2, paste0("singles_", bone_2, "_", distance ,
                                          "_", theshold_value,".csv"))
      #se_2 <- sum(single_elements %in% single_1)
      
      plausible <- cbind(data_2[,1], plausible)
      write.csv(plausible, paste0("plausible_", bone_2, "_", distance ,
                                  "_", theshold_value,".csv"))
      
      write.csv(five_pr_2, paste0("pred_", bone_2, "_", distance ,
                                  "_", theshold_value,".csv"))
      
      # Single Elements
      # singles <- cbind(se_1, se_2)
      # colnames(singles) <- c(bone_1, bone_2)
      # 
      # write.csv(singles, paste0("singles_", distance, ".csv"))
    
  }
  
  sorted <- list(five_pr_1,five_pr_2)
  return(sorted)
}
                 