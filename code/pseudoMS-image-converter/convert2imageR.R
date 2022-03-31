convert2image <-
  function(file.name,
           mz.range = c(70, 1000),
           rt.range = c(50, 1000),
           mz.pixel = 1024,
           rt.pixel = 1024,
           noise.threshold = 500,
           output.path = ".",
           threads = 4,
           mz.shift = FALSE,
           mean = 1.12361,
           sd = 4.076444,
           rt.shift = FALSE,
           rt.diff = 10,
           int.shift = FALSE,
           int.times = 1.1) {
    library(tidyverse)
    dir.create(output.path, showWarnings = FALSE)
    
    file1 <- mzR::openMSfile(file.name)
    file2 <-
      MSnbase::readMSData(
        files = file.name,
        msLevel. = 1,
        mode = "onDisk",
        verbose = TRUE
      )
    
    file.name <- stringr::str_replace(string = file.name,
                                      pattern = ".mz[X]{0,1}ML",
                                      replacement = "")
    
    peaks <- ProtGenerics::peaks(object = file1)
    
    rt <- ProtGenerics::rtime(object = file2)
    
    rm(list = c("file1", "file2"))
    
    
    
    ####-------------------------------------------------------------------
    get_mz_shift <- function(mz = c(87.04406, 90.05499, 94.06512),
                             mean = 1.12361,
                             sd = 4.076444) {
      error <- rnorm(n = 10000, mean = mean, sd = sd)
      
      mz <- purrr::map(
        mz,
        .f = function(x) {
          temp_error <- sample(error, 1)
          y <- x - (ifelse(x < 400, 400, x) * temp_error) / 10 ^ 6
          y
        }
      )
      
      unname(unlist(mz))
      
    }
    
    
    
    ####remove spectra according to RT
    cat(crayon::green("RT shift and removing some spectra according to rt...\n"))
    
    ##RT shift
    if (rt.shift) {
      rt <- rt + rt.diff
    }
    
    ###according RT to remove same spectra
    remain.idx <- which(rt > rt.range[1] & rt < rt.range[2])
    peaks <- peaks[remain.idx]
    rt <- rt[remain.idx]
    
    rm(list = c("remain.idx"))
    
    ###bin function---------------------------------------------------------------------------
    bin_x <- function(x,
                      x.min = 60,
                      x.max = 1000,
                      pixel = 1024) {
      if (min(x) < x.min |
          max(x) > x.max) {
        stop(
          "The x.min must smaller than the min of x and the x.max must be bigger than the max of x.\n"
        )
      }
      
      index <- seq(x.min, x.max, length.out = pixel + 1)
      
      index1 <- index[-length(index)]
      index2 <- index[-1]
      
      index <- data.frame(index1,
                          index2,
                          cell = 1:length(index1),
                          stringsAsFactors = FALSE)
      
      cell <-
        sapply(x, function(x) {
          idx <- which((x - index$index1) >= 0 & (x - index$index2) < 0)[1]
          return(index$cell[idx])
        })
      
      return(cell)
    }
    
    
    ###mz int shift, bin mz
    temp_fun1 <- function(idx,
                          peaks,
                          mz.shift,
                          mean,
                          sd,
                          int.shift,
                          int.times,
                          get_mz_shift,
                          mz.range,
                          noise.threshold,
                          mz.pixel,
                          bin_x) {
      library(tidyverse, warn.conflicts = FALSE)
      x <- peaks[[idx]]
      if (mz.shift) {
        x[, 1] <- get_mz_shift(x[, 1], mean, sd)
      }
      
      if (int.shift) {
        x[, 2] <- x[, 2] * int.times
      }
      
      ##remove some spectra
      x <- x[x[, 1] >= mz.range[1] &
               x[, 1] <= mz.range[2], , drop = FALSE]
      if (nrow(x) == 0) {
        x <- NULL
      }
      x <- x[x[, 2] > noise.threshold, , drop = FALSE]
      x[, 2] <- log(x[, 2], 10)
      
      ###bin mz
      cell <- bin_x(
        x = x[, 1],
        x.min = mz.range[1],
        x.max = mz.range[2],
        pixel = mz.pixel
      )
      
      x <- data.frame(x, cell, stringsAsFactors = FALSE)
      
      colnames(x)[c(1, 2)] <- c("mz", "intensity")
      
      x <-
        x %>%
        dplyr::group_by(cell) %>%
        dplyr::mutate(intensity = sum(intensity)) %>%
        dplyr::ungroup() %>%
        dplyr::select(cell, intensity) %>%
        dplyr::distinct(cell, .keep_all = TRUE)
      
      all_cell <-
        data.frame(cell = c(1:mz.pixel),
                   stringsAsFactors = FALSE)
      
      dplyr::select(dplyr::left_join(all_cell, x, by = "cell"), -cell)
      
    }
    
    
    cat(
      crayon::green(
        "m/z and RT shift and removing some spectra according to m/z and bining m/z...\n"
      )
    )
    
    # system.time(
    peaks <-
      BiocParallel::bplapply(
        X = 1:length(peaks),
        FUN = temp_fun1,
        BPPARAM = BiocParallel::SnowParam(workers = threads,
                                          progressbar = TRUE),
        peaks = peaks,
        mz.shift = mz.shift,
        mean = mean,
        sd = sd,
        int.shift = int.shift,
        int.times = int.times,
        get_mz_shift = get_mz_shift,
        mz.range = mz.range,
        noise.threshold = noise.threshold,
        mz.pixel = mz.pixel,
        bin_x = bin_x
      )
    # )
    
    peaks <-
      do.call(cbind, peaks)
    
    colnames(peaks) <- rt
    
    peaks[is.na(peaks)] <- 0
    
    ##bin rt
    cat('Binning rt...\n')
    
    cell <- bin_x(x = unname(rt),
                  x.min = rt.range[1],
                  rt.range[2],
                  pixel = rt.pixel)
    
    
    
    peaks <-
      data.frame(cell, t(peaks), stringsAsFactors = FALSE) %>%
      dplyr::group_by(cell) %>%
      dplyr::mutate_if(is.numeric, sum) %>%
      dplyr::sample_n(1) %>%
      dplyr::ungroup() %>%
      dplyr::select(-cell) %>%
      t() %>%
      as.data.frame()
    
    save(peaks, file = file.path(output.path, file.name))
    png(
      filename = file.path(output.path, paste(file.name, ".png", sep = "")),
      width = rt.pixel,
      height = mz.pixel
    )
    par(mar = c(0, 0, 0, 0))
    image(t(as.matrix(peaks)),
          axes = FALSE,
          col = colorRampPalette(colors = c("white", "black"))(256))
    dev.off()
  }
