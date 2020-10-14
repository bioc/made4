"asDataFrame" <-
function(indata) {      
        # To run ade4 the indata needs to be in a data.frame format.
        # Thanks samGG
        cc=class(indata)
        y <- NULL
        if (is.matrix(indata)) {
          if (!is.numeric(indata))
            stop("indata is a matrix, but is not numeric.")
             y <- data.frame(indata)
        } else if (is.data.frame(indata)) {
          if (!all(sapply(indata, is.numeric)))
            stop("indata is a data.frame, but contains non-numeric columns.")
            y <- indata
        } else if ("ExpressionSet" %in% class(indata)) {
          if (!"affy" %in% rownames(utils::installed.packages()))
            stop("affy package is required but not installed. Please install it.")
             y <- data.frame(affy::exprs(indata))
        } else if ("marrayRaw" %in% class(indata)) {
              nrslides = as.integer(ncol(indata@maRf))
              nrspots = as.integer(nrow(indata@maRf))
              tmp = matrix(NA, nrow = nrspots, ncol = 2 * nrslides)
              tmp[, (1:nrslides) * 2 - 1] = indata@maGf - indata@maGb
              tmp[, (1:nrslides) * 2] = indata@maRf - indata@maRb
              tmp.names = vector(mode = "character", length = 2 * nrslides)
              tmp.names[(1:nrslides) * 2 - 1] = paste("G",colnames(indata@maGf),sep="_")
              tmp.names[(1:nrslides) * 2] = paste("R",colnames(indata@maRf),sep="_")
              colnames(tmp) = tmp.names
              y <- as.data.frame(tmp)
        } else if (class(indata)%in% c("SummarizedExperiment","RangedSummarizedExperiment")) {
            y <- SummarizedExperiment::assay(indata)
        } else {
          stop(paste("indata has class ", paste(class(indata), sep = ","), ". None is handled.", sep=""))
        }
        return(y)
}

