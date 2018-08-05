# course_week:6.4  
# description: generate a random sample from an exponential distribtution and show equivalence of the estimated sampling statistics to their theoretical values.
# make generic: no


sampling <- function() {
    r <- round(rexp(40000, rate = 0.2), 4)
    rmat <- matrix(r, 1000, 40)
    sampling <- apply(rmat, 1, mean)
    r.mean <- mean(r)
    m <- mean(sampling)
    # show equivalence to population mean
    r.var <- var(r)
    v <- var(sampling)
    # do the transformation for the sampling variance and announce the difference between the two.
    qplot(r)
    qplot(sampling)
}


# course_week: 6.4 
# description: explore a basic dataset and manually calculate t-values...? (seems incomplete, just a scratch/prep file)
# make generic: no


exploring <- function () {
    library(datasets)
    tg <- ToothGrowth
    dim(tg)
    summary(tg)
    unique(tg$dose)
    tg$dose <- as.factor(tg$dose)
    summary(tg)
    # split the data by supp and dose
    # Use two-tailed independent t-test with unequal variance to compare means
    supp <- split(tg, tg$supp)
    dose <- split(tg, tg$dose)
    # plot t-distributions around the mean and sample variances of each factor. Put them all on one plot. Add lines for alpha values.
    # use ggplot for this.
    ms <- tapply(tg$len, tg$supp, mean)
    vs <- tapply(tg$len, tg$supp, var)
    md <- tapply(tg$len, tg$dose, mean)
    vd <- tapply(tg$len, tg$dose, var)
    # compare means using 95% confidence intervals
    # supp
    # show that the number of rows are equal between partitions
    supp.int <- ms - qt(.95, nrow(supp[[1]]) - 1, lower.tail = TRUE) * sqrt(vs/nrow(supp[[1]]))
    supp.int <- rbind(supp.int, ms + qt(.95, nrow(supp[[1]]) - 1, lower.tail = TRUE) * sqrt(vs/nrow(supp[[1]])))
    rownames(supp.int) <- NULL
    # dose
    # show that the number of rows are equal between partitions
    dose.int <- md - qt(.95, nrow(dose[[1]]) - 1, lower.tail = TRUE) * sqrt(vd/nrow(dose[[1]]))
    dose.int <- rbind(dose.int, md + qt(.95, nrow(dose[[1]]) - 1, lower.tail = TRUE) * sqrt(vd/nrow(dose[[1]])))
    rownames(dose.int) <- NULL
    
}