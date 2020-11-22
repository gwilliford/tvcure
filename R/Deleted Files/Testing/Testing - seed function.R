nboot = 10
n1 <- 500
n0 <- 300

r1 <- foreach(i=1:4, .options.RNG=1234) %dorng% { sample(10, 1:10) }
r2 <- foreach(i=1:4, .options.RNG=1234) %dorng% { sample(10, 1:10) }

library(doRNG)
stopCluster(cl); cl <- makeCluster(4, "SOCK"); registerDoSNOW(cl); clusterSetupRNG(cl, seed=rep(1,6))
set.seed(5)
cp <- tvcure(Surv(start, stop, event) ~ lndeaths + tie,
             cureform = ~ battletide + thirdpartycfire + tie,
             data = lhr, nboot = 10)
a <- prediction3(cp, "tie", c(0, 1), "uncureprob")
a <- prediction3(cp, "tie", c(0, 1), "spop")
a <- prediction3(cp, "tie", c(0, 1), "spop")



cp2 <- tvcure(Surv(start, stop, event) ~ lndeaths + tie,
              cureform = ~ battletide + thirdpartycfire + tie,
              data = lhr, nboot = 10)
summary(cp); summary(cp2)





clusterApply(cl, 1:4, \
             function(x) { a(x)) } ))
a <- function(nboot) {foreach(i-1:nboot, .packages = c('survival','brglm', 'brglm2'),
                   .errorhandling = 'remove', nboot, n1, n0, .combine = cbind) %dopar%
                    out <- sample(1:i, 10)
                    return(out)
}

a <- foreach(i=1:nboot, .packages = c('survival','brglm', 'brglm2'),
              .errorhandling = 'remove', .combine = c) %dopar%
  (print(i))
    out <- sample(1:i, 10)
    return(out)
}
}
}


m <- matrix(rnorm(9), 3, 3)
foreach(i=1:nrow(m), .combine=rbind) %dopar%
  (m[i,] / mean(m[i,]))



                   #   foreach(i in 1:nboot) %dorng% {
                   #     try({
                   #       id1 <- sample(1:n1, n1, replace = T)
                   #       id0 <- sample(1:n0, n0, replace = T)
                   #     })
                   #     list(id1 = id1, id0 = id0)
                   #   }
#                    # }
#
# list(id1 = id1, id0 = id0)

b <- foreach(i = 1:nboot, .packages = c('survival','brglm', 'brglm2'),
             .errorhandling = 'remove', nboot, n1, n0) %dorng% {
               foreach(i in 1:nboot) %dorng% {
                 try({
                   id1 <- sample(1:n1, n1, replace = T)
                   id0 <- sample(1:n0, n0, replace = T)
                 })
               }
               list(id1 = id1, id0 = id0)

# b <- foreach(i = 1:nboot, .packages = c('survival','brglm', 'brglm2'),
#              .errorhandling = 'remove', nboot, n1, n0) %dorng% {
#                for(i in 1:nboot) {
#                  try({
#                    id1 <- sample(1:n1, n1, replace = T)
#                    id0 <- sample(1:n0, n0, replace = T)
#                  })
#                }
#                list(id1 = id1, id0 = id0)
#              }
a[[1]]; b[[1]]
                       #   bootdata <- rbind(data1[id1, ], data0[id0, ])
                       #   bootZ <- bootdata[, gnames]
                       #   bootX <- as.matrix(cbind(bootdata[, bnames]))
                       #   if (survtype=="right") {
                       #     bootsurv <- Surv(bootdata[, 1], bootdata[, 2])
                       #     boottime <- bootdata[, 1]
                       #     bootstatus <- bootdata[, 2]
                       #   }
                       #   if (survtype=="counting") {
                       #     bootsurv <- Surv(bootdata[, 1], bootdata[, 2], bootdata[, 3])
                       #     bootstart <- bootdata[, 1]
                       #     boottime <- bootdata[, 2]
                       #     bootstatus <- bootdata[, 3]
                       #   }
                       #
                       #   bootfit <- tvem(Time = boottime, Status = bootstatus,f
                       #                   X = bootX, Z = bootZ, offset,
                       #                   gamma, beta, link, emmax,
                       #                   eps, brglm, firthcox,
                       #                   survobj = bootsurv, survtype, method)#, error = function(e) NULL)
                       #   break
                       # }, silent = F) #close try function
                     # } # close for loop
                   } # close foreach loop
