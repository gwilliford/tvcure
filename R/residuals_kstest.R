
# library(goftte) - code from goftte 1.0.5
# TO undersand KS Test
 #https://rdrr.io/cran/goftte/man/goftte-package.html

prop.coxph <- function(model, varnames = NULL, type.test = c("Lin", "Liu"),
                         R = 1000, plots = min(R,50), seed = NULL,
                         ...) {

  if(type.test == "Lin") type.test.num = 1
  if(type.test == "Liu") type.test.num = 2
  if (!is.null(seed)) set.seed(seed)

  mt = model.frame(model)
  Y = model.extract(mt, "response")
  if (attr(Y, "type") == "right") {
    Time = Y[, "Time"];
    Status = Y[, "Status"]
  } else stop("Expected right-censored data.");
  X = X

  ot = order(Time)
  Time = Time[ot]
  n = length(Time)

  Status = Status[ot]
  nd = sum(Status)
  nc = sum(Status == 0)
  index.dtimes = (1:n)[Status == 1]
  dtimes = Time[index.dtimes]
  index.censtimes = (1:n)[Status == 0]
  censtimes = Time[index.censtimes]

  X = X[ot, , drop = F]
  p = ncol(X)

  idxtime = which(Time == Time)
  otime = cbind(Time, idxtime)
  otime = otime[!duplicated(otime[, 1]) ,]
  index.otime = otime[,2]
  otime = otime[, 1];
  m = length(index.otime)

  #Ties
  # if ( n > m & model$method != "breslow")
  #   warning("In case of ties, use breslow method in coxph")

  beta = model$beta
  if (!is.null(varnames) & length(varnames) != p)
    stop("Varnames must have same length than number of variables in model")}
  if (is.null(varnames)) varnames = na.omit(colnames(X))
  variable = unique(varnames)
  UsedData = X[, na.omit(match(varnames, colnames(X))),drop = FALSE]
  #myvars = varnames
  varnames.idx = 1:p

  output = .C("coxscoreW",
               R = as.integer(R),
               n = as.integer(n),
               m = as.integer(m),
               nd = as.integer(nd),
               nc = as.integer(nc),
               p = as.integer(p),
               seed = seed,
               beta_data = as.double(beta),
               time_data = as.double(Time),
               index_otime_data = as.integer(index.otime-1),
               index_dtimes_data = as.integer(index.dtimes-1),
               index_censtimes_data = as.integer(index.censtimes-1),
               X_data = as.double(X),
               plotnum = as.integer(plots),
               type_test_num = as.integer(type.test.num),
               KS = as.double(numeric(p)),
               CvM = as.double(numeric(p)),
               AS = as.double(numeric(p)),
               Wsd = as.double(numeric(p*m)),
               cvalues = as.double(numeric(p*R)),
               Ws = as.double(numeric(p*m*plots)),
               W = as.double(numeric(p*m)),
               pkg="goftte"
  )

  UsedVars = W = Wsd = What = KS = CvM = AS = allcvalues = x = mytype = c()
  mytype = "prop"
  KS = output$KS
  CvM = output$CvM
  AS = output$AS
  W = array(output$W, dim = c(m,1,p))
  What = array(output$Ws, dim = c(m,plots,p))
  allcvalues = array(output$cvalues,dim = c(R,1,p))
  Wsd = array(output$Wsd,dim = c(m,1,p))
  x = array(rep(otime,p),dim = c(m,1,p))

  res = list(W = W, What = What,
              obs = x,
              KS = KS, CvM = CvM, AD = AS,
              cvalues = allcvalues, varnames = varnames,
              R = R, sd = Wsd, type = mytype, model="coxph", type.test = type.test,assumption="proportional hazards assumption")
  class(res) = "scproc"
  res
}
