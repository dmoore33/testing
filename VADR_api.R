library(RPostgreSQL)
library(DBI)
library(dplyr)
library(RHRV)
library(zoo)
library(mgcv)
library(MARSS)
library(scales)
library(pracma)

#* @post /trends
TRENDS <- function(session_id) {
  #######################################################################
  #                       VADR API                                      #
  #######################################################################
  # Query the database according session_id
  session_id_query <- as.character(session_id)
  required_metrics <- "('heartrate_rr','heartrate','breathing_rate','temperature_skin','gsr')"
  pg <- DBI::dbDriver("PostgreSQL") 
  db <- DBI::dbConnect(drv = pg, 
                       user="eaidevmaster",
                       password="eaidevmaster",
                       host="eai-dev-rr.cbxpfybyjgtq.eu-west-1.rds.amazonaws.com",
                       dbname="eai")
  qr <- paste0("SELECT metric, value, EXTRACT(EPOCH FROM time AT TIME ZONE 'UTC') *1000 AS time FROM ts_combined WHERE session_id = '",session_id_query,"' and metric in ",required_metrics,";")
  
  api_data <- DBI::dbGetQuery(db, qr)
  DBI::dbDisconnect(db)
  ######################### HRV #########################################
  data_RR <- api_data %>%
    dplyr::filter(metric == "heartrate_rr") %>%
    dplyr::distinct(time,.keep_all = TRUE) %>%
    dplyr::arrange(time) %>%
    dplyr::mutate(Time = cumsum(value)/1000)
  if (nrow(data_RR) == 0) {
    stop("There is no RR data, the process aborted")
  }
  beat <- data_RR %>%
    dplyr::select(Time)
  hrv.data <-  RHRV::CreateHRVData()
  hrv.data <-  RHRV::SetVerbose(hrv.data, TRUE)
  hrv.data$Beat <- beat
  hrv.data$datetime <- as.POSIXct("1900-01-01 GMT") # default value
  hrv.data <-  RHRV::BuildNIHR(hrv.data) # extract HRV
  hrv.data <-  RHRV::FilterNIHR(hrv.data) # filter HRV outliers
  hrv.data <-  RHRV::InterpolateNIHR(hrv.data, freqhr = 4) # Linear or Spline interpolator for build the sample heart rate signal
  hrv.data <-  RHRV::CreateTimeAnalysis(hrv.data, size = 100,interval = 7.8125) # Creates data analysis structure for time analysis calculation
  hrv.data <-  RHRV::CreateFreqAnalysis(hrv.data) # Creates data analysis structure for frequency analysis calculations
  hrv.data <-  RHRV::CalculatePowerBand(hrv.data,indexFreqAnalysis= 1,size = 100, shift = 30, sizesp = 2048, type = "fourier") # Calculates power of the heart rate signal at ULF, VLF, LF and HF bands
  hrv.data <-  RHRV::CreateNonLinearAnalysis(hrv.data)
  #
  value <- hrv.data$FreqAnalysis[[length(hrv.data$FreqAnalysis)]]$HF # selects only the High Frequency power band
  value	<- as.vector(spline(value,n=100)$y)
  time <- seq(dplyr::first(data_RR$time),dplyr::last(data_RR$time), length.out = 100)
  data_HRVHF <- data.frame(time,value)
  HRVHF <- zoo::zoo(data_HRVHF$value, order.by = data_HRVHF$time)
  ######################### HR ##########################################
  data_HR <- api_data %>%
    dplyr::filter(metric == "heartrate") %>%
    dplyr::select(time,value) %>% 
    dplyr::distinct(time,.keep_all = TRUE) %>% 
    dplyr::arrange(time)
  if (nrow(data_HR) == 0) {
    value <- hrv.data$HR
    time <- seq(dplyr::first(data_RR$time),dplyr::last(data_RR$time), length.out = length(hrv.data$HR))
data_HR <- data.frame(time,value)
  }
  HR <- zoo::zoo(data_HR$value, order.by = data_HR$time)
  ######################### BR ##########################################
  data_BR <- api_data %>%
    dplyr::filter(metric == "breathing_rate") %>%
    dplyr::select(time,value) %>%
    dplyr::distinct(time,.keep_all = TRUE) %>% 
    dplyr::arrange(time)
  if (nrow(data_BR) == 0) {
    stop("There is no BR data, the process aborted")
  }
  BR <- zoo::zoo(data_BR$value, order.by = data_BR$time)
  ######################### ST ##########################################
  data_ST <- api_data %>%
    dplyr::filter(metric == "temperature_skin") %>%
    dplyr::select(time,value) %>%
    dplyr::distinct(time,.keep_all = TRUE) %>% 
    dplyr::arrange(time)
  if (nrow(data_ST) == 0) {
    stop("There is no ST data, the process aborted")
  }
  ST <- zoo::zoo(data_ST$value, order.by = data_ST$time)
  ######################### SCL/SCR ##########################################
  data_GSR <- api_data %>%
    dplyr::filter(metric == "gsr") %>%
    dplyr::select(time,value) %>%
    dplyr::distinct(time,.keep_all = TRUE) %>% 
    dplyr::arrange(time)
  if (nrow(data_GSR) == 0) {
    stop("There is no GSR data, the process aborted")
  }
  data_GSR_zoo <- zoo::zoo(data_GSR$value,data_GSR$time) # zoo format
  data_GSR_st <- stl(ts(data_GSR_zoo, frequency=100), "periodic") # seasonal decomposition of GSR data
  data_GSR$SCL <- as.numeric(data_GSR_st$time.series[,"trend"])
  data_GSR$SCR <- as.numeric(data_GSR_st$time.series[,"remainder"])
  SCL <- zoo::zoo(data_GSR$SCL, order.by = data_GSR$time)
  SCR <- zoo::zoo(data_GSR$SCR, order.by = data_GSR$time)
  ####################### CLOSE CONNECTION ###################################
  DBI::dbDisconnect(db)
  ####################### MERGE DATASTREAMS ###################################
  data_biometric_time <- zoo::merge.zoo(HR,BR,ST,SCL,SCR,HRVHF)
  data_biometric_time_approx <- zoo::na.approx(data_biometric_time) # replace NA values by linear approximate
  data_biometric_time_approx_complet <- data_biometric_time_approx[complete.cases(data_biometric_time_approx)] # remove row with NA value at the begining and/or the end (not processed in na.approx)
  time_index <- zoo::index(data_biometric_time_approx_complet) # changing index as variable
  data_biometric_time_approx_complet_df <- as.data.frame(data_biometric_time_approx_complet)
  data_biometric_time_approx_complet_df <- cbind(time_index,data_biometric_time_approx_complet_df)
  #
  gamm.HR <- mgcv::gam(HR~s(time_index),data=data_biometric_time_approx_complet_df,method ="REML",correlation = corAR1()) # processes GAMM (fixed effect = time,random effect = participant, method = restricted maximum likelihood, correlation =  autocorrelation structure of order 1)
  pred.gamm.HR <- predict(gamm.HR)
  pred.gamm.HR_spline <- (spline(pred.gamm.HR, n= 100))$y
  #
  gamm.BR <- mgcv::gam(BR~s(time_index),data=data_biometric_time_approx_complet_df,method ="REML",correlation = corAR1()) # processes GAMM (fixed effect = time,random effect = participant, method = restricted maximum likelihood, correlation =  autocorrelation structure of order 1)
  pred.gamm.BR  <- predict(gamm.BR)
  pred.gamm.BR_spline <- (spline(pred.gamm.BR, n= 100))$y
  #
  gamm.ST <- mgcv::gam(ST~s(time_index),data=data_biometric_time_approx_complet_df,method ="REML",correlation = corAR1()) # processes GAMM (fixed effect = time,random effect = participant, method = restricted maximum likelihood, correlation =  autocorrelation structure of order 1)
  pred.gamm.ST  <- predict(gamm.ST)
  pred.gamm.ST_spline <- (spline(pred.gamm.ST, n= 100))$y
  #
  gamm.SCL <- mgcv::gam(SCL~s(time_index),data=data_biometric_time_approx_complet_df,method ="REML",correlation = corAR1()) # processes GAMM (fixed effect = time,random effect = participant, method = restricted maximum likelihood, correlation =  autocorrelation structure of order 1)
  pred.gamm.SCL  <- predict(gamm.SCL)
  pred.gamm.SCL_spline <- (spline(pred.gamm.SCL, n= 100))$y
  #
  gamm.SCR <- mgcv::gam(SCR~s(time_index),data=data_biometric_time_approx_complet_df,method ="REML",correlation = corAR1()) # processes GAMM (fixed effect = time,random effect = participant, method = restricted maximum likelihood, correlation =  autocorrelation structure of order 1)
  pred.gamm.SCR  <- predict(gamm.SCR)
  pred.gamm.SCR_spline <- (spline(pred.gamm.SCR, n= 100))$y
  #
  gamm.HRVHF <- mgcv::gam(HRVHF~s(time_index),data=data_biometric_time_approx_complet_df,method ="REML",correlation = corAR1()) # processes GAMM (fixed effect = time,random effect = participant, method = restricted maximum likelihood, correlation =  autocorrelation structure of order 1)
  pred.gamm.HRVHF  <- predict(gamm.HRVHF)
  pred.gamm.HRVHF_spline <- (spline(pred.gamm.HRVHF, n= 100))$y
  #
  pred_gamm_sync <- cbind(pred.gamm.HR_spline,pred.gamm.BR_spline,pred.gamm.ST_spline,pred.gamm.HRVHF_spline,pred.gamm.SCL_spline,pred.gamm.SCR_spline)
  ####################### TRENDS EXTRACTION ###################################
  pred_gamm_sync <- t(as.data.frame(pred_gamm_sync)) # rotate the dataframe
  N.ts <- dim(pred_gamm_sync)[1] #n row
  TT <- dim(pred_gamm_sync)[2] #n column
  ###### set.up.three.trends.echo #########
  cntl.list <-  list(maxit=50,safe=TRUE) #max iteration of the process before escape (the higher the better)
  model.list <- list(m=as.numeric(as.character(3)), R="diagonal and unequal") #correlation model
  kemz.3 <- MARSS::MARSS(pred_gamm_sync, model=model.list,
                         z.score=TRUE, form="dfa", control=cntl.list, silent = 2,fun.kf = "MARSSkfss") #DFA analysis (silent = 2 displays the iteration process)
  ########## varimax ############
  H.inv <- varimax(coef(kemz.3, type="matrix")$Z)$rotmat# get the inverse of the rotation matrix
  ########## rotations ##########
  Z.rot <- coef(kemz.3, type="matrix")$Z %*% H.inv# rotate factor loadings
  trends.rot <- solve(H.inv) %*% kemz.3$states# rotate trends
  TRENDS <- as.data.frame(t(trends.rot))
  ########## rename trends according to factor loading ##########
  spp <-  rownames(pred_gamm_sync)
  factor_loading <- cbind(spp,as.data.frame(Z.rot))
  colHRVHFload <- which.max(abs(factor_loading[factor_loading$spp == "pred.gamm.HRVHF_spline",2:4])) # max HRVHF defined as valence
  valHRVHFload <- factor_loading[factor_loading$spp == "pred.gamm.HRVHF_spline",attr(colHRVHFload,"names")]
  if (valHRVHFload > 0) { #high HF = negative / low HF = positive => take highest abs and if it's negative keep the trends, if it's positive invert the trend
    valence <- TRENDS[,attr(colHRVHFload,"names")]*-1
  } else {
    valence <- TRENDS[,attr(colHRVHFload,"names")]
  }
  TRENDS[,attr(colHRVHFload,"names")] <- NULL # delete valence column
  factor_loading[,attr(colHRVHFload,"names")] <- NULL # delete valence column
  ##################################################
  colHRload <- which.max(abs(factor_loading[factor_loading$spp == "pred.gamm.HR_spline",2:3])) # max HR defined as arousal
  valHRload <- factor_loading[factor_loading$spp == "pred.gamm.HR_spline",attr(colHRload,"names")]
  if (valHRload < 0) { # take highest abs of HR load and if it's positive keep the trends, if it's negative invert the trend
    arousal <- TRENDS[,attr(colHRload,"names")]*-1
  } else {
    arousal <- TRENDS[,attr(colHRload,"names")]
  }
  TRENDS[,attr(colHRload,"names")] <- NULL # delete arousal column
  ##################################################
  dominance <- TRENDS[,1]
  ##################################################
  TRENDS <- data.frame(arousal,valence,dominance) %>%
    dplyr::mutate(time = seq(dplyr::first(time_index),dplyr::last(time_index), length.out = 100)) %>%
    dplyr::mutate(arousal_rescale = scales::rescale(arousal,to = c(-1, 1))) %>% # rescale from -1 to 1
    dplyr::mutate(valence_rescale = scales::rescale(valence,to = c(-1, 1))) %>% # rescale from -1 to 1
    dplyr::mutate(degree = pracma::atan2d(arousal_rescale, valence_rescale)) %>% # atan2(x1, x2) in degree (arousal first and valence second)
    dplyr::select(time,arousal,valence,dominance,degree)
  TRENDS$circumplex [TRENDS$degree >= -22.5 & TRENDS$degree < 22.5] <-  "Pleasure"
  TRENDS$circumplex [TRENDS$degree >= 22.5  & TRENDS$degree < 67.5] <- "Excitement"
  TRENDS$circumplex [TRENDS$degree >= 67.5 & TRENDS$degree < 112.5] <- "Arousal"
  TRENDS$circumplex [TRENDS$degree >= 112.5 & TRENDS$degree < 157.5] <- "Distress"
  TRENDS$circumplex [TRENDS$degree >= 157.5] <- "Misery"
  TRENDS$circumplex [TRENDS$degree < -157.5] <- "Misery"
  TRENDS$circumplex [TRENDS$degree >= -157.5 & TRENDS$degree < -112.5] <- "Depression"
  TRENDS$circumplex [TRENDS$degree >= -112.5 & TRENDS$degree < -67.5] <- "Sleepiness"
  TRENDS$circumplex [TRENDS$degree >= -67.5 & TRENDS$degree < -22.5] <- "Contentment"
  return(TRENDS)
}

#* @post /arousal
AROUSAL <- function(session_id) {
  #######################################################################
  #                       VADR API                                      #
  #######################################################################
  # Query the database according session_id
  session_id_query <- as.character(session_id)
  required_metrics <- "('heartrate','gsr')"
  pg <- DBI::dbDriver("PostgreSQL") 
  db <- DBI::dbConnect(drv = pg, 
                       user="eaidevmaster",
                       password="eaidevmaster",
                       host="eai-dev-rr.cbxpfybyjgtq.eu-west-1.rds.amazonaws.com",
                       dbname="eai")
  qr <- paste0("SELECT metric, value, EXTRACT(EPOCH FROM time AT TIME ZONE 'UTC') *1000 AS time FROM ts_combined WHERE session_id = '",session_id_query,"' and metric in ",required_metrics,";")
  api_data <- DBI::dbGetQuery(db, qr)
  DBI::dbDisconnect(db)
  ######################### HR ##########################################
  data_HR <- api_data %>%
    dplyr::filter(metric == "heartrate") %>%
    dplyr::select(time,value) %>%
    dplyr::distinct(time,.keep_all = TRUE) %>%
    dplyr::arrange(time)
  if (nrow(data_HR) == 0) {
    stop("There is no HR data, the process aborted")
  }
  HR <- zoo::zoo(data_HR$value, order.by = data_HR$time)
  ######################### SCL/SCR ##########################################
  data_GSR <- api_data %>%
    dplyr::filter(metric == "gsr") %>%
    dplyr::select(time,value) %>%
    dplyr::distinct(time,.keep_all = TRUE) %>%
    dplyr::arrange(time)
  if (nrow(data_GSR) == 0) {
    stop("There is no GSR data, the process aborted")
  }
  data_GSR_zoo <- zoo::zoo(data_GSR$value,data_GSR$time) # zoo format
  data_GSR_st <- stl(ts(data_GSR_zoo, frequency=100), "periodic") # seasonal decomposition of GSR data
  data_GSR$SCL <- as.numeric(data_GSR_st$time.series[,"trend"])
  data_GSR$SCR <- as.numeric(data_GSR_st$time.series[,"remainder"])
  SCL <- zoo::zoo(data_GSR$SCL, order.by = data_GSR$time)
  SCR <- zoo::zoo(data_GSR$SCR, order.by = data_GSR$time)
  ####################### MERGE DATASTREAMS ###################################
  data_biometric_time <- zoo::merge.zoo(HR,SCL,SCR)
  data_biometric_time_approx <- zoo::na.approx(data_biometric_time) # replace NA values by linear approximate
  data_biometric_time_approx_complet <- data_biometric_time_approx[complete.cases(data_biometric_time_approx)] # remove row with NA value at the begining and/or the end (not processed in na.approx)
  time_index <- zoo::index(data_biometric_time_approx_complet) # changing index as variable
  data_biometric_time_approx_complet_df <- as.data.frame(data_biometric_time_approx_complet)
  data_biometric_time_approx_complet_df <- cbind(time_index,data_biometric_time_approx_complet_df)
  #
  gamm.HR <- mgcv::gam(HR~s(time_index),data=data_biometric_time_approx_complet_df,method ="REML",correlation = corAR1()) # processes GAMM (fixed effect = time,random effect = participant, method = restricted maximum likelihood, correlation =  autocorrelation structure of order 1)
  pred.gamm.HR <- predict(gamm.HR)
  pred.gamm.HR_spline <- (spline(pred.gamm.HR, n= 100))$y
  #
  gamm.SCL <- mgcv::gam(SCL~s(time_index),data=data_biometric_time_approx_complet_df,method ="REML",correlation = corAR1()) # processes GAMM (fixed effect = time,random effect = participant, method = restricted maximum likelihood, correlation =  autocorrelation structure of order 1)
  pred.gamm.SCL  <- predict(gamm.SCL)
  pred.gamm.SCL_spline <- (spline(pred.gamm.SCL, n= 100))$y
  #
  gamm.SCR <- mgcv::gam(SCR~s(time_index),data=data_biometric_time_approx_complet_df,method ="REML",correlation = corAR1()) # processes GAMM (fixed effect = time,random effect = participant, method = restricted maximum likelihood, correlation =  autocorrelation structure of order 1)
  pred.gamm.SCR  <- predict(gamm.SCR)
  pred.gamm.SCR_spline <- (spline(pred.gamm.SCR, n= 100))$y
  #
  pred_gamm_sync <- cbind(pred.gamm.HR_spline,pred.gamm.SCL_spline,pred.gamm.SCR_spline)
  ####################### TRENDS EXTRACTION ###################################
  pred_gamm_sync <- t(as.data.frame(pred_gamm_sync)) # rotate the dataframe
  N.ts <- dim(pred_gamm_sync)[1] #n row
  TT <- dim(pred_gamm_sync)[2] #n column
  ###### set.up.three.trends.echo #########
  cntl.list <-  list(maxit=50,safe=TRUE) #max iteration of the process before escape (the higher the better)
  model.list <- list(m=as.numeric(as.character(2)), R="diagonal and unequal") #correlation model
  kemz.3 <- MARSS::MARSS(pred_gamm_sync, model=model.list,
                         z.score=TRUE, form="dfa", control=cntl.list, silent = 2,fun.kf = "MARSSkfss") #DFA analysis (silent = 2 displays the iteration process)
  ########## varimax ############
  H.inv <- varimax(coef(kemz.3, type="matrix")$Z)$rotmat# get the inverse of the rotation matrix
  ########## rotations ##########
  Z.rot <- coef(kemz.3, type="matrix")$Z %*% H.inv# rotate factor loadings
  trends.rot <- solve(H.inv) %*% kemz.3$states# rotate trends
  ###############################
  AROUSAL <- as.data.frame(t(trends.rot)) %>%
    dplyr::mutate(time = seq(dplyr::first(time_index),dplyr::last(time_index), length.out = 100)) %>%
    dplyr::mutate(arousal_rescale = scales::rescale(V1,to = c(-1, 1))) %>%
    dplyr::mutate(valence_rescale = scales::rescale(V2,to = c(-1, 1))) %>%
    dplyr::mutate(degree = pracma::atan2d(arousal_rescale, valence_rescale)) %>% # atan2(x1, x2) in degree (arousal first and valence second)
    dplyr::select(time,arousal,valence,degree)
  AROUSAL$circumplex [AROUSAL$degree >= -22.5 & AROUSAL$degree < 22.5] <-  "Pleasure"
  AROUSAL$circumplex [AROUSAL$degree >= 22.5  & AROUSAL$degree < 67.5] <- "Excitement"
  AROUSAL$circumplex [AROUSAL$degree >= 67.5 & AROUSAL$degree < 112.5] <- "Arousal"
  AROUSAL$circumplex [AROUSAL$degree >= 112.5 & AROUSAL$degree < 157.5] <- "Distress"
  AROUSAL$circumplex [AROUSAL$degree < -157.5] <- "Misery"
  AROUSAL$circumplex [AROUSAL$degree >= 157.5] <- "Misery"
  AROUSAL$circumplex [AROUSAL$degree >= -157.5 & AROUSAL$degree < -112.5] <- "Depression"
  AROUSAL$circumplex [AROUSAL$degree >= -112.5 & AROUSAL$degree < -67.5] <- "Sleepiness"
  AROUSAL$circumplex [AROUSAL$degree >= -67.5 & AROUSAL$degree < -22.5] <- "Contentment"
  return(AROUSAL)
}

#* @get /quality
AutoQuality <- function(session_id)
{
  scaled_norm <- function(x, min=50, max=72)
  {
    norm <- (x - min) / (max - min)
    return(norm)
  }
  # LOOSE THRESHOLDING METHOD
  # ASSIGN MEMBERSHIP HIGH/MED/LOW QUALITY
  QmemHR  <- c(2.578511000, 14.00      , 25.00      )
  QmemRR  <- c(235.3020010, 691.742578 , 1109.573164)
  QmemTS  <- c(0.293300000, 0.602175736, 2.10       ) 
  QmemBR  <- c(0.211222636, 0.603160473, 2.249737718)
  QmemGSR <- c(0.179129400, 0.228355800, 3.503635907)
  #QmemGSR <- c(0.179129400,0.828355800,3.503635907)
  # DERIVED SD VALUES FOR FLATLINE
  GSR_sens_q <- c(0.05977367, 0.00)
  HR_sens_q  <- c(1.439559  , 0.00)
  RR_sens_q  <- c(27.96798  , 0.00)
  BR_sens_q  <- c(0.0350649 , 0.00)
  TS_sens_q  <- c(0.0234468 , 0.00)
  # Physiological Norms from Sleeping to Strenous Exercise
  HR_physiological_norm <- c(30,220) 
  BR_physiological_norm <- c(6,80) 
  # MEMORY AND EXPONENTIAL DECAY
  memory <- 10
  decay  <- 1 + (1/memory) 
  #
  session_id_query <- as.character(session_id)
  metrics          <- "('heartrate', 'heartrate_rr', 'temperature_skin', 'breathing_rate', 'gsr')"
  pg <- DBI::dbDriver("PostgreSQL") 
  db <- DBI::dbConnect(drv      = pg, 
                       user     = "eaidevmaster",
                       password = "eaidevmaster",
                       host     = "eai-dev-rr.cbxpfybyjgtq.eu-west-1.rds.amazonaws.com",
                       dbname   = "eai")
  qr <- paste0("SELECT metric, value, EXTRACT(EPOCH FROM time AT TIME ZONE 'UTC') *1000 AS time 
          FROM ts_combined 
          WHERE session_id = '",session_id_query,"' and metric in ", metrics, ";")
  api_data <- DBI::dbGetQuery(db, qr)
  DBI::dbDisconnect(db)
  # 
  if(nrow(api_data)==0)
  {
    stop("No metrics found in session")
  }
  #
  metrics   <- unique(api_data$metric)
  col_name  <- vector()
  col_count <- 0
  # WRANGLING THAT DATA 
  if("heartrate" %in% metrics)
  {
    hr  <- api_data                               %>%
      dplyr::filter(metric == "heartrate")        %>%
      dplyr::distinct(time, .keep_all = TRUE)     %>%
      dplyr::select(time, value)
    col_count <- col_count + 1
    col_name[col_count] <- "hr"
    hr  <- zoo::zoo(hr$value , order.by = hr$time)
  }
  if("heartrate_rr" %in% metrics)
  {
    rr  <- api_data                               %>%
      dplyr::filter(metric == "heartrate_rr")     %>%
      dplyr::distinct(time, .keep_all = TRUE)     %>%
      dplyr::select(time, value)
    col_count <- col_count + 1
    col_name[col_count] <- "rr"
    rr  <- zoo::zoo(rr$value , order.by = rr$time)
  }
  if("temperature_skin" %in% metrics)
  {
    ts  <- api_data                               %>%
      dplyr::filter(metric == "temperature_skin") %>%
      dplyr::distinct(time, .keep_all = TRUE)     %>%
      dplyr::select(time, value)
    col_count <- col_count + 1
    col_name[col_count] <- "ts"
    ts  <- zoo::zoo(ts$value , order.by = ts$time)
  }
  if("breathing_rate" %in% metrics)
  {
    br  <- api_data                               %>%
      dplyr::filter(metric == "breathing_rate")   %>%
      dplyr::distinct(time, .keep_all = TRUE)     %>%
      dplyr::select(time, value)
    col_count <- col_count + 1
    col_name[col_count] <- "br"
    br  <- zoo::zoo(br$value , order.by = br$time)
  }
  if("gsr" %in% metrics)
  {
    gsr <- api_data                               %>%
      dplyr::filter(metric == "gsr")              %>%
      dplyr::distinct(time, .keep_all = TRUE)     %>%
      dplyr::select(time, value)
    col_count <- col_count + 1
    col_name[col_count] <- "gsr"
    # Detect and Convert from microSiemens; ONLY AFFECTS OLD SESSIONS
    if(mean(gsr$value) > 101)
    {
      gsr$value <- gsr$value/100
    }
    gsr <- zoo::zoo(gsr$value, order.by = gsr$time)
  }
  # Merge ZOO and interpolate data **little bit of smoothing a day, keeps the doctor away**
  if(col_count == 5){
    merged_d  <- zoo::merge.zoo(hr, rr, ts, br, gsr)
    names     <- c(col_name[1], col_name[2], col_name[3], col_name[4], col_name[5])
    colnames(merged_d) <- names 
  } else if(col_count == 4){  
    merged_d  <- zoo::merge.zoo(get(col_name[1]), get(col_name[2]), get(col_name[3]), get(col_name[4]))
    names     <- c(col_name[1], col_name[2], col_name[3], col_name[4])
    colnames(merged_d) <- names
  } else if(col_count == 3){  
    merged_d  <- zoo::merge.zoo(get(col_name[1]), get(col_name[2]), get(col_name[3]))
    names     <- c(col_name[1], col_name[2], col_name[3])
    colnames(merged_d) <- names
  } else if(col_count == 2){  
    merged_d  <- zoo::merge.zoo(get(col_name[1]), get(col_name[2]))
    names     <- c(col_name[1], col_name[2])
    colnames(merged_d) <- names
  } else if(col_count == 1){  
    epoch_time  <- as.integer( as.POSIXct( index(get(col_name[1]))/1000, origin = "1970-01-01", tz="Europe/London" ) )
    names       <- c("epoch_time", col_name[1])
    interp_df   <- data.frame(epoch_time, as.vector(get(col_name[1])))
    colnames(interp_df) <- names
  }
  # Check If only 1 value present in column; therefore cannot interpolate; remove.
  if(col_count > 1)
  {
    temp     <- 0
    len_idf  <- length(merged_d[,1])
    for(i in 1:col_count)
    {
      if(sum(merged_d[,(i-temp)] %in% NA ) >=  len_idf - 5)
        {
        merged_d  <- merged_d[,-(i-temp)]
        col_name  <- col_name[-(i-temp)]
        temp      <- temp + 1
        col_count <- col_count - 1
        }
    }
    interp_df  <- zoo::na.approx(merged_d, na.rm=T)   %>%
                   na.omit()            
    epoch_time <- as.integer( as.POSIXct( index(interp_df)/1000, origin = "1970-01-01", tz="Europe/London" ) )
    interp_df  <- data.frame(epoch_time, interp_df)
    colnames(interp_df) <- c("epoch_time", col_name)      
  }
  if(nrow(interp_df) == 0){
    stop("SENSOR FAILURE; Check Placement of Device")
  }
  #
  metrics   <- colnames(interp_df)
  if("interp_df" %in% metrics){
    stop("Inconsistent naming detected")
  }
  metrics   <- metrics[-seq_len(1)]
  col_name  <- vector()
  alt_name  <- vector()
  col_count <- 0
  # DEFINE WINDOW SIZE FOR SD CALC
  window    <- 100
  #
  time <- interp_df$epoch_time
  time <- time[-seq_len(1)]
  # MAIN BODY OF CALCULATION
  if("hr" %in% metrics)
  {
    
    hr       <- diff(interp_df$hr);  hr.memb  <- vector(length=length(hr))
    # FLATLINE DETECTION
    hr.sd    <- rollapplyr(interp_df$hr, window, sd, fill = NA)[-1]
    # ADD A BIAS MULTIPLIER BASED ON PROBABLITY THAT SIGNAL IS A FLATLINE
    bias_flatline <- rep(1.0, length = length(hr))
    flatline      <- which(hr.sd < HR_sens_q[1])
    # ASSIGN BIAS MULTIPLIERS
    bias_flatline[flatline] <- scaled_norm(hr.sd[flatline], min=HR_sens_q[2], max=HR_sens_q[1])
    # HIGH QUALITY LABEL
    indHR_H <- which(hr < QmemHR[1])
    hr.memb[indHR_H] <- 100
    # LOW QUALITY LABEL
    indHR_M <- which(hr > QmemHR[1])
    hr.memb[indHR_M] <- 100-(100*scaled_norm(hr[indHR_M], min = QmemHR[1], max = QmemHR[2]))
    # PHYSIOLOGICAL NORM CHECK
    indHR.pnorm <- which(interp_df$hr[-1] < HR_physiological_norm[1] | interp_df$hr[-1] > HR_physiological_norm[2] )
    hr.memb[indHR.pnorm] <- 0
    # MULTIPLY QUALITY BY FLATLINE BIAS
    hr.memb <- hr.memb * bias_flatline
    # CAP LOW QUALITY AT 0
    hr.memb[which(hr.memb  < 0)] <- 0
    # 
    col_count           <- col_count + 1
    col_name[col_count] <- "hr.memb"
    alt_name[col_count] <- "heartrate_quality"
  }
  if("rr" %in% metrics)
  {
    rr      <- diff(interp_df$rr);  rr.memb  <- vector(length=length(rr))
    # FLATLINE DETECTION
    rr.sd    <- rollapplyr(interp_df$rr, window, sd, fill = NA)[-1]
    # ADD A BIAS MULTIPLIER BASED ON PROBABLITY THAT SIGNAL IS A FLATLINE
    bias_flatline <- rep(1.0, length = length(rr))
    flatline      <- which(rr.sd < RR_sens_q[1])
    # ASSIGN BIAS MULTIPLIERS
    bias_flatline[flatline] <- scaled_norm(rr.sd[flatline], min=RR_sens_q[2], max=RR_sens_q[1])
    #
    indRR_H <- which(rr < QmemRR[1])
    rr.memb[indRR_H] <- 100
    indRR_M <- which(rr > QmemRR[1])
    rr.memb[indRR_M] <- 100-(100*scaled_norm(rr[indRR_M], min = QmemRR[1], max = QmemRR[2]))
    # MULTIPLY QUALITY BY FLATLINE BIAS
    rr.memb <- rr.memb * bias_flatline
    #
    rr.memb[which(rr.memb  <0)] <- 0
    col_count           <- col_count + 1
    col_name[col_count] <- "rr.memb"
    alt_name[col_count] <- "heartrate_rr_quality"
  }
  if("ts" %in% metrics)
  {
    ts      <- diff(interp_df$ts);  ts.memb  <- vector(length=length(ts))
    # FLATLINE DETECTION
    ts.sd    <- rollapplyr(interp_df$ts, window, sd, fill = NA)[-1]
    # ADD A BIAS MULTIPLIER BASED ON PROBABLITY THAT SIGNAL IS A FLATLINE
    bias_flatline <- rep(1.0, length = length(ts))
    flatline      <- which(ts.sd < TS_sens_q[1])
    # ASSIGN BIAS MULTIPLIERS
    bias_flatline[flatline] <- scaled_norm(ts.sd[flatline], min=TS_sens_q[2], max=TS_sens_q[1])
    #
    indTS_H <- which(ts < QmemTS[1])
    ts.memb[indTS_H] <- 100
    indTS_M <- which(ts > QmemTS[1])
    ts.memb[indTS_M] <- 100-(100*scaled_norm(ts[indTS_M], min = QmemTS[1], max = QmemTS[2]))
    # MULTIPLY QUALITY BY FLATLINE BIAS
    ts.memb <- ts.memb * bias_flatline
    #
    ts.memb[which(ts.memb  <0)] <- 0
    col_count           <- col_count + 1
    col_name[col_count] <- "ts.memb"
    alt_name[col_count] <- "temperature_skin_quality"
  }
  if("br" %in% metrics)
  {
    br      <- diff(interp_df$br);  br.memb  <- vector(length=length(br))
    # FLATLINE DETECTION
    br.sd    <- rollapplyr(interp_df$br, window, sd, fill = NA)[-1]
    # ADD A BIAS MULTIPLIER BASED ON PROBABLITY THAT SIGNAL IS A FLATLINE
    bias_flatline <- rep(1.0, length = length(br))
    flatline      <- which(br.sd < BR_sens_q[1])
    # ASSIGN BIAS MULTIPLIERS
    bias_flatline[flatline] <- scaled_norm(br.sd[flatline], min=BR_sens_q[2], max=BR_sens_q[1])
    #
    indBR_H <- which(br < QmemBR[1])
    br.memb[indBR_H] <- 100
    indBR_M <- which(br > QmemBR[1])
    br.memb[indBR_M] <- 100-(100*scaled_norm(br[indBR_M], min = QmemBR[1], max = QmemBR[2]))
    indBR.pnorm <- which(interp_df$br[-1] < BR_physiological_norm[1] | interp_df$br[-1] > BR_physiological_norm[2] )
    br.memb[indBR.pnorm] <- 0
    # MULTIPLY QUALITY BY FLATLINE BIAS
    br.memb <- br.memb * bias_flatline
    #
    br.memb[which(br.memb  <0)] <- 0
    col_count           <- col_count + 1
    col_name[col_count] <- "br.memb"
    alt_name[col_count] <- "breathing_rate_quality"
  }
  if("gsr" %in% metrics)
  {
    gsr    <- diff(interp_df$gsr); gsr.memb <- vector(length=length(gsr))
    # FLATLINE DETECTION
    gsr.sd <- rollapplyr(interp_df$gsr, window, sd, fill = NA)[-1]
    # ADD A BIAS MULTIPLIER BASED ON PROBABLITY THAT SIGNAL IS A FLATLINE
    bias_flatline <- rep(1.0, length = length(gsr))
    flatline      <- which(gsr.sd < GSR_sens_q[1])
    # ASSIGN BIAS MULTIPLIERS
    bias_flatline[flatline] <- scaled_norm(gsr.sd[flatline], min=GSR_sens_q[2], max=GSR_sens_q[1])
    #
    indGSR_H <- which(gsr < QmemGSR[1])
    gsr.memb[indGSR_H]  <- 100
    indGSR_M <- which(gsr > QmemGSR[1])
    gsr.memb[indGSR_M]  <- 100-(100*scaled_norm(gsr[indGSR_M], min = QmemGSR[1], max = QmemGSR[2]))
    # MULTIPLY QUALITY BY FLATLINE BIAS
    gsr.memb <- gsr.memb * bias_flatline
    #
    gsr.memb[which(gsr.memb<0)] <- 0
    col_count           <- col_count + 1
    col_name[col_count] <- "gsr.memb"
    alt_name[col_count] <- "gsr_quality"
  }
  for(i in 1:memory)
  {
  # HR MEMORY & DECAY
    if("hr.memb" %in% col_name)
    {
      mem.ind <- which(hr.memb != 100)
      for(j in mem.ind)
      {
        if(j+1 >= length(hr.memb)){ break }
        if(hr.memb[j+1] >= hr.memb[j])
        {
          hr.memb[j+1] <- 100 - ((100-hr.memb[j])/decay)
    } } }
  # RR MEMORY & DECAY
    if("rr.memb" %in% col_name)
    {
      mem.ind <- which(rr.memb != 100)
      for(j in mem.ind)
      {
        if(j+1 > length(rr.memb)){ break }
        if(rr.memb[j+1] >= rr.memb[j])
        {
          rr.memb[j+1] <- 100 - ((100-rr.memb[j])/decay)
    } } }
  # BR MEMORY & DECAY
    if("br.memb" %in% col_name)
    {
      mem.ind <- which(br.memb != 100)
      for(j in mem.ind)
      {
        if(j+1 > length(br.memb)){ break }
        if(br.memb[j+1] >= br.memb[j])
        {
          br.memb[j+1] <- 100 - ((100-br.memb[j])/decay)
    } } }
  # TS MEMORY & DECAY
    if("ts.memb" %in% col_name)
    {
      mem.ind <- which(ts.memb != 100)
      for(j in mem.ind)
      {
        if(j+1 > length(ts.memb)){ break }        
        if(ts.memb[j+1] >= ts.memb[j])
        {
          ts.memb[j+1] <- 100 - ((100-ts.memb[j])/decay)
    } } }
  # GSR MEMORY & DECAY
    if("gsr.memb" %in% col_name)
    {
      mem.ind <- which(gsr.memb != 100)
      for(j in mem.ind)
      {
        if(j+1 > length(gsr.memb)){ break }         
        if(gsr.memb[j+1] >= gsr.memb[j])
        {
          gsr.memb[j+1] <- 100 - ((100-gsr.memb[j])/decay)
  } } } }
  # END OF MAIN BODY #
  # RETURN CORRECT DATA FRAME
  # FORMAT: | TIME (EPOCH) | QUALITY METRIC 1 | *QUALITY METRIC 2 | *QUALITY METRIC 3 | *QUALITY METRIC 4 | *QUALITY METRIC 5 |
  # *IF AVILABLE
  if(col_count == 5){
    df <- data.frame(time, get(col_name[1]), get(col_name[2]), get(col_name[3]), get(col_name[4]), get(col_name[5]))
    colnames(df) <- c("time", alt_name)
    return(df)
  } else if(col_count == 4){
    df <- data.frame(time, get(col_name[1]), get(col_name[2]), get(col_name[3]), get(col_name[4]))
    colnames(df) <- c("time", alt_name)
    return(df)
  } else if(col_count == 3){
    df <- data.frame(time, get(col_name[1]), get(col_name[2]), get(col_name[3]))
    colnames(df) <- c("time", alt_name)
    return(df)
  } else if(col_count == 2){
    df <- data.frame(time, get(col_name[1]), get(col_name[2]))
    colnames(df) <- c("time", alt_name)
    return(df)
  } else {
    df <- data.frame(time, get(col_name[1]))
    colnames(df) <- c("time", alt_name)
    return(df)
  }
}
#