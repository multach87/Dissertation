#standard lasso
lasso.full <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/Lasso_resultDF_FULL.RData")
mean(lasso.full[ , "fpr"])
mean(lasso.full[ , "fpr"] , na.rm = T)
mean(lasso.full[ , "fnr"])
mean(lasso.full[ , "fnr"] , na.rm = T)
mean(lasso.full[((lasso.full$eta.x != 0) | (lasso.full$eta.y != 0)) , "fpr"] , na.rm = T)
mean(lasso.full[ , "fnr"] , na.rm = T)
mean(lasso.full[ , "mpe"])
mean(lasso.full[ , "mpe"] , na.rm = T)

lasso.HD.half <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/Lasso_resultDF_HD_500.RData")
mean(lasso.HD.half[ , "fpr"])
mean(lasso.HD.half[ , "fpr"] , na.rm = T)
mean(lasso.HD.half[((lasso.HD.half$eta.x != 0) | (lasso.HD.half$eta.y != 0)) , "fpr"] , na.rm = T)
mean(lasso.HD.half[ , "fnr"])
mean(lasso.HD.half[ , "fnr"] , na.rm = T)
mean(lasso.HD.half[((lasso.HD.half$eta.x != 0) | (lasso.HD.half$eta.y != 0)) , "fnr"] , na.rm = T)
mean(lasso.HD.half[ , "mpe"])
mean(lasso.HD.half[ , "mpe"] , na.rm = T)



#adaptive lasso
adalasso.half <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/adalasso_resultDF_500.RData")
mean(adalasso.half[ , "fpr"])
mean(adalasso.half[ , "fpr"] , na.rm = T)
mean(adalasso.half[ , "fnr"])
mean(adalasso.half[ , "fnr"] , na.rm = T)
mean(adalasso.half[ , "mpe"])
mean(adalasso.half[ , "mpe"] , na.rm = T)

adalassoHD.half <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/adalasso_resultDF_HD_500.RData")
mean(adalassoHD.half[ , "fpr"])
mean(adalassoHD.half[ , "fpr"] , na.rm = T)
mean(adalassoHD.half[((adalassoHD.half$eta.x != 0) | (adalassoHD.half$eta.y != 0)) , "fpr"] , na.rm = T)
mean(adalassoHD.half[ , "fnr"])
mean(adalassoHD.half[ , "fnr"] , na.rm = T)
mean(adalassoHD.half[ , "mpe"])
mean(adalassoHD.half[((adalassoHD.half$eta.x != 0) | (adalassoHD.half$eta.y != 0)) , "fnr"] , na.rm = T)
mean(adalassoHD.half[ , "mpe"] , na.rm = T)


#SNCD LAD Lasso

#SNCD LAD Elastic Net




#LAD Lasso: ERRORS
#ladlasso.error.indices <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/ladlasso_errorindices_500.RData")
#ladlasso.half <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/ladlasso_resultDF_500.RData")
#mean(ladlasso.half[ , "fpr"])
#mean(ladlasso.half[ , "fpr"] , na.rm = T)
#mean(ladlasso.half[((ladlasso.half$eta.x != 0) | (ladlasso.half$eta.y != 0)) , "fpr"] , na.rm = T)
#mean(ladlasso.half[ , "fnr"] , na.rm = T)
#mean(ladlasso.half[((ladlasso.half$eta.x != 0) | (ladlasso.half$eta.y != 0)) , "fnr"] , na.rm = T)
#mean(ladlasso.half[ , "mpe"] , na.rm = T)
#mean(ladlasso.half[((ladlasso.half$eta.x != 0) | (ladlasso.half$eta.y != 0)) , "mpe"] , na.rm = T)

#ladlassoHD.half <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/HD_resultDF_500.RData")
#mean(ladlassoHD.half[ , "fpr"] , na.rm = T)
#mean(ladlassoHD.half[((ladlassoHD.half$eta.x != 0) | (ladlassoHD.half$eta.y != 0)) , "fpr"] , na.rm = T)
#mean(ladlassoHD.half[ , "fnr"] , na.rm = T)
#mean(ladlassoHD.half[((ladlassoHD.half$eta.x != 0) | (ladlassoHD.half$eta.y != 0)) , "fnr"] , na.rm = T)
#mean(ladlassoHD.half[ , "mpe"] , na.rm = T)
#mean(ladlassoHD.half[((ladlassoHD.half$eta.x != 0) | (ladlassoHD.half$eta.y != 0)) , "mpe"] , na.rm = T)

#huber lasso: non-sncd
#huberlasso.full <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/Huber_resultDF_FULL.RData")
#mean(huberlasso.full[ , "fpr"])
#mean(huberlasso.full[ , "fpr"] , na.rm = T)
#mean(huberlasso.full[((huberlasso.full$eta.x != 0) | (huberlasso.full$eta.y != 0)) , "fpr"] , na.rm = T)
#mean(huberlasso.full[ , "fnr"])
#mean(huberlasso.full[ , "fnr"] , na.rm = T)
#mean(huberlasso.full[((huberlasso.full$eta.x != 0) | (huberlasso.full$eta.y != 0)) , "fnr"] , na.rm = T)
#mean(huberlasso.full[ , "mpe"])
#mean(huberlasso.full[ , "mpe"] , na.rm = T)
#mean(huberlasso.full[((huberlasso.full$eta.x != 0) | (huberlasso.full$eta.y != 0)) , "mpe"] , na.rm = T)
#INF.mpe
#huberlassoHD.half <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/Huber_resultDF_HD_500.RData")
#huberlassoHD.inf <- which(huberlassoHD.half[ , "mpe"] == Inf)
#mean(huberlassoHD.half[ , "fpr"])
#mean(huberlassoHD.half[ , "fpr"] , na.rm = T)
#mean(huberlassoHD.half[((huberlassoHD.half$eta.x != 0) | (huberlassoHD.half$eta.y != 0)) , "fpr"] , na.rm = T)
#mean(huberlassoHD.half[ , "fnr"])
#mean(huberlassoHD.half[ , "fnr"] , na.rm = T)
#mean(huberlassoHD.half[((huberlassoHD.half$eta.x != 0) | (huberlassoHD.half$eta.y != 0)) , "fnr"] , na.rm = T)
#mean(huberlassoHD.half[ , "mpe"])
#mean(huberlassoHD.half[ , "mpe"] , na.rm = T)
#mean(huberlassoHD.half[((huberlassoHD.half$eta.x != 0) | (huberlassoHD.half$eta.y != 0)) , "mpe"] , na.rm = T)



#SNCD Huber lasso
SNCDhuberlasso.half <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/SNCDHuberLasso_resultDF_500.RData")
mean(SNCDhuberlasso.half[ , "fpr"])
mean(SNCDhuberlasso.half[ , "fpr"] , na.rm = T)
mean(SNCDhuberlasso.half[((SNCDhuberlasso.half$eta.x != 0) | (SNCDhuberlasso.half$eta.y != 0)) , "fpr"] , na.rm = T)
mean(SNCDhuberlasso.half[ , "fnr"])
mean(SNCDhuberlasso.half[ , "fnr"] , na.rm = T)
mean(SNCDhuberlasso.half[((SNCDhuberlasso.half$eta.x != 0) | (SNCDhuberlasso.half$eta.y != 0)) , "fnr"] , na.rm = T)
mean(SNCDhuberlasso.half[ , "mpe"])
mean(SNCDhuberlasso.half[ , "mpe"] , na.rm = T)
mean(SNCDhuberlasso.half[((SNCDhuberlasso.half$eta.x != 0) | (SNCDhuberlasso.half$eta.y != 0)) , "mpe"] , na.rm = T)

SNCDhuberlassoHD.half <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/SNCDHuberLasso_resultDF_HD_500.RData")
mean(SNCDhuberlassoHD.half[ , "fpr"])
mean(SNCDhuberlassoHD.half[ , "fpr"] , na.rm = T)
mean(SNCDhuberlassoHD.half[((SNCDhuberlassoHD.half$eta.x != 0) | (SNCDhuberlassoHD.half$eta.y != 0)) , "fpr"] , na.rm = T)
mean(SNCDhuberlassoHD.half[ , "fnr"])
mean(SNCDhuberlassoHD.half[ , "fnr"] , na.rm = T)
mean(SNCDhuberlassoHD.half[((SNCDhuberlassoHD.half$eta.x != 0) | (SNCDhuberlassoHD.half$eta.y != 0)) , "fnr"] , na.rm = T)
mean(SNCDhuberlassoHD.half[ , "mpe"])
mean(SNCDhuberlassoHD.half[ , "mpe"] , na.rm = T)
mean(SNCDhuberlassoHD.half[((SNCDhuberlassoHD.half$eta.x != 0) | (SNCDhuberlassoHD.half$eta.y != 0)) , "mpe"] , na.rm = T)


#OS Lasso PLUS: ERRORS + INF
OSlassoPLUS.half <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/OSLassoPLUS_resultDF_500.RData")
OSlassoPLUS.error.indices <- which(is.na(OSlassoPLUS.half$fpr))
OS.inf <- which(OSlassoPLUS.half[ , "mpe"] == Inf)
mean(OSlassoPLUS.half[ , "fpr"])
mean(OSlassoPLUS.half[ , "fpr"] , na.rm = T)
mean(OSlassoPLUS.half[((OSlassoPLUS.half$eta.x != 0) | (OSlassoPLUS.half$eta.y != 0)) , "fpr"] , na.rm = T)
mean(OSlassoPLUS.half[ , "fnr"])
mean(OSlassoPLUS.half[ , "fnr"] , na.rm = T)
mean(OSlassoPLUS.half[((OSlassoPLUS.half$eta.x != 0) | (OSlassoPLUS.half$eta.y != 0)) , "fnr"] , na.rm = T)
mean(OSlassoPLUS.half[ , "mpe"])
mean(OSlassoPLUS.half[ , "mpe"] , na.rm = T)
mean(OSlassoPLUS.half[((OSlassoPLUS.half$eta.x != 0) | (OSlassoPLUS.half$eta.y != 0)) , "mpe"] , na.rm = T)


OSlassoPLUSHD.half <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/OSLassoPLUS_resultDF_HD_500.RData")
mean(OSlassoPLUSHD.half[ , "fpr"])
mean(OSlassoPLUSHD.half[ , "fpr"] , na.rm = T)
mean(OSlassoPLUSHD.half[((OSlassoPLUSHD.half$eta.x != 0) | (OSlassoPLUSHD.half$eta.y != 0)) , "fpr"] , na.rm = T)
mean(OSlassoPLUSHD.half[ , "fnr"])
mean(OSlassoPLUSHD.half[ , "fnr"] , na.rm = T)
mean(OSlassoPLUSHD.half[((OSlassoPLUSHD.half$eta.x != 0) | (OSlassoPLUSHD.half$eta.y != 0)) , "fnr"] , na.rm = T)
mean(OSlassoPLUSHD.half[ , "mpe"] , na.rm = T)
mean(OSlassoPLUSHD.half[((OSlassoPLUSHD.half$eta.x != 0) | (OSlassoPLUSHD.half$eta.y != 0)) , "mpe"] , na.rm = T)
which(OSlassoPLUSHD.half[ , "mpe"] == Inf)



#OShuberlasso: INF.mpe
OShuberlasso.half <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/oshuberlasso_resultDF_500.RData")
OShuberlasso.inf <- which(OShuberlasso.half[ , "mpe"] == Inf)
mean(OShuberlasso.half[ , "fpr"])
mean(OShuberlasso.half[ , "fpr"] , na.rm = T)
mean(OShuberlasso.half[((OShuberlasso.half$eta.x != 0) | (OShuberlasso.half$eta.y != 0)) , "fpr"] , na.rm = T)
mean(OShuberlasso.half[ , "fnr"])
mean(OShuberlasso.half[ , "fnr"] , na.rm = T)
mean(OShuberlasso.half[((OShuberlasso.half$eta.x != 0) | (OShuberlasso.half$eta.y != 0)) , "fnr"] , na.rm = T)
mean(OShuberlasso.half[ , "mpe"])
mean(OShuberlasso.half[ , "mpe"] , na.rm = T)
mean(OShuberlasso.half[((OShuberlasso.half$eta.x != 0) | (OShuberlasso.half$eta.y != 0)) , "mpe"] , na.rm = T)

OShuberlassoHD.half <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/OShuberlasso_resultDF_HD_500.RData")
mean(OShuberlassoHD.half[ , "fpr"])
mean(OShuberlassoHD.half[ , "fpr"] , na.rm = T)
mean(OShuberlassoHD.half[((OShuberlassoHD.half$eta.x != 0) | (OShuberlassoHD.half$eta.y != 0)) , "fpr"] , na.rm = T)
mean(OShuberlassoHD.half[ , "fnr"])
mean(OShuberlassoHD.half[ , "fnr"] , na.rm = T)
mean(OShuberlassoHD.half[((OShuberlassoHD.half$eta.x != 0) | (OShuberlassoHD.half$eta.y != 0)) , "fnr"] , na.rm = T)
mean(OShuberlassoHD.half[ , "mpe"])
mean(OShuberlassoHD.half[ , "mpe"] , na.rm = T)
mean(OShuberlassoHD.half[((OShuberlassoHD.half$eta.x != 0) | (OShuberlassoHD.half$eta.y != 0)) , "mpe"] , na.rm = T)



#elnet5
elnet5.half <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/elnet5_resultDF_500.RData")
mean(elnet5.half[ , "fpr"])
mean(elnet5.half[ , "fpr"] , na.rm = T)
mean(elnet5.half[((elnet5.half$eta.x != 0) | (elnet5.half$eta.y != 0)) , "fpr"] , na.rm = T)
mean(elnet5.half[ , "fnr"])
mean(elnet5.half[ , "fnr"] , na.rm = T)
mean(elnet5.half[((elnet5.half$eta.x != 0) | (elnet5.half$eta.y != 0)) , "fnr"] , na.rm = T)
mean(elnet5.half[ , "mpe"])
mean(elnet5.half[ , "mpe"] , na.rm = T)
mean(elnet5.half[((elnet5.half$eta.x != 0) | (elnet5.half$eta.y != 0)) , "mpe"] , na.rm = T)

elnet5HD.half <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/elnet5_resultDF_HD_500.RData")
mean(elnet5HD.half[ , "fpr"])
mean(elnet5HD.half[ , "fpr"] , na.rm = T)
mean(elnet5HD.half[((elnet5HD.half$eta.x != 0) | (elnet5HD.half$eta.y != 0)) , "fpr"] , na.rm = T)
mean(elnet5HD.half[ , "fnr"])
mean(elnet5HD.half[ , "fnr"] , na.rm = T)
mean(elnet5HD.half[((elnet5HD.half$eta.x != 0) | (elnet5HD.half$eta.y != 0)) , "fnr"] , na.rm = T)
mean(elnet5HD.half[ , "mpe"])
mean(elnet5HD.half[ , "mpe"] , na.rm = T)
mean(elnet5HD.half[((elnet5HD.half$eta.x != 0) | (elnet5HD.half$eta.y != 0)) , "mpe"] , na.rm = T)

#elnet75/elnet9?



#adaelnet5
adaelnet5.half <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/adaelnet5_resultDF_500.RData")
mean(adaelnet5.half[ , "fpr"])
mean(adaelnet5.half[ , "fpr"] , na.rm = T)
mean(adaelnet5.half[((adaelnet5.half$eta.x != 0) | (adaelnet5.half$eta.y != 0)) , "fpr"] , na.rm = T)
mean(adaelnet5.half[ , "fnr"])
mean(adaelnet5.half[ , "fnr"] , na.rm = T)
mean(adaelnet5.half[((adaelnet5.half$eta.x != 0) | (adaelnet5.half$eta.y != 0)) , "fnr"] , na.rm = T)
mean(adaelnet5.half[ , "mpe"])
mean(adaelnet5.half[ , "mpe"] , na.rm = T)
mean(adaelnet5.half[((adaelnet5.half$eta.x != 0) | (adaelnet5.half$eta.y != 0)) , "mpe"] , na.rm = T)

adaelnet5HD.half <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/adaelnet5_resultDF_HD_500.RData")
mean(adaelnet5HD.half[ , "fpr"])
mean(adaelnet5HD.half[ , "fpr"] , na.rm = T)
mean(adaelnet5HD.half[((adaelnet5HD.half$eta.x != 0) | (adaelnet5HD.half$eta.y != 0)) , "fpr"] , na.rm = T)
mean(adaelnet5HD.half[ , "fnr"])
mean(adaelnet5HD.half[ , "fnr"] , na.rm = T)
mean(adaelnet5HD.half[((adaelnet5HD.half$eta.x != 0) | (adaelnet5HD.half$eta.y != 0)) , "fnr"] , na.rm = T)
mean(adaelnet5HD.half[ , "mpe"])
mean(adaelnet5HD.half[ , "mpe"] , na.rm = T)
mean(adaelnet5HD.half[((adaelnet5HD.half$eta.x != 0) | (adaelnet5HD.half$eta.y != 0)) , "mpe"] , na.rm = T)

#adaelnet75/adaelnet9?


#SNCDhuberelnet5
SNCDhuberelnet5.half <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/")
mean(SNCDhuberelnet5.half[ , "fpr"])
mean(SNCDhuberelnet5.half[ , "fpr"] , na.rm = T)
mean(SNCDhuberelnet5.half[((SNCDhuberelnet5.half$eta.x != 0) | (SNCDhuberelnet5.half$eta.y != 0)) , "fpr"] , na.rm = T)
mean(SNCDhuberelnet5.half[ , "fnr"])
mean(SNCDhuberelnet5.half[ , "fnr"] , na.rm = T)
mean(SNCDhuberelnet5.half[((SNCDhuberelnet5.half$eta.x != 0) | (SNCDhuberelnet5.half$eta.y != 0)) , "fnr"] , na.rm = T)
mean(SNCDhuberelnet5.half[ , "mpe"])
mean(SNCDhuberelnet5.half[ , "mpe"] , na.rm = T)
mean(SNCDhuberelnet5.half[((SNCDhuberelnet5.half$eta.x != 0) | (SNCDhuberelnet5.half$eta.y != 0)) , "mpe"] , na.rm = T)

SNCDhuberelnet5HD.half <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/SNCDHuberELNet5_HD_resultDF_500.RData")
mean(SNCDhuberelnet5HD.half[ , "fpr"])
mean(SNCDhuberelnet5HD.half[ , "fpr"] , na.rm = T)
mean(SNCDhuberelnet5HD.half[((SNCDhuberelnet5HD.half$eta.x != 0) | (SNCDhuberelnet5HD.half$eta.y != 0)) , "fpr"] , na.rm = T)
mean(SNCDhuberelnet5HD.half[ , "fnr"])
mean(SNCDhuberelnet5HD.half[ , "fnr"] , na.rm = T)
mean(SNCDhuberelnet5HD.half[((SNCDhuberelnet5HD.half$eta.x != 0) | (SNCDhuberelnet5HD.half$eta.y != 0)) , "fnr"] , na.rm = T)
mean(SNCDhuberelnet5HD.half[ , "mpe"])
mean(SNCDhuberelnet5HD.half[ , "mpe"] , na.rm = T)
mean(SNCDhuberelnet5HD.half[((SNCDhuberelnet5HD.half$eta.x != 0) | (SNCDhuberelnet5HD.half$eta.y != 0)) , "mpe"] , na.rm = T)



#msadaelnet5: ERROR.mpe? --> NULL MODELS
msadaelnet5.half <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/msaelnet5_resultDF_500.RData")
msadaelnet5.null.indices <- which(is.na(msadaelnet5.half$mpe))
mean(msadaelnet5.half[ , "fpr"])
mean(msadaelnet5.half[ , "fpr"] , na.rm = T)
mean(msadaelnet5.half[((msadaelnet5.half$eta.x != 0) | (msadaelnet5.half$eta.y != 0)) , "fpr"] , na.rm = T)
mean(msadaelnet5.half[ , "fnr"])
mean(msadaelnet5.half[ , "fnr"] , na.rm = T)
mean(msadaelnet5.half[((msadaelnet5.half$eta.x != 0) | (msadaelnet5.half$eta.y != 0)) , "fnr"] , na.rm = T)
mean(msadaelnet5.half[ , "mpe"])
mean(msadaelnet5.half[ , "mpe"] , na.rm = T)
mean(msadaelnet5.half[((msadaelnet5.half$eta.x != 0) | (msadaelnet5.half$eta.y != 0)) , "mpe"] , na.rm = T)
#NULL MODELS
msadaelnet5HD.half <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/msaelnet5_resultDF_HD_500.RData")
msadaelnet5HD.null.indices <- which(is.na(msadaelnet5HD.half$mpe)) 
mean(msadaelnet5HD.half[ , "fpr"])
mean(msadaelnet5HD.half[ , "fpr"] , na.rm = T)
mean(msadaelnet5HD.half[((msadaelnet5HD.half$eta.x != 0) | (msadaelnet5HD.half$eta.y != 0)) , "fpr"] , na.rm = T)
mean(msadaelnet5HD.half[ , "fnr"])
mean(msadaelnet5HD.half[ , "fnr"] , na.rm = T)
mean(msadaelnet5HD.half[((msadaelnet5HD.half$eta.x != 0) | (msadaelnet5HD.half$eta.y != 0)) , "fnr"] , na.rm = T)
mean(msadaelnet5HD.half[ , "mpe"])
mean(msadaelnet5HD.half[ , "mpe"] , na.rm = T)
mean(msadaelnet5HD.half[((msadaelnet5HD.half$eta.x != 0) | (msadaelnet5HD.half$eta.y != 0)) , "mpe"] , na.rm = T)

#msadaelnet75/msadaelnet9?


#pense5: INF.mpe
#pense5.half <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/pense5_resultDF_500.RData")
#pense5.inf.mpe <- which(pense5.half[ , "mpe"] == Inf)
#mean(pense5.half[-pense5.inf.mpe , "fpr"])
#mean(pense5.half[ , "fpr"] , na.rm = T)
#mean(pense5.half[((pense5.half$eta.x != 0) | (pense5.half$eta.y != 0)) , "fpr"] , na.rm = T)
#mean(pense5.half[ , "fnr"])
#mean(pense5.half[ , "fnr"] , na.rm = T)
#mean(pense5.half[((pense5.half$eta.x != 0) | (pense5.half$eta.y != 0)) , "fnr"] , na.rm = T)
#mean(pense5.half[ , "mpe"])
#mean(pense5.half[ , "mpe"] , na.rm = T)
#mean(pense5.half[((pense5.half$eta.x != 0) | (pense5.half$eta.y != 0)) , "mpe"] , na.rm = T)

#pense5HD.half <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/pense5_resultDF_HD_500.RData")
#mean(pense5HD.half[ , "fpr"] , na.rm = T)
#mean(pense5HD.half[((pense5HD.half$eta.x != 0) | (pense5HD.half$eta.y != 0)) , "fpr"] , na.rm = T)
#mean(pense5HD.half[ , "fnr"] , na.rm = T)
#mean(pense5HD.half[((pense5HD.half$eta.x != 0) | (pense5HD.half$eta.y != 0)) , "fnr"] , na.rm = T)
#mean(pense5HD.half[ , "mpe"] , na.rm = T)
#mean(pense5HD.half[((pense5HD.half$eta.x != 0) | (pense5HD.half$eta.y != 0)) , "mpe"] , na.rm = T)








