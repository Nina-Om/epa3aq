first_err <- function(Input) {
  
  #Input <- read_xlsx("inp_first.xlsx", col_names = TRUE)
  
  messg0 = c()
  
  for (l1 in 2:dim(Input)[2]){
    m = c()
    msg = c()
    CODE = as.numeric(l1-1)  
    APPRAT=as.numeric(as.character(Input[3,l1]))
    APPNUM=as.numeric(as.character(Input[4,l1]))
    APSPAC=as.numeric(as.character(Input[5,l1]))
    PCA=as.numeric(as.character(Input[6,l1]))
    KD=as.numeric(as.character(Input[7,l1]))
    KOC=as.numeric(as.character(Input[8,l1]))
    METHAF=as.numeric(as.character(Input[9,l1]))
    WETTED = Input[10,l1]
    METHOD=Input[11,l1]
    INCORP=as.numeric(as.character(Input[12,l1]))
    SOL=as.numeric(as.character(Input[13,l1]))
    METHAP=as.numeric(as.character(Input[14,l1]))
    HYDHAP=as.numeric(as.character(Input[15,l1]))
    FOTHAP=as.numeric(as.character(Input[16,l1]))
    
    if(APPRAT <= 0 | is.na(APPRAT)) {
      m=paste0("Run#",CODE,": Enter APPRAT value > or = 1")
      msg = rbind(msg, m)
    } 
    if(APPNUM <= 0 | is.na(APPNUM)) {
      m=paste0("Run#",CODE,": Enter APPNUM value > or = 1")
      msg = rbind(msg, m)
    } 
    
    if(APPNUM > 1 & (is.na(APSPAC))) {
      m=paste0("Run#",CODE,": Enter APSPAC value > 0 if APPNUM > 1 (default value=1)")
      msg = rbind(msg, m)
    }
    
    if(PCA < 0 | PCA > 1 | is.na(PCA)) {
      m=paste0("Run#",CODE,": Enter PCA value between 0 and 1")
      msg = rbind(msg, m)
    }
    if((KOC <= 0 | is.na(KOC)) & (KD<=0 | is.na(KD))) {
      m=paste0("Run#",CODE,": Enter KD value > 0 or enter KOC > 0")
      msg = rbind(msg, m)
    }
    
    if((KD <= 0 | is.na(KD)) & (KOC<=0 | is.na(KOC))) {
      m=paste0("Run#",CODE,": Enter KOC value > 0 or enter KD > 0")
      msg = rbind(msg, m)
    }
    if(METHAF < 0 | is.na(METHAF)) {
      m=paste0("Run#",CODE,": Enter METHAF value > or = 0")
      msg = rbind(msg, m)
    }
    if(!WETTED %in% c("y","Y","n","N") | is.na(WETTED)) {
      m=paste0("Run#",CODE,": WETTED (Y or N)?")
      msg = rbind(msg, m)
    }
    if(!METHOD %in% c("A","B","C","D","a","b","c","d") | is.na(METHOD)) {
      m=paste0("Run#",CODE,": METHOD (A, B, C or D)?")
      msg = rbind(msg, m)
    }
    if(INCORP < 0 | is.na(INCORP)) {
      m=paste0("Run#",CODE,": Enter INCORP value > 0")
      msg = rbind(msg, m)
    }
    if(SOL < 0 | is.na(SOL)) {
      m=paste0("Run#",CODE,": Enter SOL value > 0")
      msg = rbind(msg, m)
    }
    if(METHAP<0 & is.na(HYDHAP)) {
      m=paste0("Run#",CODE,": Enter HYDHAP value")
      msg = rbind(msg, m)
    }
    if(is.na(METHAP)) {
      m=paste0("Run#",CODE,": Enter METHAP value, if unavailable enter '0'")
      msg = rbind(msg, m)
    }
    if(is.na(FOTHAP)) {
      m=paste0("Run#",CODE,": Enter FOTHAP value")
      msg = rbind(msg, m)
    }
    messg0=rbind(messg0,msg)
  }
    if(is.null(messg0[,1])) {
      messg0 = "No errors! All input parameters passed."
  }

  filePath <- "warnings_first.csv"
  write.csv(messg0, filePath, row.names = FALSE, quote = TRUE)
  messg <- as.data.frame(read.csv("warnings_first.csv"))
  names(messg) <- "Errors:"
  #print(messg[1])
  return(messg[1])
}

geneec_err <- function(Input) {
  #Input <- read_xlsx("inp_geneec.xlsx", col_names = TRUE)
  messg0 = c()
  
  for (l1 in 2:dim(Input)[2]){
    m = c()
    msg = c()
    CODE = as.numeric(l1-1)  
    APPRAT=as.numeric(as.character(Input[3,l1]))
    APPNUM=as.numeric(as.character(Input[4,l1]))
    APSPAC=as.numeric(as.character(Input[5,l1]))
    KD=as.numeric(as.character(Input[6,l1]))
    KOC=as.numeric(as.character(Input[7,l1]))
    METHAF=as.numeric(as.character(Input[8,l1]))
    WETTED=Input[9,l1]
    METHOD=Input[10,l1]
    AIRFLG=Input[11,l1]
    GRNFLG=Input[12,l1]
    GRSIZE=Input[13,l1]
    ORCFLG=Input[14,l1]
    YLOCEN=as.numeric(as.character(Input[15,l1]))
    INCORP=as.numeric(as.character(Input[16,l1]))
    SOL=as.numeric(as.character(Input[17,l1]))
    METHAP=as.numeric(as.character(Input[18,l1]))
    HYDHAP=as.numeric(as.character(Input[19,l1]))
    FOTHAP=as.numeric(as.character(Input[20,l1]))
  
    if(APPRAT <= 0 | is.na(APPRAT)) {
      m=paste0("Run#",CODE,": Enter APPRAT value > or = 1")
      msg = rbind(msg, m)
    } 
    if(APPNUM <= 0 | is.na(APPNUM)) {
      m=paste0("Run#",CODE,": Enter APPNUM value > or = 1")
      msg = rbind(msg, m)
    } 
    if(APPNUM > 1 & APSPAC <= 0) {
      m=paste0("Run#",CODE,": Enter APSPAC value > 0 if APPNUM > 1 (default value=1)")
      msg = rbind(msg, m)
    }
    if((KOC <= 0 | is.na(KOC)) & (KD<=0 | is.na(KD))) {
      m=paste0("Run#",CODE,": Enter KD value > 0 or enter KOC > 0")
      msg = rbind(msg, m)
    }
    if((KD <= 0 | is.na(KD)) & (KOC<=0 | is.na(KOC))) {
      m=paste0("Run#",CODE,": Enter KOC value > 0 or enter KD > 0")
      msg = rbind(msg, m)
    }
    if(METHAF < 0 | is.na(METHAF)) {
      m=paste0("Run#",CODE,": Enter METHAF value > or = 0")
      msg = rbind(msg, m)
    }
    if(!WETTED %in% c("y","Y","n","N") | is.na(WETTED)) {
      m=paste0("Run#",CODE,": WETTED (Y or N)?")
      msg = rbind(msg, m)
    }
    if(!METHOD %in% c("A","B","C","D","a","b","c","d") | is.na(METHOD)) {
      m=paste0("Run#",CODE,": METHOD (A, B, C or D)?")
      msg = rbind(msg, m)
    }
    
    if(METHOD %in% c("a", "A") & (is.na(AIRFLG) | !AIRFLG %in% c("A","B","C","D","a","b","c","d"))) {
      m=paste0("Run#",CODE,": AIRFLG (A, B, C or D)?")
      msg = rbind(msg, m)
    }
    if(METHOD %in% c("b", "B") & (is.na(GRNFLG) | !GRNFLG %in% c("A","B","a","b"))) {
      m=paste0("Run#",CODE,": GRNFLG (A or B)?")
      msg = rbind(msg, m)
    }
    
    if(GRNFLG %in% c("a", "A") & (is.na(GRSIZE) | !GRSIZE %in% c("A","B","a","b"))) {
      m=paste0("Run#",CODE,": GRSIZE (A or B)?")
      msg = rbind(msg, m)
    }
    
    if(GRNFLG %in% c("b", "B") & (is.na(GRSIZE) | !GRSIZE %in% c("A","B","a","b"))) {
      m=paste0("Run#",CODE,": GRSIZE (A or B)?")
      msg = rbind(msg, m)
    }

    if(YLOCEN < 0 | is.na(YLOCEN)) {
      m=paste0("Run#",CODE,": Enter YLOCEN value > or = 0")
      msg = rbind(msg, m)
    }

    if(INCORP < 0 | is.na(INCORP)) {
      m=paste0("Run#",CODE,": Enter INCORP value > 0")
      msg = rbind(msg, m)
    }
    if(SOL < 0 | is.na(SOL)) {
      m=paste0("Run#",CODE,": Enter SOL value > 0")
      msg = rbind(msg, m)
    }
    if(METHAP<0 & is.na(HYDHAP)) {
      m=paste0("Run#",CODE,": Enter HYDHAP value")
      msg = rbind(msg, m)
    }
    if(is.na(METHAP)) {
      m=paste0("Run#",CODE,": Enter METHAP value, if unavailable enter '0'")
      msg = rbind(msg, m)
    }
    if(is.na(FOTHAP)) {
      m=paste0("Run#",CODE,": Enter FOTHAP value")
      msg = rbind(msg, m)
    }
    messg0=rbind(messg0,msg)
  }
  
    if(is.null(messg0[,1])) {
      messg0 = "No errors! All input parameters passed."
    }
  
  filePath <- "warnings_first.csv"
  write.csv(messg0, filePath, row.names = FALSE, quote = TRUE)
  messg <- as.data.frame(read.csv("warnings_first.csv"))
  names(messg) <- "Errors:"
  #print(messg[1])
  return(messg[1])
  
  
  filePath <- "warnings_geneec.csv"
  write.csv(messg0, filePath, row.names = FALSE, quote = TRUE)
  messg <- as.data.frame(read.csv("warnings_geneec.csv"))
  names(messg) <- "Errors:"
  return(messg[1])
}






