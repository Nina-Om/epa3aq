first111 <- function(Input) {
  
  warn=first_err(Input)
   if (!warn$"Errors:" %in% c("No errors! All input parameters passed.")) {
     stop()
   }
#Input <- read_xlsx("inp_first.xlsx", col_names = TRUE)

CONC0_df <- c()
CON365_df <- c()
res_df <- c()
CODE_df <- c()
CROP_df <- c()
CHMNAM_df <- c()  

for (l in 2:dim(Input)[2]){
ADSFRR <- c()
DEGFRF <- c()
ROCONC <- c()
ADSFRS <- c()
SDCONC <- c()
CHRONIC <- c()

APPRAT = 0
APPNUM = 0
APPTOT = 0
KD = 0
KOC = 0
KDFRAC = 0
SOL = 0
METHAF = 0
METHAP = 0
HYDHAP = 0
FOTHAP = 0
DEGHAP = 0
INCORP = 0
PCTSRO = 0
ROAREA = 0
ROINIT = 0
ROFIN = 0
SDINIT = 0
SDFIN = 0
SHIFT = 0
STORM = 0
KADS1 = 0
KADSUR = 0
KADSUS = 0
KDEGP = 0
KMETF = 0
KHYDP = 0
KMETP = 0
KFOTP = 0
CONC0 = 0
CONC4 = 0
CONC21 = 0
CONC60 = 0
CONC90 = 0
CON365 = 0
SUM4 = 0
SUM21 = 0
SUM60 = 0
SUM90 = 0
SUM365 = 0
#
for(K in 1:375){
  ROCONC[K] = 0
  SDCONC[K] = 0
  CHRONIC[K] = 0
}

for(K in 1:100){
  ADSFRR[K] = 0
  ADSFRS[K] = 0
}

for(K in 1:8){
  DEGFRF[K] = 0
}

I = 0
K = 0
#######################

CODE = as.numeric(l-1)
CHMNAM = Input[1,l]
CROP=Input[2,l]

APPRAT=as.numeric(as.character(Input[3,l]))

APPNUM=as.numeric(as.character(Input[4,l]))
APPTOT = APPRAT * APPNUM

if (APPNUM > 1){
  APSPAC=as.numeric(as.character(Input[5,l]))
} else {
  APSPAC=1
}

PCA=as.numeric(as.character(Input[6,l]))
TDEGF = APSPAC * (APPNUM-1) + 2

KD=as.numeric(as.character(Input[7,l]))

if(KD <= 0.0){
KOC=as.numeric(as.character(Input[8,l]))
ADSORP = 'A'
KD = 0.0116 * KOC
} else {
KOC = KD * 86.207
ADSORP = 'B'
}

STORM = 2
METHAF=as.numeric(as.character(Input[9,l]))
WETTED = Input[10,l]

if(WETTED == 'Y'| WETTED == 'y') STORM = 0
METHOD=Input[11,l]

if (METHOD == 'A'| METHOD == 'a'){
  DRIFT = 0.16
  APPEFF = 0.95
  SPTYPE = 'AERIAL'
} else if (METHOD == 'B'| METHOD == 'b'){
  DRIFT = 0.064
  APPEFF = 0.99
  SPTYPE = 'GROUND'
} else if (METHOD == 'C'| METHOD == 'c'){
  DRIFT = 0.063
  APPEFF = 0.99
  SPTYPE = 'ABLAST'
} else if (METHOD == 'D'| METHOD == 'd'){
  DRIFT = 0.0
  APPEFF = 1.0
  SPTYPE = 'GRANUL'
}

if(METHOD == 'B'| METHOD =='b'| METHOD == 'D'| METHOD == 'd'){
  INCORP=as.numeric(as.character(Input[12,l]))
  APFLAG = 0
}

if(KD  <=  5.00e-3){
  KDFRAC = 1.0
} else if (KD  <=  1.00e-2 & KD > 5.00e-3){
  KDFRAC = 0.9991715 + (1.0 - 0.9991715) * (1.00e-2 - KD) /(1.00e-2 - 5.00e-3)
} else if(KD  <=  5.00e-2 & KD > 1.00e-2){
  KDFRAC = 0.9933720 + (0.9991715 - 0.9933720) * (5.00e-2 - KD) /(5.00e-2 - 1.00e-2)
} else if(KD  <=  1.00e-1 & KD > 5.00e-2){
  KDFRAC = 0.9859155 + (0.9933720 - 0.9859155) * (1.00e-1 - KD) /(1.00e-1 - 5.00e-2)
} else if(KD  <=  3.00e-1 & KD > 1.00e-1){
  KDFRAC = 0.9569180 + (0.9859155 - 0.9569180) * (3.00e-1 - KD) /(3.00e-1 - 1.00e-1)
} else if(KD  <=  5.00e-1 & KD > 3.00e-1){
  KDFRAC = 0.9295775 + (0.9569180 - 0.9295775) * (5.00e-1 - KD) /(5.00e-1 - 3.00e-1)
} else if(KD  <=  7.50e-1 & KD > 5.00e-1){
  KDFRAC = 0.8980944 + (0.9295775 - 0.8980944) * (7.50e-1 - KD) /(7.50e-1 - 5.00e-1)
} else if(KD  <=  1.00e00 & KD > 7.50e-1){
  KDFRAC = 0.8682684 + (0.8980944 - 0.8682684) * (1.00e00 - KD) /(1.00e00 - 7.50e-1)
} else if(KD  <=  1.25e00 & KD > 1.00e00){
  KDFRAC = 0.8409279 + (0.8682684 - 0.8409279) * (1.25e00 - KD) /(1.25e00 - 1.00e00)
} else if(KD  <=  1.50e00 & KD > 1.25e00){
  KDFRAC = 0.8147307 + (0.8409279 - 0.8147307) * (1.50e00 - KD) /(1.50e00 - 1.25e00)
} else if(KD  <=  1.75e00 & KD > 1.50e00){
  KDFRAC = 0.7904060 + (0.8147307 - 0.7904060) * (1.75e00 - KD) /(1.75e00 - 1.50e00)
} else if(KD  <=  2.00e00 & KD > 1.75e00){
  KDFRAC = 0.7675973 + (0.7904060 - 0.7675973) * (2.00e00 - KD) /(2.00e00 - 1.75e00)
} else if(KD  <=  2.25e00 & KD > 2.00e00){
  KDFRAC = 0.7461475 + (0.7675973 - 0.7461475) * (2.25e00 - KD) /(2.25e00 - 2.00e00)
} else if(KD  <=  2.50e00 & KD > 2.25e00){
  KDFRAC = 0.7260066 + (0.7461475 - 0.7260066) * (2.50e00 - KD) /(2.50e00 - 2.25e00)
} else if(KD  <=  2.75e00 & KD > 2.50e00){
  KDFRAC = 0.7070340 + (0.7260066 - 0.7070340) * (2.75e00 - KD) /(2.75e00 - 2.50e00)
} else if(KD  <=  3.00e00 & KD > 2.75e00){
  KDFRAC = 0.6891881 + (0.7070340 - 0.6891881) * (3.00e00 - KD) /(3.00e00 - 2.75e00)
} else if(KD  <=  3.50e00 & KD > 3.00e00){
  KDFRAC = 0.6562883 + (0.6891881 - 0.6562883) * (3.50e00 - KD) /(3.50e00 - 3.00e00)
} else if(KD  <=  4.00e00 & KD > 3.50e00){
  KDFRAC = 0.6269097 + (0.6562883 - 0.6269097) * (4.00e00 - KD) /(4.00e00 - 3.50e00)
} else if(KD  <=  4.50e00 & KD > 4.00e00){
  KDFRAC = 0.6004060 + (0.6269097 - 0.6004060) * (4.50e00 - KD) /(4.50e00 - 4.00e00)
} else if(KD  <=  5.00e00 & KD > 4.50e00){
  KDFRAC = 0.5765700 + (0.6004060 - 0.5765700) * (5.00e00 - KD) /(5.00e00 - 4.50e00)
} else if(KD  <=  5.50e00 & KD > 5.00e00){
  KDFRAC = 0.5548384 + (0.5765700 - 0.5548384) * (5.50e00 - KD) /(5.50e00 - 5.00e00)
} else if(KD  <=  6.00e00 & KD > 5.50e00){
  KDFRAC = 0.5352196 + (0.5548384 - 0.5352196) * (6.00e00 - KD) /(6.00e00 - 5.50e00)
} else if(KD  <=  7.00e00 & KD > 6.00e00){
  KDFRAC = 0.5007954 + (0.5352196 - 0.5007954) * (7.00e00 - KD) /(7.00e00 - 6.00e00)
} else if(KD  <=  8.00e00 & KD > 7.00e00){
  KDFRAC = 0.4717896 + (0.5007954 - 0.4717896) * (8.00e00 - KD) /(8.00e00 - 7.00e00)
} else if(KD  <=  9.00e00 & KD > 8.00e00){
  KDFRAC = 0.4471002 + (0.4717896 - 0.4471002) * (9.00e00 - KD) /(9.00e00 - 8.00e00)
} else if(KD  <=  1.00e01 & KD > 9.00e00){
  KDFRAC = 0.4257415 + (0.4471002 - 0.4257415) * (1.00e01 - KD) /(1.00e01 - 9.00e00)
} else if(KD  <=  1.25e01 & KD > 1.00e01){
  KDFRAC = 0.3837614 + (0.4257415 - 0.3837614) * (1.25e01 - KD) /(1.25e01 - 1.00e01)
} else if(KD  <=  1.50e01 & KD > 1.25e01){
  KDFRAC = 0.3528086 + (0.3837614 - 0.3528086) * (1.50e01 - KD) /(1.50e01 - 1.25e01)
} else if(KD  <=  1.75e01 & KD > 1.50e01){
  KDFRAC = 0.3292129 + (0.3528086 - 0.3292129) * (1.75e01 - KD) /(1.75e01 - 1.50e01)
} else if(KD  <=  2.00e01 & KD > 1.75e01){
  KDFRAC = 0.3107208 + (0.3292129 - 0.3107208) * (2.00e01 - KD) /(2.00e01 - 1.75e01)
} else if(KD  <=  2.50e01 & KD > 2.00e01){
  KDFRAC = 0.2834880 + (0.3107208 - 0.2834880) * (2.50e01 - KD) /(2.50e01 - 2.00e01)
} else if(KD  <=  3.00e01 & KD > 2.50e01){
  KDFRAC = 0.2646396 + (0.2834880 - 0.2646396) * (3.00e01 - KD) /(3.00e01 - 2.50e01)
} else if(KD  <=  4.00e01 & KD > 3.00e01){
  KDFRAC = 0.2400580 + (0.2646396 - 0.2400580) * (4.00e01 - KD) /(4.00e01 - 3.00e01)
} else if(KD  <=  5.00e01 & KD > 4.00e01){
  KDFRAC = 0.2249793 + (0.2400580 - 0.2249793) * (5.00e01 - KD) /(5.00e01 - 4.00e01)
} else if(KD  <=  1.00e02 & KD > 5.00e01){
  KDFRAC = 0.1939188 + (0.2249793 - 0.1939188) * (1.00e02 - KD) /(1.00e02 - 5.00e01)
} else if(KD  <=  5.00e02 & KD > 1.00e02){
  KDFRAC = 0.1788732 + (0.1939188 - 0.1788732) * (5.00e02 - KD) /(5.00e02 - 1.00e02)
} else if(KD  <=  1.00e03 & KD > 5.00e02){
  KDFRAC = 0.1615742 + (0.1788732 - 0.1615742) * (1.00e03 - KD) /(1.00e03 - 5.00e02)
} else if(KD  <=  5.00e03 & KD > 1.00e03){
  KDFRAC = 0.1425352 + (0.1615742 - 0.1425352) * (5.00e03 - KD) /(5.00e03 - 1.00e03)
} else if(KD  <=  1.00e04 & KD > 5.00e03){
  KDFRAC = 0.1258409 + (0.1425352 - 0.1258409) * (1.00e04 - KD) /(1.00e04 - 5.00e03)
} else if(KD  <=  2.00e04 & KD > 1.00e04){
  KDFRAC = 0.1021458 + (0.1258409 - 0.1021458) * (2.00e04 - KD) /(2.00e04 - 1.00e04)
} else if(KD  <=  3.00e04 & KD > 2.00e04){
  KDFRAC = 0.0859983 + (0.1021458 - 0.0859983) * (3.00e04 - KD) /(3.00e04 - 2.00e04)
} else if(KD  <=  5.00e04 & KD > 3.00e04){
  KDFRAC = 0.0653521 + (0.0859983 - 0.0653521) * (5.00e04 - KD) /(5.00e04 - 3.00e04)
} else if(KD  <=  1.00e05 & KD > 5.00e04){
  KDFRAC = 0.0408318 + (0.0653521 - 0.0408318) * (1.00e05 - KD) /(1.00e05 - 5.00e04)
} else if(KD  <=  5.00e05 & KD > 1.00e05){
  KDFRAC = 0.0102055 + (0.0408318 - 0.0102055) * (5.00e05 - KD) /(5.00e05 - 1.00e05)
} else if(KD  <=  1.00e06 & KD > 5.00e05){
  KDFRAC = 0.0052672 + (0.0102055 - 0.0052672) * (1.00e06 - KD) /(1.00e06 - 5.00e05)
} else if(KD > 1.00e06){
  KDFRAC = 0.001
}
#############################
if (METHAF  <=  0.0){
  KMETF = 0.0
} else {
  KMETF = log(2.0) / METHAF
}

if(INCORP <= 0.0001) APFLAG = 1
if(INCORP <= 1.0) INCORP = 1.0
if(INCORP >= 6.0) INCORP = 6.0

PCTSRO = 0.08
ROAREA = 172.8
WBAREA = 5.28

if(KD <= 1.0){
  KDADJA = 0.705
  KDADJP = 0.974
} else if(KD <= 10){
  KDADJA = 0.705 - KD * ((0.705-0.664)/10.0)
  KDADJP = 0.974 - KD * ((0.974-0.930)/10.0)
} else if(KD <= 100){
  KDADJA = 0.664 - KD * ((0.664-0.732)/100.0)
  KDADJP = 0.930 - KD * ((0.930-0.960)/100.0)
} else if(KD <= 1000){
  KDADJA = 0.732 - KD * ((0.732-0.918)/1000.0)
  KDADJP = 0.960 - KD * ((0.960-0.992)/1000.0)
} else if(KD <= 10000){
  KDADJA = 0.918 - KD * ((0.918-0.987)/10000.0)
  KDADJP = 0.992 - KD * ((0.992-0.999)/10000.0)
} else {
  KDADJA = 1.0
  KDADJP = 1.0
}

FLADJA = KDADJA
FLADJP = KDADJP
SHIFT = 0

DEGFRF <- c()
for(I in 1:8){
  SHIFT = I-1
  DEGFRF[I] = exp(-KMETF*SHIFT)
  DEGF1 = DEGFRF[1]
  DEGF2 = DEGFRF[2]
  DEGF3 = DEGFRF[3]
  DEGF4 = DEGFRF[4]
  DEGF5 = DEGFRF[5]
  DEGF6 = DEGFRF[6]
  DEGF7 = DEGFRF[7]
  DEGF8 = DEGFRF[8]
  DEGF9 = DEGFRF[9]
  DEGF10 = DEGFRF[10]
}
###################################
SOL= Input[13,l]
STORM = STORM + 1

METHAP=as.numeric(as.character(Input[14,l]))

if(METHAP <= 0.0) {
  KMETP = 0.0
  METHAP = 0.00
  HYDHAP=as.numeric(as.character(Input[15,l]))
  
  if(HYDHAP <= 0.0) {
    KHYDP = 0.0
  } else {
    KHYDP = log(2.0) / HYDHAP
    }
  } else {
    HYDHAP= 0.0                   #added
    KMETP = log(2.0) / METHAP
    KHYDP = 0.0                  # added
    }
#################################################
FOTHAP=as.numeric(as.character(Input[16,l]))

if(FOTHAP <= 0.0) {
  KFOTP = 0.0
} else {
  KFOTP = (log(2.0) / FOTHAP) / 124
}

KDEGP = KHYDP + KFOTP + KMETP

if(KDEGP <= 0.0) {
  DEGHAP = 1000000
} else {
  DEGHAP = log(2.0) / KDEGP
}

PSTMSF <- c()
for(I in 1:600){
  PSTMSF[I] = 0.0
}

PSTMSP <- c()
for(I in 1:600){
  PSTMSP[I] = 0.0
}

I = 1
PSTMSF[1] = APPRAT

I = 1
PSTMSP[1] = APPRAT

if(METHAF <= 0.0) {
  KDEGF = 0.0
} else {
  KDEGF = log(2.0) / METHAF
}
###################################################
if(APPNUM == 1){
  PSTMSF[1] = APPRAT
}else{
  for(I in 2:APPNUM){
  PSTMSF[I] = PSTMSF[I-1] * exp(-KDEGF*APSPAC) + APPRAT
  }
}

if(APPNUM == 1){
  PSTMSP[1] = APPRAT
}else{
for(I in 2:APPNUM){
  PSTMSP[I] = PSTMSP[I-1] * exp(-KDEGP*APSPAC) + APPRAT
}
}

KADS1 = (9.2529+1.751*KOC) / (1.341E6+KOC)
SDINIT=(1.12085*PSTMSP[APPNUM]*DRIFT*WBAREA*exp(-KADS1))/144.146
KADSUS = (9366.5+12.4*KOC) / (655000+KOC)

for(I in 1:375){
  ADSFRS[I] = exp(-KADSUS*I)
}

SDFIN = 1.12085 * PSTMSP[APPNUM] * DRIFT * WBAREA * 0.13875 * ((37.0388+9E-6*KOC) / (750+KOC))

for(I in 1:375){
  SDCONC[I] = (SDFIN + ADSFRS[I] * (SDINIT-SDFIN)) * exp(-KDEGP*I)
}

ROINIT = (1.12085 * PSTMSF[APPNUM] * APPEFF * ROAREA * PCTSRO * KDFRAC * DEGFRF[STORM] / INCORP) / 144.146
KADSUR = (5742.9+7.6*KOC) / (405000+KOC)

for(I in 1:375){
  ADSFRR[I] = exp(-KADSUR*I)
}

ROFIN = 1.12085 * PSTMSF[APPNUM] * APPEFF * PCTSRO * ROAREA * DEGFRF[STORM] * ((157.845+4.3E-6*KOC**1.215) / (510+KOC**1.215)) / INCORP / (6.262 * 7.2073)

for(I in 1:375){
  ROCONC[I] = (ROFIN+ADSFRR[I] * (ROINIT-ROFIN)) * exp(-KDEGP*I)
}

if(METHOD == 'A' | METHOD == 'a' | METHOD == 'B' | METHOD == 'b' | METHOD == 'C' | METHOD == 'c'){
  CONC0 = ROINIT + SDCONC[STORM]
  for(I in 1:375){
    CHRONIC[I] = ROCONC[I] + SDCONC[I+STORM-1]
  }

} else if (METHOD == 'D' | METHOD == 'd'){
  CONC0 = ROINIT
  for(I in 1:375){
    CHRONIC[I] = ROCONC[I]
  }
}

SUM4 = 0.0
SUM21 = 0.0
SUM60 = 0.0
SUM90 = 0.0
SUM365 = 0.0

for(I in 1:3){
  SUM4 = SUM4 + CHRONIC[I]
}
CONC4 = (CONC0 + SUM4) / 4

for(I in 1:20){
  SUM21 = SUM21 + CHRONIC[I]
}
CONC21 = (CONC0 + SUM21) / 21

for(I in 1:59){
  SUM60 = SUM60 + CHRONIC[I]
}
CONC60 = (CONC0 + SUM60) / 60

for(I in 1:89){
  SUM90 = SUM90 + CHRONIC[I]
}
CONC90 = (CONC0 + SUM90) / 90

for(I in 1:364){
  SUM365 = SUM365 + CHRONIC[I]
}
CON365 = (CONC0 + SUM365) / 365

CONC0 = PCA * CONC0
CON365 = PCA * CON365

CONC0 = FLADJP * CONC0
CON365 = FLADJA * CON365

if(KD >= 10.0) {
  AVFAC = 2.0 * log10(KD)
  CON365 = CON365 * AVFAC
}

if(CONC0 >= SOL) {CONC0 = SOL}
if(CONC4 >= SOL) {CONC4 = SOL}
if(CONC21 >= SOL) {CONC21 = SOL}
if(CONC60 >= SOL) {CONC60 = SOL}
if(CONC90 >= SOL) {CONC90 = SOL}
if(CON365 >= SOL) {CON365 = SOL}

# if(CONC0 >= 1.0){
#   UNITS = 'MILLIGRAMS/LITER (PPM)'
# }
# 
# if(CONC0 < 1.0 & CONC0 >= 0.001){
#   CONC0 = CONC0 * 1000
#   CONC4 = CONC4 * 1000
#   CONC21 = CONC21 * 1000
#   CONC60 = CONC60 * 1000
#   CONC90 = CONC90 * 1000
#   CON365 = CON365 * 1000
#   UNITS = 'MICROGRAMS/LITER (PPB)'
# }
# 
# if(CONC0 < 0.001){
#   CONC0 = CONC0 * 1000000
#   CONC4 = CONC4 * 1000000
#   CONC21 = CONC21 * 1000000
#   CONC60 = CONC60 * 1000000
#   CONC90 = CONC90 * 1000000
#   CON365 = CON365 * 1000000
#   UNITS = 'NANOGRAMS/LITER (PPTr)'
# }

# if(SOL >= 1.0){
#   SOLUNI = 'PPM'
# }
# 
# if(SOL < 1.0 & SOL >= 0.001) {
#   SOL = SOL * 1000
#   SOLUNI = 'PPB'
# }
# 
# if(SOL < 0.001) {
#   SOL = SOL * 1000000
#   SOLUNI = 'PPTr'
# }
########################
# Start writing to an output file
 # OUTFIL="out_first.txt"
 # sink(OUTFIL, append = T)

# if(ADSORP == 'A' | ADSORP == 'a'){
#     cat(sprintf("\n\n\n   RUN No. %3.0f FOR  %s   ON  %s   * INPUT VALU 2ES * ", CODE,CHMNAM,CROP))
#     cat('\n   --------------------------------------------------------------------')
#     cat('\n   RATE (#/AC)   No.APPS &   SOIL  SOLUBIL  APPL TYPE  %CROPPED INCORP')
#     cat("\n    ONE(MULT)    INTERVAL    Koc   (PPM)   (%DRIFT)     AREA   (IN)")
#     cat('\n   --------------------------------------------------------------------\n')
#     cat(sprintf(" %7.3f%s%7.3f%s  %d %d   %10.1f %7.1f   %s%s%4.1f%s %5.1f  %4.1f", APPRAT,'(',PSTMSF[APPNUM],')',APPNUM,APSPAC,KOC,SOL,SPTYPE,'(',DRIFT*100,')',PCA*100.0,INCORP,"\n"))
# 
# } else if (ADSORP == 'B' | ADSORP == 'b'){
#   cat(sprintf("   RUN No. %3.0f FOR  %s   ON  %s   * INPUT VALU 2ES * ", CODE,CHMNAM,CROP))
#   cat('\n   --------------------------------------------------------------------')
#   cat('\n   RATE (#/AC)   No.APPS &   SOIL  SOLUBIL  APPL TYPE  %CROPPED INCORP')
#   cat("\n    ONE(MULT)    INTERVAL    Kd    (PPM)   (%DRIFT)     AREA   (IN)")
#   cat('\n   --------------------------------------------------------------------\n')
#   cat(sprintf(" %7.3f%s%7.3f%s  %d %d   %10.1f %7.1f   %s%s%4.1f%s %5.1f  %4.1f", APPRAT,'(',PSTMSF[APPNUM],')',APPNUM,APSPAC,KD,SOL,SPTYPE,'(',DRIFT*100,')',PCA*100.0,INCORP,"\n"))
# }
# 
# title2 = paste(
#   "\n\n\n   FIELD AND RESERVOIR HALFLIFE VALUES (DAYS)
#    --------------------------------------------------------------------
#    METABOLIC  DAYS UNTIL  HYDROLYSIS   PHOTOLYSIS   METABOLIC  COMBINED
#     (FIELD)  RAIN/RUNOFF  (RESERVOIR)  (RES.-EFF)   (RESER.)   (RESER.)
#    --------------------------------------------------------------------\n", sep = "\n")
# 
# if(DEGHAP > 999999) DEGHAP = 0.0
# 
# if(HYDHAP < 0.0){
#   cat(sprintf(title2,"\n"))
#   cat(sprintf("   %7.2f     %2d          %s       %6.2f%s%8.2f %6.2f   %7.2f", METHAF,STORM-1,'N/A ',FOTHAP,'-',FOTHAP*124,METHAP,DEGHAP))
# 
#   } else {
#   cat(sprintf(title2,"\n"))
#   cat(sprintf("   %7.2f     %2d          %7.2f  %6.2f%s%8.2f   %6.2f    %6.2f", METHAF,STORM-1,HYDHAP,FOTHAP,'-',FOTHAP*124,METHAP,DEGHAP))
# }

####################################
# if(l == 2){
# cat(sprintf("   UNTREATED WATER CONC (PPM) Ver 1.1.1  MAR 26, 2008\n"))
# cat("   ----------------------------------------------------------
#         PEAK DAY  (ACUTE)      ANNUAL AVERAGE (CHRONIC)
#          CONCENTRATION             CONCENTRATION
#    ---------------------------------------------------------------\n")
# cat(sprintf("            %7.3f                     %7.3f   \n",CONC0,CON365))
# }else{
# cat(sprintf("            %7.3f                     %7.3f   \n",CONC0,CON365))
# }
# 
# sink()

CONC0_df <- rbind(CONC0_df, CONC0)
CON365_df <- rbind(CON365_df, CON365)
CODE_df <- rbind(CODE_df, CODE)
CHMNAM_df <- rbind(CHMNAM_df, CHMNAM)
CROP_df <- rbind(CROP_df, CROP)
}

res_df <- data.frame(CODE_df,
                     CROP_df,
                     CHMNAM_df,
                     CONC0_df*1000,
                     CON365_df*1000)

names(res_df) <- c("Run", "Crop", "Product", "Peak.day.accute", "Annual.average.chronic")

#res_df2=format.df(res_df, digits=4, numeric.dollar=F)
# 
#   output1=readLines("out_first.txt")
#   file.remove("out_first.txt")

#return(list(res_df2, output1, res_df))
return(list(res_df))
}





