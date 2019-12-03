#Input <- read_xlsx('inp_scigrow.xlsx', col_names=F)

scigrow <- function(Input) {

  if (dim(Input)[1] != 6) {
    stop("Input data dimension is not compatible with SCI-GROW input file! Please choose SCI-GROW input file.")
  }
  
##----------Sci-Grow model--------------------------#
Conc_gw <- data.frame()

Crop_df <- c()
Product_df <- c()
GW_conc_df <- c()
Koc_flag_df <- c()
res_df <- c()
CODE_df <- c()

#****** half life dependent conditions***************
for (l in 2:dim(Input)[2]){
  CODE = as.numeric(l-1)
  
  D <- log10(as.numeric(as.character(Input[5,l])) + 5)
  
  if(Input[6,l] < 6) {
    C <- log10(as.numeric(as.character(Input[6,l]))/6)
    RILP <- C * D
  } else if(between(as.numeric(as.character(Input[6,l])),6 ,1500)){
    C <- log10(as.numeric(as.character(Input[6,l])) - 5)
    RILP <- C*(4-D)
  } else if (as.numeric(as.character(Input[6,l])) > 1500) {
    C <- log10(1500)
    RILP <- C * (4 - D)
  }
  if(as.numeric(as.character(Input[5,l])) > 9995){
    Koc_flag <- paste("Koc out of bounds", sep="")
    normalized_conc <- 0.006
  } else {
    F <- -2.241 + 0.610 * RILP 
    normalized_conc <- 10 ** F
  }
  #****special condition if Koc  > 9995 *********************** 
  #default condition when Koc is gretater than 9995" 
  total_pesticide_use <- as.numeric(as.character(Input[3,l])) * as.numeric(as.character(Input[4,l]))		#total pesticide applied per year
  conc <- normalized_conc * total_pesticide_use				#output concentration
  #************************************************************
  dig <- function(x, k) as.numeric(as.character(trimws(format(round(x, k), nsmall=k))))
  
  Koc_flag <- ifelse(as.numeric(as.character(Input[5,l]))>9995,"Koc out of bounds", "Koc < 9995 - OK!")
  
  Crop=Input[[1,l]] 
  Product=Input[[2,l]]
  GW_conc=dig(conc,4)
  
#  Conc_df<- cbind(Crop, format(Product,justify="left"), dig(conc,4), Koc_flag)
#  Conc_gw<- rbind(Conc_gw, Conc_df)
#  
  Crop_df = rbind(Crop_df, Crop)
  Product_df = rbind(Product_df, Product)
  GW_conc_df = rbind(GW_conc_df, GW_conc)
  Koc_flag_df = rbind(Koc_flag_df, Koc_flag)
  CODE_df <- rbind(CODE_df, CODE)
  
#-------------Model Output-------------#
#   OUTFIL="out_scigrow.txt"
#   sink(OUTFIL, append = T)
#   if(l==2){
#  cat("   Crop             Product                         GW_conc  Koc_Flag\n")
#  cat(sprintf("%-12s  %-35s  %7.3f  %-20s\n",Crop, Product, GW_conc, Koc_flag))
#   }else{
#  cat(sprintf("%-12s  %-35s  %7.3f  %-20s\n",Crop, Product, GW_conc, Koc_flag))
#   }
#   sink()
 }

res_df = data.frame(CODE_df,
                    Crop_df, 
                    Product_df, 
                    GW_conc_df*1000, 
                    Koc_flag_df)

names(res_df) = c("Run", "Crop", "Product", "GW_conc", "Koc_flag")

#res_df2 = format.df(res_df, digits=4, numeric.dollar=F)

# output1=readLines("out_scigrow.txt")
# file.remove("out_scigrow.txt")
#test2<-capture.output(cat(output1, sep = "\n"))
return(list(res_df))
}

