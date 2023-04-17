#!/usr/bin/env Rscript
suppressPackageStartupMessages(library("dplyr"))

class_colnumber <- function(head){
  if(length(grep("^super.*class$",head,perl = T,ignore.case = T))==1){
    return(grep("^super.*class$",head,perl = T,ignore.case = T)[1])
  }else if(length(grep("^class$",head,perl = T,ignore.case = T))==1){
    return(grep("^class$",head,perl = T,ignore.case = T)[1])
  }else if(length(grep("^sub.*class$",head,perl = T,ignore.case = T))==1){
    return(grep("^sub.*class$",head,perl = T,ignore.case = T)[1])
  }else{
    cat("Classification not found, please check the input file!\n")
    q()
  }
}

class_summary <- function(annotation_file){
  mat <-  read.table(annotation_file, header=T, sep="\t",check.names=F,quote="",comment.char = "")
  col_number=class_colnumber(colnames(mat))
  mat <- as.data.frame(mat[,col_number])
  colnames(mat) <- c("class")
  mat <- filter(mat,class != "0" & class != "-")
  freq_df <- as.data.frame(table(mat$class))
  colnames(freq_df) <- c("class","count")
  freq_df <- freq_df[order(freq_df$count,decreasing = T),]
  return(freq_df)
}

main <- function(conf){
  source(conf)
  raw_outdir <-outdir
  for (ion_mode in c("POS", "NEG")){
    annotation_file <- ifelse(ion_mode == "POS", Annotation_pos, Annotation_neg)
    outdir <- paste0(raw_outdir, "/", ion_mode, "/")
    if(!file.exists(outdir)){
        dir.create(outdir,recursive=TRUE)
    }
    freq_df <- class_summary(annotation_file)
    write.table(freq_df,file = paste0(outdir,"/classification.stat.xls"),sep = "\t",row.names = F,col.names =T,quote=FALSE)
  }
}

## 开始运行
argv <- commandArgs(TRUE)
if (length(argv) == 1) {
  main(argv[1])
}

