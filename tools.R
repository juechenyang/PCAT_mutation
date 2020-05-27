# Title     : TODO
# Objective : TODO
# Created by: JasonY
# Created on: 5/27/20

fetch_files <- function(dir, type){
    files <- list.files(path = dir, pattern = paste0('\\.', type, '$'))
    if(length(files)==0){
        return(c())
    }else{
        files <- sapply(files, function(x){
            a = file.path(dir, x)
            return(a)
        })
        names(files) <- NULL
        return(files)
    }
}

single_gene_special_transfer <- function(gene){
    if(grepl("^\\d", gene, perl = T)){
        gene <- paste('X', gene, sep='')
    }else if(grepl('-', gene)){
        gene <- gsub('-','..', gene)
    }
    return(gene)
}

single_gene_special_r_transfer <- function(gene){
    if(grepl('\\.\\.', gene)){
        gene_v <- strsplit(gene, '\\.\\.')[[1]]
        # if(length(gene_v)>2){
        #     gene <- paste(gene_v[1:length(gene_v)-1], collapse='-')
        #     gene <- paste(gene, gene_v[length(gene_v)], sep='.')
        # }else{
        #     gene <- paste(gene_v[1:length(gene_v)], collapse='.')
        # }
        gene <- paste(gene_v[1:length(gene_v)], collapse='-')
    }
    if(grepl("^X\\d", gene, perl = T)){
        gene = substr(gene, 2, nchar(gene))
    }
    return(gene)
}

keepf4 = function(x){
    a = strsplit(x, '-')[[1]]
    first_four = a[1:4]
    total <- paste(first_four, collapse='-')
    return(total)
}

df_factor_to_char <- function(df){
    indx <- sapply(df, is.factor)
    df[indx] <- lapply(df[indx], function(x) as.character(x))
    return(df)
}