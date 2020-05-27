# Title     : TODO
# Objective : TODO
# Created by: JasonY
# Created on: 5/27/20

source('tools.R')

all_maf_files = fetch_files('input_data', 'maf')

combined_maf_df = data.frame()

for(maf_file in all_maf_files){
  #read a maf file
  maf <- maftools::read.maf(maf = maf_file)
  maf_df <- data.frame(maf@data)

  #define the variables to keep
  key_vars <- c("Hugo_Symbol","Chromosome","Start_Position","End_Position","Variant_Classification","Variant_Type",
               "Reference_Allele","Tumor_Seq_Allele1","Tumor_Seq_Allele2","Tumor_Sample_Barcode","Mutation_Status",
               "HGVSc","HGVSp","HGVSp_Short")
  maf_df <- maf_df[, key_vars]

  #rename variables to match the mysql database settings
  maf_df <- data.table::setnames(maf_df, old = c("Hugo_Symbol","Variant_Classification", "Tumor_Sample_Barcode"),
                            new = c('gene_name','mut_group', 'sample_id'))

  combined_maf_df <- rbind(combined_maf_df, maf_df)
}

#convert the combined maf infos to data frame
combined_maf_df <- data.frame(combined_maf_df)

#change all factor var to character var
combined_maf_df = df_factor_to_char(combined_maf_df)

#limit target bar code to sample level
combined_maf_df[, 'sample_id'] <- sapply(combined_maf_df[, 'sample_id'], keepf4)

write.csv(combined_maf_df, 'output_data/target_mutation_info.csv', row.names=F)

#get gene expression profile
target_expr <- readRDS('./input_data/target_expr.rds')

part <- target_expr[,1:5]

#merge expr with mutation
expr_mut <- dplyr::inner_join(part, combined_maf_df, by='sample_id')

cc <- expr_mut
#get product of unique samples and genes
gene_name <- unique(names(target_expr)[2:length(names(target_expr))])
sample_id<- unique(cc[ ,'sample_id'])

gs_df <- data.frame(tidyr::crossing(sample_id, gene_name))
gs_df <- df_factor_to_char(gs_df)

gs_df  <- dplyr::left_join(gs_df , combined_maf_df, by=c('sample_id'='sample_id', 'gene_name'='gene_name'))

gs_df <- gs_df[, c('gene_name', 'sample_id', 'mut_group')]



#gs_df[ ,'mut_group'] <- sapply(gs_df[ ,'mut_group'], function (x){
#  if(is.na(x)){
#    return('Wildtype')
#  }else{
#    return(x)
#  }
#})
#
#print(any(is.na(gs_df)))

#savedata(combined_maf_df, 'target_mut_info_bbk')
#savedata(gs_df, 'target_mut_bbk')







