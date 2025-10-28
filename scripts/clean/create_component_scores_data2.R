### This script cleans up the component scores data
### with sensible variable names and subject identifiers,
### and only including the components that explain at least
### 1% of the variance in the data
###
### Ellyn Butler
### October 16, 2025 - October 27, 2025

# Load subjects' component data
indir <- '/projects/b1108/projects/personalized_versus_group/data/processed/neuroimaging/tabulated/'

#### Spatial
netnames <- c('VisCent', 'VisCeri', 'SomMotA', 'SomMotB', 'DorsAttnA',
              'DorsAttnB', 'SalVenAttnA', 'SalVenAttnB', 'LimbicA',
              'LimbicB', 'ContA', 'ContB', 'ContC', 'DefaultA',
              'DefaultB', 'DefaultC', 'TempPar')
for (j in 1:17) {
    df <- read.csv(paste0(indir, 'component_scores_thresholded_', netnames[j], '_svd.csv'))
    ve <- read.csv(paste0(indir, 'variance_explained_thresholded_j', j, '_svd.csv'))

    # Select for columns that explain at least 1% of the variance
    df <- df[,1:nrow(ve[ve$variance_explained > 0.01,])]
    if (typeof(df) == 'double') {
        df <- as.matrix(df)
    }
    names(df) <- paste0(netnames[j], '_component', 1:ncol(df))
    assign(paste0('df', j), df)
}
df <- cbind(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10, df11, df12, 
            df13, df14, df15, df16, df17)

temp_subjs <- read.csv(paste0(indir, 'prior_subjects_2025-09-19.csv'))
df <- cbind(temp_subjs[, c('subid', 'sesid')], df)
write.csv(df, paste0(indir, 'spatial_components_', format(Sys.Date(), "%Y-%m-%d"), '.csv'), row.names = FALSE)

#### FC
df <- read.csv(paste0(indir, 'component_scores_FC_svd.csv'))
ve <- read.csv(paste0(indir, 'variance_explained_FC_svd.csv'))

# Select for columns that explain at least 1% of the variance
df <- df[,1:nrow(ve[ve$variance_explained > 0.01,])]
if (typeof(df) == 'double') {
    df <- as.matrix(df)
}
names(df) <- paste0('FC_component', 1:ncol(df))
df <- cbind(temp_subjs[, c('subid', 'sesid')], df)
write.csv(df, paste0(indir, 'FC_components_', format(Sys.Date(), "%Y-%m-%d"), '.csv'), row.names = FALSE)

