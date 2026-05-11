# author: Kwame Okrah
# date: 2026-03-05

# read_merged_data (read QCd merged data)
read_merged_data = function(merged_data_path) {
    res = read.csv(merged_data_path, header=TRUE, check.names=FALSE,
                   stringsAsFactors=FALSE)
    
    rownames(res) = paste0(res$"Platename", "|", res$"Plate Position")
    
    return(res)
}
