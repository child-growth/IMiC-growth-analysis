library(googledrive)

search_keyword = "YEAR"
folder_id = "GOOGLE_FOLDER_ID"

query=paste0("name contains '", search_keyword, "' and '", folder_id, "' in parents")
files_to_download = drive_find(q = query)

for(i in 1:nrow(files_to_download)){
  tryCatch(
    expr = {
      drive_download(files_to_download[i,], overwrite=FALSE)   
    },
    error = function(e) {          
      message("For file ", files_to_download[i,], ": ", e)
    }
  )
}
