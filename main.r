# main call to call_daily.r checking the log file if previous entry has new data, 
# Wrapped with simple error handling printed to log.txt and cleanup of the misc folder.

tryCatch(
	expr=source("call_daily.r"),
	warning=function(warn){
		warnLog <- paste("\n WARNING", "on", as.character(Sys.time()), "\t", warn)
		cat(warnLog, file="log.txt", append=TRUE)
	},
	error=function(err){
		errLog <- paste("\n ERROR", "on", as.character(Sys.time()), "\t", err)
		cat(errLog, file="log.txt", append=TRUE)
	},
	finally=file.remove(list.files("misc/", full.names=TRUE))
)