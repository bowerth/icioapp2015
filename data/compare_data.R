
## compare data before replace

path.data.old <- file.path(dbpath, "GitHub", "icioapp2015", "data")
## path.data.new <- file.path("/", "FS-CH-1", "Transfer", "STI", "EAS", "tiva", "Rdata")
path.data.new <- file.path("D:", "icioapp2015_data", "2015-07-03")
## list.files(path.data.old)
## list.files(path.data.new)

namefile <- c("CVB", "FDTTLexINVNT", "GRTR")
namefile <- paste0("DATA.ICIOecon", namefile)

## file <- namefile[1]
## file <- namefile[2]
## file <- namefile[3]
## for (file in namefile) {
for (file in namefile[2:3]) {
    ##
    env.old <- new.env()
    env.new <- new.env()
    ##
    load(file = file.path(path.data.old, paste0(file, ".Rdata")), envir = env.old)
    load(file = file.path(path.data.new, paste0(file, ".Rdata")), envir = env.new)
    ##
    data.old <- mget(ls(envir = env.old), envir = env.old)
    data.new <- mget(ls(envir = env.new), envir = env.new)
    ##
    cat(paste0('\n---\nCompare file ',
               file,
               '\n---\n\n'))
    str(data.old)
    str(data.new)
}
