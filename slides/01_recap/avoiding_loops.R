# avoiding_loops.R
#
# content: (1) Read log files
#          (2) Create data frame
#
# input: log files HAUM from 2016
# output: --
#
# last mod: May/13/2023, NW

# setwd("C:/Users/nwickelmaier/Nextcloud/Documents/MDS/2023ss/60100_master_thesis/data/HAUM Logs/")

#--------------- (1) Read log files ---------------

fnames <- dir("_2016", pattern = "*.log")[1:50]

dat_logs <- NULL

for (i in seq_along(fnames)) {
  cdat <- data.frame(logs = readLines(paste0("_2016/", fnames[i])), 
                     fileid = paste("logFile", sprintf("%03.f", i), sep = "_"))
  cdat$entryid = sprintf("%04.f", seq_len(nrow(cdat)))
  dat_logs <- rbind(dat_logs, cdat)
  rm(cdat)
}

dat_logs <- subset(dat_logs, dat_logs$logs != "")

head(dat_logs$logs)

#--------------- (2) Create data frame ---------------

system.time({
meta_data <- NULL

for (log in dat_logs$logs) {
  meta_data <- rbind(meta_data, data.frame(
      fileid  = dat_logs[dat_logs == log, "fileid"],
      entryid = dat_logs[dat_logs == log, "entryid"],
      weekday = gsub("^\\[(.{3}).*\\], \\[.*$", "\\1", log),
      month   = gsub("^\\[.{4}(.{3}).*\\], \\[.*$", "\\1", log),
      day     = gsub("^\\[.{8}(.[0-9]{1-2}).*\\], \\[.*$", "\\1", log),
      year    = gsub("^\\[.*([0-9]{4}).*\\], \\[.*$", "\\1", log),
      time    = gsub("^\\[.*[0-9]{4}.(.*)\\], \\[.*$", "\\1", log)
    )
  )
}
})

# with sapply():

system.time({
meta_data <- data.frame(
    fileid  = dat_logs$fileid,
    entryid = dat_logs$entryid,
    weekday = sapply(dat_logs$logs,
                     function(x) gsub("^\\[(.{3}).*\\], \\[.*$", "\\1", x),
                     USE.NAMES = FALSE),
    month   = sapply(dat_logs$logs,
                     function(x) gsub("^\\[.{4}(.{3}).*\\], \\[.*$", "\\1", x),
                     USE.NAMES = FALSE),
    day     = sapply(dat_logs$logs,
                     function(x) gsub("^\\[.{8}(.[0-9]{1-2}).*\\], \\[.*$", "\\1", x),
                     USE.NAMES = FALSE),
    year    = sapply(dat_logs$logs,
                     function(x) gsub("^\\[.*([0-9]{4}).*\\], \\[.*$", "\\1", x),
                     USE.NAMES = FALSE),
    time    = sapply(dat_logs$logs,
                     function(x) gsub("^\\[.*[0-9]{4}.(.*)\\], \\[.*$", "\\1", x),
                     USE.NAMES = FALSE)
  )
})

# function + sapply()

create_meta_data <- function(log) {
  res <- data.frame(
      fileid  = dat_logs[dat_logs == log, "fileid"],
      entryid = dat_logs[dat_logs == log, "entryid"],
      weekday = gsub("^\\[(.{3}).*\\], \\[.*$", "\\1", log),
      month   = gsub("^\\[.{4}(.{3}).*\\], \\[.*$", "\\1", log),
      day     = gsub("^\\[.{8}(.[0-9]{1-2}).*\\], \\[.*$", "\\1", log),
      year    = gsub("^\\[.*([0-9]{4}).*\\], \\[.*$", "\\1", log),
      time    = gsub("^\\[.*[0-9]{4}.(.*)\\], \\[.*$", "\\1", log)
    )
  res
}

system.time({
  res <- lapply(dat_logs$logs, create_meta_data)
})

meta_data <- do.call(rbind, res)

