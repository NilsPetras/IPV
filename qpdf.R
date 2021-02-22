# add qpdf before check

old_path <- Sys.getenv("PATH")
Sys.setenv(PATH = paste(old_path, "C:/Program Files (x86)/qpdf-10.0.4/bin/", sep = ";"))
