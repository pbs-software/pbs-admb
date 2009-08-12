.First.lib <- function(lib,pkg)
{
	#pkg_info <- utils::sessionInfo( package="PBSadmbwin" )$otherPkgs$PBSadmbwin
	#pkg_date <- strsplit( pkg_info$Packaged, " " )[[1]][1]
	
	#userguide_path <- system.file( "doc/PBSadmbwin-UG.pdf", package = "PBSadmbwin" )
	
	cat("
PBS ADMB for Windows 0.50.08 -- Copyright (C) 2008-09 Fisheries and Oceans Canada

A complete user guide 'PBSadmbwin-UG.pdf' is located at 
.../library/PBSadmbwin/doc/PBSadmbwin-UG.pdf
in the R library tree.

Packaged on 2009-08-11
Pacific Biological Station, Nanaimo

Type admb() to start a GUI for operating ADMB.

")
#	cat("
#PBS ADMB for Windows", pkg_info$Version, "-- Copyright (C) 2008-09 Fisheries and Oceans Canada
#
#A complete user guide 'PBSadmbwin-UG.pdf' is located at 
#", userguide_path, "
#
#Packaged on", pkg_date, "
#Pacific Biological Station, Nanaimo
#
#Type admb() to start a GUI for operating ADMB.
#
#
#")
}
