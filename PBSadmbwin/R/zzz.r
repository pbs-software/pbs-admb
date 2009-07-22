.First.lib <- function(lib,pkg)
{
	#pkg_info <- utils::sessionInfo( package="PBSadmbwin" )$otherPkgs$PBSadmbwin
	#pkg_date <- strsplit( pkg_info$Packaged, " " )[[1]][1]
	
	#userguide_path <- system.file( "doc/PBSadmbwin-UG.pdf", package = "PBSadmbwin" )
	
	cat("
PBS ADMB for Windows 0.50.r4 -- Copyright (C) 2008-09 Fisheries and Oceans Canada

A complete user guide 'PBSadmbwin-UG.pdf' is located at 
C:/Apps/R/R291/library/PBSadmbwin/doc/PBSadmbwin-UG.pdf 

Packaged on 2009-07-22
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
