.First.lib <- function(lib,pkg)
{
	.initOptions()

	pkg_info <- utils::sessionInfo( package=pkg )$otherPkgs[[ pkg ]]
	pkg_date <- strsplit( pkg_info$Packaged, " " )[[1]][1]
	
	userguide_path <- system.file( "doc/PBSadmbwin-UG.pdf", package = pkg )
	
	cat("
PBS ADMB", pkg_info$Version, "-- Copyright (C) 2008-09 Fisheries and Oceans Canada

A complete user guide 'PBSadmbwin-UG.pdf' is located at 
", userguide_path, "

Packaged on", pkg_date, "
Pacific Biological Station, Nanaimo

Type admb() to start a GUI for operating ADMB.


")

}
