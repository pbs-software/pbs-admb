.onLoad <- function(libname, pkgname)
{
#	.initOptions()
	pkg_info <- utils::sessionInfo( package="PBSadmb" )$otherPkgs$PBSadmb
	if( is.character( pkg_info$Packaged ) )
		pkg_date <- strsplit( pkg_info$Packaged, " " )[[1]][1]
	else
		pkg_date  <- "unkown"
	
	userguide_path <- system.file( "doc/PBSadmb-UG.pdf", package = "PBSadmb" )
	
	packageStartupMessage("
-----------------------------------------------------------
PBS ADMB ", pkg_info$Version, " -- Copyright (C) 2008-2011 Fisheries and Oceans Canada

A complete user guide 'PBSadmb-UG.pdf' is located at 
", userguide_path, "

Packaged on ", pkg_date, "
Pacific Biological Station, Nanaimo

Type admb() to start a GUI for operating ADMB.
-----------------------------------------------------------


")
}
.onAttach = function(libname, pkgname){
	.initOptions()
}

