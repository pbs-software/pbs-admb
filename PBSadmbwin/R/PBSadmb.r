# Authors: Jon T. Schnute, Rowan Haigh

#admb-----------------------------------2009-07-21
# Starts the primary GUI interface
#-----------------------------------------------RH
admb=function(prefix="",pkg="PBSadmbwin",wdf="admbWin.txt",optfile="ADopts.txt"){
	if (!require(PBSmodelling)) stop("!!!!!Install package PBSmodelling!!!!!")
	assign("PBSadmb",list(pkg=pkg,func="admb",useCols=NULL),envir=.GlobalEnv)
	if (exists(".ADopts",envir=.GlobalEnv)) rm(.ADopts,envir=.GlobalEnv) # remove previous hidden object

	pdir <- system.file(package=pkg)                 # package directory
	wdir <- paste(pdir,"/win",sep="")                # window description file directory
	adir <- paste(pdir,"/ADMB",sep="")               # ADMB directory
	edir <- paste(pdir,"/examples",sep="")           # examples directory
	tdir <- tempdir(); tdir <- gsub("\\\\","/",tdir) # temporary directory for R
	stripExt=function(x) { return(sub("[.].{1,3}$", "", x)) }

	wnam <- paste(wdir,wdf,sep="/")
	wtmp <- paste(tdir,wdf,sep="/")
	temp <- readLines(wnam)
	temp <- gsub("@editADfile",paste("\"editADfile(`",wtmp,"`)\"",sep=""),temp)
	if (is.null(prefix) || prefix=="") prefix="vonb"
	temp <- gsub("@prefix",prefix,temp)
	temp <- gsub("@admpath",adir,temp)
	temp <- gsub("@optfile",optfile,temp)
	#---examples---
	etpl <- basename(Sys.glob(file.path(edir,"*.tpl"))) # TPL files in examples directory
	eprf <- stripExt(etpl)                              # strip off extensions
	enew=character(0)
	for (i in eprf) 
		enew=c(enew,paste("menuitem label=",i," function=doAction action=\"copyFiles(`",
			i,".`,dir0=`",edir,"`)\"",sep=""))
	temp <- gsub("@nitems",length(eprf),temp)
	temp <- gsub("@menuitems",paste(enew,collapse="\n\t"),temp)
	temp <- gsub("@pkg",pkg,temp)
	writeLines(temp,con=wtmp)
	createWin(wtmp) #TODO use astext=TRUE
	.win.initAD()
	invisible() }
#---------------------------------------------admb

initAD <- function(optfile="ADopts.txt") {
  fileOK <- file.exists(optfile); #in the user's working directory
  if (fileOK) 
    readADopts(optfile)
  else cat(paste("No AD options file: ","\n",optfile,sep=""));
  #guess at empty fields

  if( exists( ".ADopts" ) == FALSE )
	  .ADopts <<- list()
	
  #guess ADMB path
  if( is.null( .ADopts[[ "admpath" ]] ) || .ADopts$admpath == "" ) {
	tmp <- findProgram( "tpl2rem" )
	if( !is.null( tmp ) )
		.ADopts$admpath <<- dirname( tmp )
  }

  #guess gcc path
  if( is.null( .ADopts[[ "gccpath" ]] ) || .ADopts$gccpath == "" ) {
	tmp <- findProgram( "g++" )
	if( !is.null( tmp ) )
		.ADopts$gccpath <<- dirname( tmp )
  }

  #guess editor
  if( is.null( .ADopts[[ "editor" ]] ) || .ADopts$editor == "" ) {
	tmp <- findProgram( "kate" )
	if( !is.null( tmp ) )
		.ADopts$admpath <<- dirname( tmp )
	tmp <- findProgram( "notepad" )
	if( !is.null( tmp ) )
		.ADopts$admpath <<- dirname( tmp )
	tmp <- findProgram( "gvim" )
	if( !is.null( tmp ) )
		.ADopts$admpath <<- dirname( tmp )
  }

  invisible(fileOK); };

.win.initAD=function(winName="PBSadmb") {
	getWinVal(scope="L",winName=winName)
	initAD(optfile=optfile)
	try(setWinVal(.ADopts[c("admpath","gccpath","editor")],winName=winName),silent=TRUE) 
	.win.checkADopts()
	invisible() }

makeADopts <- function(admpath,gccpath,editor,ver="gcc421") {
	opts <- list(admpath=admpath, gccpath=gccpath, editor=editor,ver=ver)
	for(i in names(opts)) {setPBSoptions(i,opts[[i]])} # for compatibility with PBSoptions
	assign(".ADopts",opts,envir=.GlobalEnv) }
	
.win.makeADopts=function(winName="PBSadmb"){
	getWinVal(scope="L",winName=winName)
	makeADopts(admpath,gccpath,editor) 
	invisible() }

writeADopts <- function(opts=.ADopts,optfile="ADopts.txt") {
  writeList(opts,optfile,format="P",comments=" Options for PBSadmb");
  invisible(opts); };

.win.writeADopts=function(winName="PBSadmb") {
	getWinVal(scope="L",winName=winName)
	writeADopts(optfile=optfile) 
	invisible() }

readADopts <- function(optfile="ADopts.txt") {
	opts <- readList(optfile)
	opts=sapply(opts,paste,collapse=" ",simplify=FALSE) # for ADopts paths with no enclosing quotes
	for(i in names(opts)) {setPBSoptions(i,opts[[i]])}  # for compatibility with PBSoptions
	assign(".ADopts",opts,envir=.GlobalEnv) }

.win.readADopts=function(winName="PBSadmb") {
	getWinVal(scope="L",winName=winName)
	if (file.exists(optfile)) {
		readADopts(optfile=optfile)
		setWinVal(.ADopts[c("admpath","gccpath","editor")],winName="PBSadmb") }
	else {
		mess=paste("Options file '",optfile,"' does not exist",sep="")
		showAlert(mess); stop(mess) }
	invisible() }

checkADopts=function(opts=.ADopts, check=c("admpath","gccpath","editor"),
     warn=TRUE, popup=FALSE) {
	# Check that .ADopts has all required components and that links point to actual files on the hard drive.
	if (!exists(".ADopts",envir=.GlobalEnv)) initAD()
	sAF=options()$stringsAsFactors; options(stringsAsFactors=FALSE)
	mess=list()
	for (i in names(opts)) {
		if (!any(i==check))
			next
		ii=ipath=opts[[i]]
		if (i=="admpath") {
			ipath=paste(ii,"/bin",sep="")
			if( .Platform$OS.type == "windows" )
				progs=c("tpl2cpp.exe","tpl2rem.exe")
			else
				progs=c("tpl2cpp","tpl2rem") #TODO verify these names
		}
		else if (i=="gccpath")
			if( .Platform$OS.type == "windows" )
				progs="g++.exe" 
			else
				progs="g++" 
		else if (i=="editor") {
			ipath=dirname(ii)
			progs=basename(ii)
		}
		target=paste(ipath,progs,sep="/")
		istatus=file.exists(target); names(istatus)=progs
		mess[[ipath]]=istatus
	}
	ADstatus=all(unlist(mess)==TRUE); attr(ADstatus,"status")=mess
	vmess=unlist(mess)
	names(vmess)=paste(rep(names(mess),sapply(mess,length,simplify=FALSE)),
		unlist(sapply(mess,names,simplify=FALSE)),sep="/")
	attr(ADstatus,"message")=vmess
	if (warn|popup) {
		if (all(vmess==TRUE)) {
			if(warn) cat("All programs found\n\n") }
			#if(popup) showAlert("All programs found","Continue","info") }
		else {
			badmess=paste("Programs not found:\n",paste(names(vmess)[!vmess],collapse="\n"),
				"\n\nEither alter '.ADopts' or remove it and alter 'ADopts.txt'.\n\n",
				"If using the ADMB GUI, alter the appropriate entry.\n\n",sep="")
			if (warn) cat(badmess)
			if (popup) showAlert(badmess,"User action required","warning") } }
	options(stringsAsFactors=sAF)
	invisible(ADstatus) }

.win.checkADopts=function(winName="PBSadmb") {
	getWinVal(scope="L",winName=winName)
	chkstat=checkADopts(opts=list(admpath=admpath,gccpath=gccpath,editor=editor),popup=TRUE)
	setWinVal(list(chkstat=ifelse(chkstat," OK"," Fix")),winName=winName)
	invisible(chkstat) }

startLog <- function(prefix) {
  p.log <- paste(prefix, ".log", sep="");
  if (file.exists(p.log)) file.remove(p.log);
  dstamp <- date();
  line1 <- paste("Log file for ", prefix, " (", dstamp, ")\n", sep="")
  writeLines(line1, con=p.log) }

.win.startLog=function(winName="PBSadmb") {
	getWinVal(scope="L",winName=winName)
	startLog(prefix=prefix) 
	invisible() }

appendLog <- function(prefix, lines) {
  p.log <- paste(prefix, ".log", sep="");
  if (!file.exists(p.log)) startLog(prefix);
  cat(lines, file=p.log, sep="\n", append=TRUE) }

.win.appendLog=function(winName="PBSadmb") {
	getWinVal(scope="L",winName=winName)
	appendLog(prefix=prefix,lines=lines) 
	invisible() }

.callSys <- function(..., wait=TRUE) { # note dots is not interpreted as expected, uses only first in list
	dots=list(...)
	if (.Platform$OS.type=="windows") {
		if("edit"%in%names(dots))
			dots[[1]]=paste("start \"\"",dots[[1]],sep=" ")
		out=shell(dots,intern=TRUE) }
	else {
		cmd <- unlist(list(...))
		if( wait == FALSE )
	   		out=system(cmd,wait=FALSE)
		else
	   		out=system(cmd,intern=TRUE)
	}
	invisible(out) }

#convAD---------------------------------2009-08-12
# Conver TPL file to CPP code.
#-------------------------------------------JTS/RH
convAD <- function(prefix, raneff=FALSE, logfile=TRUE, add=FALSE, verbose=TRUE, comp="GCC") {
  adp <- .ADopts$admpath;
  index=ifelse(raneff,2,1)
  cmd=parseCmd(prefix,index=index,admpath=adp,comp=comp )
#TODO bat files not portable - what does RE use on unix?
  if (raneff) {.makeREbat(); cmd=paste("re",prefix,collapse=" ")}  # RE model fix for ADMB_HOME nonsense
  if (logfile & !add) startLog(prefix);
  if (verbose) cat(cmd,"\n");
if (.Platform$OS.type=="windows") {
  cmd=.addQuotes(convSlashes(cmd))
}
  tplout <- .callSys(cmd)
  tplout2 <- c(cmd,tplout)
  if (logfile) appendLog(prefix, tplout2);
  if (verbose) cat(tplout, sep="\n");
  if (raneff) file.remove("re.bat")
  invisible(tplout2); };

.win.convAD=function(winName="PBSadmb") {
	isOK=.win.checkADopts(); if (!isOK) return()
	setWinVal(list(Mtime=matrix(NA,nrow=3,ncol=3)),winName=winName)
	time0=proc.time()[1:3]
	getWinVal(scope="L",winName=winName)
	convAD(prefix=prefix,raneff=raneff) #,logfile=logfile,add=add,verbose=verbose)
	Ttime=round(proc.time()[1:3]-time0,2)
	setWinVal(list("Mtime[1,1]"=Ttime[1],"Mtime[1,2]"=Ttime[2],"Mtime[1,3]"=Ttime[3]),winName=winName) 
	invisible(Ttime) }

#compAD---------------------------------2009-08-12
# Apparently "raneff" doesn't influence the compile stage,
# but the argument is preserved here for future development.
#-------------------------------------------JTS/RH
compAD <- function(prefix, raneff=FALSE, safe=TRUE, logfile=TRUE, add=TRUE, verbose=TRUE, comp="GCC") {
  adp <- .ADopts$admpath;
  gcp <- .ADopts$gccpath;
  index=ifelse(safe,4,3)
  cmd=parseCmd(prefix,index=index,admpath=adp,gccpath=gcp,comp=comp)
  if (logfile & !add) startLog(prefix);
  if (verbose) cat(cmd,"\n");
if (.Platform$OS.type=="windows") {
  cmd=.addQuotes(convSlashes(cmd))
}
  out1 <- .callSys(cmd)
  out2 <- c(cmd,out1)
  if (logfile) appendLog(prefix, out2);
  if (verbose) cat(out1, sep="\n");
  invisible(out2); };

.win.compAD=function(winName="PBSadmb") {
	isOK=.win.checkADopts(); if (!isOK) return()
	time0=proc.time()[1:3]
	getWinVal(scope="L",winName=winName)
	compAD(prefix=prefix,raneff=raneff,safe=safe) #,logfile=logfile,add=add,verbose=verbose)
	Ttime=round(proc.time()[1:3]-time0,2)
	setWinVal(list("Mtime[2,1]"=Ttime[1],"Mtime[2,2]"=Ttime[2],"Mtime[2,3]"=Ttime[3]),winName=winName) 
	invisible(Ttime) }

#linkAD---------------------------------2009-08-12
# Links binaries into executable
#-------------------------------------------JTS/RH
linkAD <- function(prefix, raneff=FALSE, safe=TRUE, logfile=TRUE, add=TRUE, verbose=TRUE, comp="GCC") {
  adp <- .ADopts$admpath;
  gcp <- .ADopts$gccpath;
  index=ifelse(safe&raneff,8,ifelse(!safe&raneff,7,ifelse(safe&!raneff,6,5)))
  cmd=parseCmd(prefix,index=index,admpath=adp,gccpath=gcp,comp=comp)
  if (logfile & !add) startLog(prefix);
  if (verbose) cat(cmd,"\n");
if (.Platform$OS.type=="windows") {
  cmd=.addQuotes(convSlashes(cmd))
}
  out1 <- .callSys(cmd)
  out2 <- c(cmd,out1)
  if (logfile) appendLog(prefix, out2);
  if (verbose) cat(out1, sep="\n");
  invisible(out2); };

.win.linkAD=function(winName="PBSadmb") {
	isOK=.win.checkADopts(); if (!isOK) return()
	time0=proc.time()[1:3]
	getWinVal(scope="L",winName=winName)
	linkAD(prefix=prefix,raneff=raneff,safe=safe) #,logfile=logfile,add=add,verbose=verbose) 
	Ttime=round(proc.time()[1:3]-time0,2)
	setWinVal(list("Mtime[3,1]"=Ttime[1],"Mtime[3,2]"=Ttime[2],"Mtime[3,3]"=Ttime[3]),winName=winName) 
	invisible(Ttime) }

makeAD <- function(prefix, raneff=FALSE, safe=TRUE, logfile=TRUE, verbose=TRUE) {
  convAD(prefix, raneff, logfile, add=FALSE, verbose);
  compAD(prefix, raneff, safe, logfile, add=TRUE, verbose);
  linkAD(prefix, raneff, safe, logfile, add=TRUE, verbose); };

.win.makeAD=function(winName="PBSadmb") {
	isOK=.win.checkADopts(); if (!isOK) return()
	setWinVal(list(Mtime=matrix(NA,nrow=3,ncol=3)),winName=winName)
	time0=proc.time()[1:3]
	getWinVal(scope="L",winName=winName)
	convAD(prefix=prefix,raneff=raneff) #,logfile=logfile,add=add,verbose=verbose)
	Ttime=round(proc.time()[1:3]-time0,2); time0=proc.time()[1:3]
	setWinVal(list("Mtime[1,1]"=Ttime[1],"Mtime[1,2]"=Ttime[2],"Mtime[1,3]"=Ttime[3]),winName=winName)
	compAD(prefix=prefix,raneff=raneff,safe=safe) #,logfile=logfile,add=add,verbose=verbose)
	Ttime=round(proc.time()[1:3]-time0,2); time0=proc.time()[1:3]
	setWinVal(list("Mtime[2,1]"=Ttime[1],"Mtime[2,2]"=Ttime[2],"Mtime[2,3]"=Ttime[3]),winName=winName)
	linkAD(prefix=prefix,raneff=raneff,safe=safe) #,logfile=logfile,add=add,verbose=verbose) 
	Ttime=round(proc.time()[1:3]-time0,2)
	setWinVal(list("Mtime[3,1]"=Ttime[1],"Mtime[3,2]"=Ttime[2],"Mtime[3,3]"=Ttime[3]),winName=winName) 
	invisible() }

runAD <- function(prefix, argvec="", logfile=TRUE, add=TRUE, verbose=TRUE)
{
	if( .Platform$OS.type == "windows" )
		p.exe <- paste(prefix,".exe",sep="")
	else
		p.exe <- paste("./", prefix, sep="" )
	if (logfile) {
		p.log=paste(prefix,".log",sep=""); p.log.log=paste(p.log,".log",sep="")
		if (file.exists(p.log)) file.copy(p.log,p.log.log,overwrite=TRUE) }
	p.cmd <- paste(p.exe, paste(argvec,collapse=" "), sep=" ");
	p.err <- paste("File",p.exe,"does not exist.\n",sep=" ");
	if (.Platform$OS.type=="windows") {
		p.cmd=.addQuotes(convSlashes(p.cmd))
	}
	if (file.exists(p.exe)) p.out <- .callSys(p.cmd) else p.out <- p.err;
	if (logfile) {
		if (!add) startLog(prefix)
		else if (file.exists(p.log.log)) file.copy(p.log.log,p.log,overwrite=TRUE)
		appendLog(prefix, p.out) }
	if (verbose) cat(p.out, sep="\n");
	invisible(p.out); };

.win.runAD=function(winName="PBSadmb") {
	setWinVal(list(Rtime=matrix(NA,nrow=1,ncol=3)),winName=winName)
	time0=proc.time()[1:3]
	getWinVal(scope="L",winName=winName)
	runAD(prefix=prefix,argvec=argvec,logfile=logfile,add=add,verbose=verbose)
	Ttime=round(proc.time()[1:3]-time0,2)
	setWinVal(list("Rtime[1,1]"=Ttime[1],"Rtime[1,2]"=Ttime[2],"Rtime[1,3]"=Ttime[3]),winName=winName) 
	invisible(Ttime) }

runMC <- function(prefix, nsims=2000, nthin=20, outsuff=".mc.dat",
                  logfile=FALSE, add=TRUE, verbose=TRUE) {
  outf <- paste(prefix,outsuff,sep="");
  arg1 <- paste("-mcmc",.asIs(nsims),"-mcsave",.asIs(nthin),sep=" ");
  setWinVal(list(argvec=arg1))
  arg2 <- "-mceval";
  runAD(prefix, arg1, logfile=logfile, add=add, verbose=verbose);
  p.out <- runAD(prefix, arg2, logfile=logfile, add=TRUE, verbose=verbose);
  writeLines(p.out,outf)
  invisible(p.out); };

.win.runMC=function(winName="PBSadmb") {
	time0=proc.time()[1:3]
	getWinVal(scope="L",winName=winName)
	runMC(prefix=prefix,nsims=nsims,nthin=nthin,logfile=logfile,add=add,verbose=verbose)
	Ttime=round(proc.time()[1:3]-time0,2)
	setWinVal(list("Rtime[1,1]"=Ttime[1],"Rtime[1,2]"=Ttime[2],"Rtime[1,3]"=Ttime[3]),winName=winName)
	PBSadmb$useCols<<-NULL
	invisible(Ttime) }

.win.run=function(winName="PBSadmb"){
	getWinVal(scope="L",winName=winName)
	if (runType=="mcmc") .win.runMC()
	else .win.runAD() 
	invisible()}

#editADfile-----------------------------2009-02-13
# Use the specified editor to edit a file.
#-------------------------------------------JTS/RH
editADfile <- function(fname) {
  if (!checkADopts(warn=FALSE)) {cat("Invalid options for PBSadmb\n"); stop()}
  #f.edit <- paste("start \"\"",.addQuotes(convSlashes(.ADopts$editor)),.addQuotes(convSlashes(fname)),sep=" ");
	if (.Platform$OS.type=="windows") {
  f.edit <- paste(.addQuotes(convSlashes(.ADopts$editor)),.addQuotes(convSlashes(fname)),sep=" ");
	} else {

  f.edit <- paste(.ADopts$editor,fname,sep=" ");
	}
  f.err  <- paste("File",fname,"does not exist.\n",sep=" ");
  if (file.exists(fname)) {.callSys(edit=f.edit, wait=FALSE); cat(f.edit,"\n"); f.out <- TRUE}
  else {cat(f.err); f.out <- FALSE};
  return(f.out); };

editAD <- function(prefix, suffix=c(".tpl",".cpp",".log")) {
	npref=length(prefix); nsuff=length(suffix); ed.out=logical(npref*nsuff); k=0
	for (i in 1:npref) {
		for (j in 1:nsuff) {
			fname=paste(prefix[i],suffix[j],sep=""); k=k+1; ed.out[k]=editADfile(fname) } }
  return(ed.out) }

.win.editAD=function(winName="PBSadmb") {
	getWinVal(scope="L",winName=winName)
	editAD(prefix=prefix); invisible() }
.win.editPLT=function() {
	pref=.findPrefix(".plt")
	editAD(prefix=pref,suffix=".plt"); invisible() }

showADargs <- function(prefix,ed=TRUE) {
	if( .Platform$OS.type == "windows" )
		p.exe <- paste(prefix,".exe", sep="")
	else
		p.exe <- paste("./", prefix, sep="") #TODO verify
	p.exe <- paste(prefix,".exe", sep="");
	p.arg <- paste(prefix,".arg", sep="");
	p.err <- paste("File",p.exe,"does not exist.\n",sep=" ");
	p.cmd <- paste(p.exe,"-?",sep=" ");
	p.cmd=.addQuotes(convSlashes(p.cmd))
	if (file.exists(p.exe)) p.out <- .callSys(p.cmd) else p.out <- p.err;
	if (ed) {writeLines(p.out,p.arg); editADfile(p.arg); }
	else {cat(paste(p.out,collapse="\n")); cat(paste(p.arg,collapse="\n")) }
	invisible(p.out) };

.win.showADargs=function(winName="PBSadmb") {
	getWinVal(scope="L",winName=winName)
	showADargs(prefix=prefix) 
	invisible() }

.win.showGUIargs=function(winName="PBSadmb"){ # display the argument vector in the GUI based on radio selection
	getWinVal(scope="L",winName=winName)
	if (runType=="normal") setWinVal(list(argvec=""),winName=winName)
	else if (runType=="mcmc") setWinVal(list(argvec=paste("-mcmc",.asIs(nsims),"-mcsave",.asIs(nthin),sep=" ")),winName=winName)
	else if (runType=="lprof") setWinVal(list(argvec="-lprof"),winName=winName)
	else setWinVal(list(argvec=argvec),winName=winName) 
	invisible() }

.win.findTPL=function(suffix=".tpl",winName="PBSadmb"){ 
	choice=.findPrefix(suffix) 
	chooseWinVal(choice,"prefix",winname=winName) 
	invisible() }

.win.viewRep=function(winName="PBSadmb") {
	getWinVal(scope="L",winName=winName); act=getWinAct()[1]
	if (!is.null(act) && act=="allnone") {
		if (allnone==1) {toView=rep(TRUE,length(toView)); pltView=TRUE}
		if (allnone==0) {toView=rep(FALSE,length(toView)); pltView=FALSE} }
	if (!is.null(act) && act=="check") allnone=3
	setWinVal(list(allnone=allnone,toView=toView,pltView=pltView),winName=winName)
	if (!is.null(act) && act=="open") {
		if (sum(toView)>0) {
			suffix=paste(".",names(toView)[toView],sep="")
			editAD(prefix,suffix) }
		if (pltView) .win.editPLT() }
	invisible() }

#readRep--------------------------------2009-07-09
# Imports the generated reports in various ways
#-----------------------------------------------RH
readRep=function(prefix, suffix=c(".cor",".rep",".std",".mc.dat"), global=FALSE) {
	findFormat=function(dat){ # extracted from Alex's readList()
		for (i in 1:length(dat)) {
			if (!any(grep("^[ \t]*[#`]", dat[i]))) {
				if (any(grep("^[ \t]*structure", dat[i]))) fileformat="D"
				else if (any(grep("^[ \t]*list", dat[i]))) fileformat="R"
				else if (any(grep("^[ \t]*\\$", dat[i])))  fileformat="P"
				else fileformat="U" # unknown fileformat detected
				break
		}	}
	return(fileformat) }
	sAF=options()$stringsAsFactors; options(stringsAsFactors=FALSE)
	if (missing(prefix) || any(is.null(c(prefix,suffix))) || any(c(prefix,suffix)=="")) return()
	flist=list()
	fname=paste(prefix,suffix,sep="")
	if (global) cat("R objects created:\n")
	for (i in fname) {
		if(!file.exists(i)) next
		mtime=file.info(i)$mtime  # date & time file was modified/made
		if (global) cat(paste("     ",i,"\n",sep=""))
		ii=substring(i,nchar(prefix)+2)
		contents=dat=readLines(i); ncont=length(contents)
		ff=findFormat(contents)
		if (ff=="P") dat=PBSmodelling:::.readList.P(i)
		else if (ff=="D" || ff=="R") dat=eval(parse(i))
		else if (any(ii==c("cor","std","mc.dat"))) { # treat these as a data frames
			tcont=gsub("std dev","std",contents)
			if (ii=="cor") tcont[2]=paste("index name value std ",paste(1:(ncont-2),collapse=" "))
			tfile=paste(prefix,".tmp",sep="")
			writeLines(tcont,tfile)
			skip=ifelse(any(ii==c("cor")),1,0)
			header=ifelse(any(ii==c("cor","std")),TRUE,FALSE)
			fill=ifelse(any(ii==c("cor","std")),TRUE,FALSE)
			dat=read.table(tfile,skip=skip,header=header,fill=fill)
			#---look for MCMC names---
			if (ii=="mc.dat") { 
				report=NULL
				report=try(readList(paste(prefix,".rep",sep="")),silent=TRUE)
				if (!is.null(report) && any(names(report)=="mcest")) {
					if (length(report$mcest)==ncol(dat))
						dat=rbind(report$mcest,dat) }
				if (!is.null(report) && any(names(report)=="mcnames")) {
					if (length(report$mcnames)==ncol(dat))
						names(dat)=report$mcnames }
			}
			#---correlation matrix manipulation---
			if (ii=="cor") { 
				ldh=as.numeric(substring(tcont[1],regexpr("=",tcont[1])+1)) # log determinant of hessian
				name=NULL; NAME=dat[,2]; value=dat[,3]; std=dat[,4]
				names(NAME)=1:length(NAME) # to keep the original order after split and sapply
				ldupe=split(NAME,NAME)
				ndupe=sapply(ldupe,function(x){
					if(length(x)>1) {
						xnam=names(x); len=length(x); npad=ifelse(any(len==10^(0:10)),1,0)
						npad=ceiling(log10(len))+npad
						x=paste(x,pad0(1:len,npad),sep=""); names(x)=xnam }
						return(x) },simplify=FALSE)
				for (j in names(ndupe)) name=c(name,ndupe[[j]])
				name=name[order(as.numeric(names(name)))]
				dat=dat[,setdiff(names(dat),c("index","name","value","std"))]
				names(dat)=name; row.names(dat)=name
				dat[is.na(dat)]=t(dat)[is.na(dat)]
				attr(dat,"determinant")=ldh; attr(dat,"value")=value; attr(dat,"std")=std
			} #---end correlation manipulation---
		}
		if (ii!="mc.dat") attr(dat,"contents")=contents
		attr(dat,"mtime")=mtime
		expr=paste("if (",global,") assign(\"",i,"\",dat,envir=.GlobalEnv); flist$",i,"=dat",sep="") 
		eval(parse(text=expr))
	}
	if (length(flist)==1) flist=flist[[1]] # return a single object, not a list of objects
	options(stringsAsFactors=sAF)
	invisible(flist) }
#------------------------------------------readRep

.win.readRep=function(winName="PBSadmb") {
	getWinVal(scope="L",winName=winName); act=getWinAct()[1]
	if (sum(toView)>0) {
		suffix=paste(".",names(toView)[toView],sep="")
		readRep(prefix=prefix,suffix=suffix,global=TRUE) }
	if (pltView) {
		pref=.findPrefix(".plt")
		if (length(pref)>0){
			for (i in pref) readRep(prefix=i,suffix=".plt",global=TRUE) } }
	invisible() }

#plotMC---------------------------------2009-02-06
# Plots the MCMC output in various ways
#-----------------------------------------------RH
plotMC=function(prefix,act="pairs",pthin=1,useCols=NULL){
	if (is.null(prefix) || prefix=="") return()
	inFile=paste(prefix, ".mc.dat", sep="")
	if(!file.exists(inFile) || file.info(inFile)$size==0){
		showAlert(paste("Cannot find file", inFile, "in working directory.")); return() }
	if (!exists(inFile,envir=.GlobalEnv) || is.null(attributes(get(inFile))$mtime) ||
			file.info(inFile)$mtime!=attributes(get(inFile))$mtime)
		inData=readRep(prefix,".mc.dat")
	else inData=get(inFile)
	if (nrow(inData)==1) {
		showAlert(paste("No MCMC data for '",prefix,
			"'\n\nSpecify an 'mceval_phase()' in your 'tpl' file.",sep="")); return() }
	x=1:nrow(inData); pthin=max(pthin,1); xthin=seq(1,nrow(inData),pthin)
	if (pthin>1) inData=inData[xthin,]
	if (!is.null(useCols)) inData=inData[,useCols]
	nc=ncol(inData); puce="#cc8899"
	clrs=c("blue", "red", "green", "magenta","navy",puce)
	clrs=rep(clrs,nc)[1:nc]
	resetGraph()
	
	panel=function(x,y) {
		len=length(x);
		points(x[2:len],y[2:len],pch=21,col="grey",bg="gainsboro",cex=0.8)
		points(x[1],y[1],pch=21,bg=puce,cex=1.2)
	}
	if (act=="pairs") pairs(inData, panel=panel,gap=0)
	if (act=="eggs") plotFriedEggs(inData)
	if (act=="acf") {
		expandGraph(mfrow=c(1,1),mar=c(3,3.5,.5,.5))
		plotACF(inData, clrs=rep(c("blue","red"),nc)[1:nc],lwd=ifelse(nc>6,1,2))
		mtext("Correlation",side=2,line=2.25,cex=1.2); mtext("Lags",side=1,line=1.5,cex=1) }
	if (act=="trace") {
		inData=cbind(x=xthin,inData)
		expandGraph(mfrow=c(nc,1),mar=c(0,0,0,0),oma=c(4,4.5,.5,.5))
		for (i in 1:nc) {
			plotTrace(inData[,c(1,i+1)],clrs=clrs[i],xaxt="n")
			axis(1,labels=par()$mfg[1]==par()$mfg[3])
			mtext(names(inData)[i+1],side=2,line=3,cex=1) }
		mtext("Sequential chain values",side=1,outer=TRUE,line=2,cex=1) }
	if (act=="dens") {
		rc=PBSmodelling:::.findSquare(nc)
		#sqn=sqrt(nc); m=ceiling(sqn); n=ceiling(nc/m)
		expandGraph(mfrow=c(rc[1],rc[2]),mar=c(2,2,0,0),oma=c(1,1.75,.5,.5),mgp=c(1.5,.2,0))
		for (i in 1:nc) {
			plotDens(inData[,i],clrs=clrs[i]) 
			addLabel(0.95,0.95,names(inData)[i],adj=c(1,1),cex=1.2) }
		#normData=sweep(inData,2,apply(inData,2,calcGM),"/")
		mtext("Kernel Density",outer=TRUE,side=2,line=0.2,cex=1.2) 
		mtext("Parameter estimates",outer=TRUE,side=1,line=-0.5,cex=1)
	}
	invisible() }
#-------------------------------------------plotMC

.win.plotMC=function(winName="PBSadmb") {
	getWinVal(scope="L",winName=winName)
	if (is.null(prefix) || prefix=="") return()
	act=getWinAct()[1]
	if (is.null(act)) act="pairs"
	plotMC(prefix=prefix,act=act,pthin=pthin,useCols=PBSadmb$useCols) 
	invisible() }

.win.viewCode=function(winName="PBSadmb",pkg="PBSadmbwin"){
	getWinVal(scope="L",winName=winName)
	tdir <- tempdir(); tdir <- gsub("\\\\","/",tdir)  # temporary directory for R
	pkgN=match(paste("package:",pkg,sep=""),search()) # position number of package
	pkgO=ls(pos=pkgN) # package objects
	z=sapply(pkgO,function(x){f=get(x);is.function(f)}); pkgF=names(z)[z] # package functions
	code=c("# PBSadmb Functions","#------------------")
	for (i in pkgF) {
		expr=paste("fun=deparse(",pkg,"::",i,"); fun[1]=paste(\"",i,"\",fun[1],sep=\" = \",collapse=\"\"); code=c(code,fun)",sep="")
		eval(parse(text=expr)) }
	fname=paste(tdir,"/",pkg,".r",sep="")
	writeLines(code, fname); editADfile(fname)
	invisible() }

.asIs=function(x) {
	if (is.numeric(x)) x=format(x,scientific=FALSE)
	return(x) }

#TODO how to make this portable?
.makeREbat=function(opts=.ADopts) {
	isOK=checkADopts(opts); if (!isOK) return()
	unpackList(opts)
	cmd=c("SETLOCAL",
		paste("set ADMB_HOME=",admpath,sep=""),
		"set ADMB_PATH=%ADMB_HOME%/bin",
		"set Path=.;%ADMB_PATH%;%Path%",
		"\"%ADMB_HOME%/bin/tpl2rem.exe\" %1")
	doscmd=gsub("/","\\\\",cmd)
	writeLines(doscmd,"re.bat") }

#copyFiles------------------------------2009-02-04
# Copy files with specified prefixes and suffixes 
# from one location to another.
#-----------------------------------------------RH
copyFiles=function(prefix,suffix=NULL,dir0=getwd(),dir1=getwd(),ask=TRUE){
	if (missing(prefix)) return()
	if (is.null(prefix) || prefix=="*") prefix=""
	if (is.null(suffix) || suffix=="*") suffix=""
	prefix=gsub("\\.","\\\\\\.",prefix)
	suffix=gsub("\\.","\\\\\\.",suffix)
	npref=length(prefix); nsuff=length(suffix)
	fpatt=paste(rep(prefix,each=nsuff),rep(suffix,npref),sep="")
	fname=sapply(fpatt,function(x,dir){list.files(dir,x,ignore.case=TRUE)},dir=dir0,simplify=FALSE)
	fname=unique(unlist(fname,use.names=FALSE)); nfile=length(fname)
	fname=list.files(dir0,pattern=fpatt,ignore.case=TRUE)
	if (nfile==0) return()
	copy.out=rep(FALSE,nfile); names(copy.out)=fname
	for (i in 1:nfile){
		fnam0=paste(dir0,"/",fname[i],sep="")
		fnam1=paste(dir1,"/",fname[i],sep="")
		if (!file.exists(fnam1)) {ask=FALSE; ovr=TRUE}
		if (ask) ovr=getYes(paste("Overwrite",fname[i],"?"))
		copy.out[i]=file.copy(fnam0,dir1,overwrite=ovr) }
	if (exists(".PBSmod",envir=.GlobalEnv) && .PBSmod$.activeWin=="PBSadmb") 
		setWinVal(list(prefix=substring(prefix,1,nchar(prefix)-2)),winName="PBSadmb")
	invisible(copy.out) }
#----------------------------------------copyFiles

.findPrefix=function(suffix){ # to replace Anisa's findPrefix
	spat=gsub("\\.","\\\\\\.",suffix)
	sfiles=list.files(pattern=paste(spat,"$",sep=""),ignore.case=TRUE)
	pref=substring(sfiles,1,nchar(sfiles)-nchar(suffix))
	return(pref) }

.chooseCols=function(winName="PBSadmb") {
	getWinVal(scope="L",winName=winName)
	if (is.null(prefix) || prefix=="") return()
	inFile=paste(prefix, ".mc.dat", sep="")
	if(!file.exists(inFile)){
		showAlert(paste("Cannot find file", inFile, "in working directory.")); return() }
	if (!exists(inFile,envir=.GlobalEnv) || is.null(attributes(get(inFile))$mtime) ||
			file.info(inFile)$mtime!=attributes(get(inFile))$mtime)
		inData=readRep(prefix,".mc.dat")
	else inData=get(inFile)
	flds=names(inData); nc=length(flds); rc=PBSmodelling:::.findSquare(nc)
	assign("PBSadmb",PBSadmb); useCols=PBSadmb$useCols
	if (is.null(useCols) || length(useCols)!=nc) useCols=rep(TRUE,nc)
	winDesc = c("window name=chooseCols title=Choose",
		"grid 3 1",
		"label text=\"Choose fields to plot:\"",
		paste("vector mode=logical vertical=T length=\"",nc,"\" names=useCols values=\"",
			paste(useCols,collapse=" "),"\" labels=\"",paste(flds,collapse=" "),"\"",sep=""),
		paste("button text=OK bg=skyblue function=doAction action=\"",
			"PBSadmb$useCols=getWinVal(winName=`chooseCols`)$useCols; ",
			"assign(`PBSadmb`,PBSadmb,envir=.GlobalEnv); closeWin(`chooseCols`)\"",sep=""))
	createWin(winDesc, astext = TRUE) }

.addQuotes=function(str){
	return(paste("\"",str,"\"",sep="")) }

#cleanAD--------------------------------2009-07-22
# Clean files with given prefix.
#-------------------------------------------JTS/RH
cleanAD <- function(prefix=NULL) {
	ofile=findPat(c("^admodel\\.","^eigv\\.","^sims$","^variance$","^tmp_","^fmin\\.log$",
		"^tfile$","\\.tmp$","\\.bak$","^hessian\\.bin$","^hesscheck$","^diags$","^dgs2$"),
		list.files(all.files=TRUE,ignore.case=TRUE))
	if (is.null(prefix)) {
		tpl=.findPrefix(".tpl")
		apat=paste("^",tpl,"\\.",sep="") # all tpls
		bpat=paste("\\.",c("tpl","dat","pin","r","pdf","mc.dat"),"$",sep="") # keep files
		afile=sapply(apat,function(x){list.files(pattern=x,ignore.case=TRUE)},simplify=FALSE)
		afile=as.vector(unlist(afile))
		bfile=sapply(bpat,function(x){list.files(pattern=x,ignore.case=TRUE)},simplify=FALSE)
		bfile=as.vector(unlist(bfile))
		pfile=setdiff(afile,bfile)
		ofile=union(ofile,pfile)  # remove duplicated names
		if (length(ofile)==0) ofile=NULL
		isOK=.cleanWD(files=ofile)
		if (!isOK) closeWin("cleanWindow") 
	}
	else {
		pfile=list.files(pattern=paste("^",prefix,"\\.",sep=""),ignore.case=TRUE)
		if (length(pfile)==0) psuff=NULL
		else {
			psuff=substring(pfile,nchar(prefix)+1)
			psuff=setdiff(psuff,c(".tpl",".dat",".pin",".r",".pdf",".mc.dat")) }
		ofile=setdiff(ofile,paste(prefix,psuff,sep="")) # remove duplicated names
		if (length(ofile)==0) ofile=NULL
		isOK=.cleanUp(prefix, suffix=psuff, files=ofile)
		if (!isOK) closeWin("cleanWindow") 
	} }
#------------------------------------------cleanAD

.win.cleanAD=function(winName="PBSadmb") {
	getWinVal(scope="L",winName=winName)
	cleanAD(prefix=prefix) 
	invisible() }

#.cleanUp-------------------------------2009-02-11
# Anisa's cleanProj function modified for flexibility.
#-----------------------------------------------RH
.cleanUp=function(prefix, suffix, files) {
	if (missing(suffix)) suffix = character(0)
	if (missing(files))  files  = character(0)
	rowLen = ceiling(sqrt(max(length(suffix), length(files))))
	if (rowLen == 0) return(invisible(FALSE))
	winDesc = c("window name=cleanWindow title=Clean",
		"grid 1 3",
		paste("entry name=cleanPrefix value=\"", prefix, "\" label=Prefix ",
			"mode=character width=12 font=\"bold 9\"", sep = ""),
		"button text=\">\" function=.win.findClean",
		"button text=refresh function=.cleanUpAgain",
		"label text=\"\n\nSuffixes to Clean\" font=\"bold 9\"", 
		PBSmodelling:::.makeCleanVec("suff", suffix, rowLen), 
		"label text=\"\n\nFiles to Clean\" font=\"bold 9\"", 
		PBSmodelling:::.makeCleanVec("file", files, rowLen), 
		"grid 1 3 relief=groove padx=4 pady=4", 
		"button function=.selectCleanBoxes action=1 text=\"Select All\" padx=4 pady=4", 
		"button function=.selectCleanBoxes action=0 text=\"Deselect All\" padx=4 pady=4", 
		"button function=.doClean text=Clean bg=aliceblue padx=4 pady=4")
	createWin(winDesc, astext = TRUE) 
	invisible(TRUE) }
.cleanUpAgain=function(winName="cleanWindow"){
	cleanAD(getWinVal(winName=winName)$cleanPrefix); invisible() }
.win.findClean=function(winName="cleanWindow"){
	choice=.findPrefix(".tpl") 
	chooseWinVal(choice,"cleanPrefix",winname=winName) 
	invisible() }

#.cleanWD-------------------------------2009-02-11
# Clean all potential grabage files.
#-----------------------------------------------RH
.cleanWD=function(files){ # Clean all nuisance files
	rowLen = ceiling(sqrt(length(files)))
	if (rowLen == 0) {
		try(closeWin("cleanWD"),silent=TRUE); return(invisible(FALSE)) }
	winDesc = c("window name=cleanWD title=Clean",
		"label text=\"\n\nFiles to Clean\" font=\"bold 9\"",
		PBSmodelling:::.makeCleanVec("file", files, rowLen),
		"grid 1 3 relief=groove padx=4 pady=4", 
		"button function=.selectCleanBoxes action=1 text=\"Select All\" padx=4 pady=4", 
		"button function=.selectCleanBoxes action=0 text=\"Deselect All\" padx=4 pady=4", 
		"button function=doAction text=Clean bg=aliceblue padx=4 pady=4 action=\".doCleanWD(); closeWin(`cleanWD`)\"")
	createWin(winDesc, astext = TRUE) 
	invisible(TRUE) }

#.doCleanWD-----------------------------2009-02-11
# Anisa's .doClean function modified for file names only
#-----------------------------------------------RH
.doCleanWD=function () { 
	vec=getWinVal(scope="L")
	vecList=logical()
	for (i in names(vec)) vecList=c(vecList,vec[[i]])
	filenames = names(vecList)[vecList]
	if (!length(filenames)) 
		showAlert("No files to delete.")
	else if (getYes(paste("Delete ", paste(filenames, collapse = ", "), "?", sep = ""))) 
		file.remove(filenames)
	remaining = file.exists(filenames)
	if (sum(remaining)) 
		showAlert(paste("Failed to delete", paste(filenames[remaining], collapse = ", "))) }

#.win.viewCode--------------------------2009-02-13
# View the package R code on the fly.
#-----------------------------------------------RH
.win.viewCode=function(pkg="PBSadmbwin"){
	eval(parse(text=paste("if(!require(",pkg,",quietly=TRUE)) stop(\"",pkg," package is required\")",sep="")))
	tdir <- tempdir(); tdir <- gsub("\\\\","/",tdir)                      # temporary directory for R
	pkgO=ls(paste("package:",pkg,sep=""),all=TRUE)                        # package objects
	z=sapply(pkgO,function(x){f=get(x);is.function(f)}); pkgF=names(z)[z] # package functions
	bad=regexpr("[\\|()[{^$*+?<-]",pkgF); pkgF=pkgF[bad<0]                # get rid of weird names
	if (length(pkgF)==0) {showAlert(paste(pkg,"has no recognizable functions")); return()}
	dots=regexpr("^\\.",pkgF); pkgF0=pkgF[dots==1]; pkgF1=pkgF[dots!=1]
	code=c(paste("#",pkg,"Functions"),paste("#",paste(rep("-",nchar(pkg)+10),collapse="")))
	for (i in c(pkgF1,pkgF0)) {
		expr=paste("fun=deparse(",pkg,"::",i,"); fun[1]=paste(\"",i,"\",fun[1],sep=\" = \",collapse=\"\"); code=c(code,fun)",sep="")
		eval(parse(text=expr)) }
	fname=paste(tdir,"/",pkg,".r",sep="")
	writeLines(code, fname); editADfile(fname)
	invisible() }
#------------------------------------.win.viewCode

#parseCmd-------------------------------2009-08-11
# Parse a command for an ADMB command.
#-----------------------------------------------RH
parseCmd = function(prefix, index, os=.Platform$OS, comp="GCC", admpath="", gccpath="") {
	data(ADMBcmd)
	dat=ADMBcmd; dat$OS=tolower(dat$OS)
	osdat = dat[dat$OS%in%os & dat$Comp%in%comp,]
	if(nrow(osdat)==0) stop("No records for specified OS and compiler")
	idat  = osdat[osdat$Index%in%index,]
	if(nrow(idat)==0) stop("No records for specified index")
	cmd=idat$Command[1]
	cmd=gsub("`","\"",cmd)
	cmd=gsub("@prefix",prefix,cmd)
	cmd=gsub("@adHome",admpath,cmd)
	cmd=gsub("@ccPath",gccpath,cmd)
	return(cmd) } 


