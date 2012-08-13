arquipDbox<-function () 
{
require(EcoVirtual)
initializeDialog(title = gettextRcmdr("The Archipelago"))
#### Salva dados
dsname <- tclVar("Do_Not_Save")
entryDsname <- tkentry(top, width="20", textvariable=dsname)
####
tmaxVar <- tclVar("100")
tmaxEntry <- tkentry(top, width = "4", textvariable = tmaxVar)
nIslVar <- tclVar("10")
nIslEntry <- tkentry(top, width = "3", textvariable = nIslVar)
ctVar <- tclVar("100")
ctEntry <- tkentry(top, width = "4", textvariable = ctlVar)
ar.minVar <- tclVar("10")
ar.maxVar <- tclVar("100")
ar.minEntry <- tkscale(top, from=10, to=50, showvalue=TRUE, variable=ar.minVar, resolution=1, orient="horizontal")
ar.maxEntry <- tkscale(top, from=50, to=100, showvalue=TRUE, variable=ar.maxVar, resolution=1, orient="horizontal")
rqVar <- tclVar("10")
fsp1Var <- tclVar("0.00")
####
command=paste(paste("gr.abund(rq = ", as.numeric(tclvalue(rqVar)), ", fsp1 = ", as.numeric(tclvalue(fsp1Var)), ",add=FALSE)"))
doItAndPrint(command)
########################
	set.gr.abund=function(...)
	{
	#command <- paste("matrix(c(", paste(counts, collapse=","), "), ", nrows, ", ", nrows,", byrow=TRUE)", sep="") 
	#gr.toff=function(riq, fsp1,pe,add=FALSE,...)
	command=paste("gr.abund(rq = ", as.numeric(tclvalue(rqVar)), ", fsp1 = ", as.numeric(tclvalue(fsp1Var)),  ",add=TRUE)")
	doItAndPrint(command)
	}
rqEntry <- tkscale(top, from=2, to=100, showvalue=TRUE, variable=rqVar, resolution=1, orient="horizontal", command=set.gr.abund)
fsp1Entry <-tkscale(top, from=0, to=1, showvalue=TRUE, variable=fsp1Var, resolution=0.01, orient="horizontal",command=set.gr.abund)
cantoVar <- tclVar("1")
cantoBox <- tkcheckbutton(top, variable = cantoVar)
################ passando as variáveis
onOK <- function() 
	{
	command="dev.off(dev.cur()); x11()"
	doItAndPrint(command)
	closeDialog()
	tmax=as.numeric(tclvalue(tmaxVar))
	nIsl=as.numeric(tclvalue(nIslVar))
	ct=as.numeric(tclvalue(ctVar))
	ar.min=as.numeric(tclvalue(ar.minVar))
	ar.max=as.numeric(tclvalue(ar.maxVar))
		if (ar.min>ar.max)
		{
		errorCondition("Maximum area < minimum area ")
		return()
		}
	rq <- as.numeric(tclvalue(rqVar))
	fsp1 <- as.numeric(tclvalue(fsp1Var))
	rank <- 1:rq
	abund <- fsp1*(1-fsp1)^(rank-1)
	cantoVF <- as.logical(as.numeric(tclvalue(cantoVar)))
   dsnameValue <- trim.blanks(tclvalue(dsname))
        if (dsnameValue == "Do_Not_Save" | dsnameValue=="") 
        {
        	command <- paste("arquip(nIsl= ", nIsl, ",ar.min= ", ar.min,",ar.max= ",ar.max, ",Nspp= ",rq, ", chuva.total = ", ct,", abund = ", abund,", tmax = ", tmax,", anima = ", cantoVF,")" , sep = "")
        }
        else  
		  {
		  command <- paste(dsnameValue, "<- arquip(nIsl= ", nIsl, ",ar.min= ", ar.min,",ar.max= ",ar.max, ",Nspp= ",rq, ", chuva.total = ", ct,", abund = ", abund,", tmax = ", tmax,", anima = ", cantoVF,")" , sep = "")
		  }
	doItAndPrint(command)
	tkfocus(CommanderWindow())
	}
############ 
OKCancelHelp(helpSubject = "com.compete")
tkgrid(tklabel(top, text="Enter name for data set:"), entryDsname, sticky="e")
tkgrid(tklabel(top, text = "Maximum time"), tmaxEntry, sticky = "e")
###########
tkgrid(tklabel(top, text="Archipelago Conditions :", fg="blue"), sticky="w")
tkgrid(tklabel(top, text = "Number of Islands"), nIslEntry, sticky = "e")
tkgrid(tklabel(top, text = "Smaller Island (square side)"), ar.minEntry, sticky = "e")
tkgrid(tklabel(top, text = "Bigger Island (square side)"), ar.maxEntry, sticky = "e")
#########
tkgrid(tklabel(top, text="Mainland Parameters :", fg="blue"), sticky="w")
tkgrid(tklabel(top, text = "Number of species "), rqEntry, sticky = "se")
tkgrid(tklabel(top, text = "Abundance eveness "), fsp1Entry, sticky = "se")
tkgrid(tklabel(top, text = "Seed rain size "), ctEntry, sticky = "se")
tkgrid(tklabel(top, text = "Show simulation frames"), cantoBox, sticky = "e")
#
tkgrid(buttonsFrame, sticky = "w", columnspan = 2)
tkgrid.configure(entryDsname, sticky = "w")
tkgrid.configure(tmaxEntry, sticky = "w")
tkgrid.configure(nIslEntry, sticky = "w")
tkgrid.configure(ar.minEntry, sticky = "w")
tkgrid.configure(ar.maxEntry, sticky = "w")

#
tkgrid.configure(rqEntry, sticky = "w")
tkgrid.configure(fsp1Entry, sticky = "w")
tkgrid.configure(ctEntry, sticky = "w")
#
tkgrid.configure(cantoBox, sticky = "w")
dialogSuffix(rows = 10, columns = 2, focus = tmaxEntry)
}
######################################
ColExtDbox<-function () 
{
#animaColExt(minimo=0.01, maximo=1, interv=0.01, Ext="crs", Col="dcr")
require(EcoVirtual)
initializeDialog(title = gettextRcmdr("Colonization/Extinction Balance"))
#### 
#radioButtons(top, "col", buttons=c("crs", "dcr","fix" ), 
#		labels=gettextRcmdr(c("Increase", "Decrease", "Fixed")), title=gettextRcmdr("Colonization"))
#radioButtons(top, "ext", buttons=c("crs", "dcr","fix" ), 
#		labels=gettextRcmdr(c("Increase", "Decrease", "Fixed")), title=gettextRcmdr("Colonization"))
ColVar <- tclVar("crs")
    ColCrsButton <- ttkradiobutton(top, variable=ColVar, value="crs")
    ColDcrButton <- ttkradiobutton(top, variable=ColVar, value="dcr")
    ColFixButton <- ttkradiobutton(top, variable=ColVar, value="fix")
ExtVar <- tclVar("fix")
    ExtCrsButton <- ttkradiobutton(top, variable=ExtVar, value="crs")
    ExtDcrButton <- ttkradiobutton(top, variable=ExtVar, value="dcr")
    ExtFixButton <- ttkradiobutton(top, variable=ExtVar, value="fix")
onOK <- function(){
        closeDialog()
         Col <- tclvalue(ColVar)
#			Col <- if (col == "crs") "crs"
#			else if (col == "dcr") "dcr"
#			else  "fix"
#			
			Ext <- tclvalue(ExtVar)
#			Ext <- if (ext == "crs") "crs"
#			else if (ext == "dcr") "dcr"
#			else  "fix"
#        
        doItAndPrint(paste("animaColExt(Ext= '", Ext,  "', Col ='", Col,"')", sep=""))
        }
OKCancelHelp(helpSubject = "com.compete")
###########
tkgrid(tklabel(top, text="Colonization  :", fg="blue"), sticky="w")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Increase")), ColCrsButton, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Decrease")), ColDcrButton, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Stable")), ColFixButton, sticky="e")
tkgrid(tklabel(top, text="Extinction  :", fg="blue"), sticky="w")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Increase")), ExtCrsButton, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Decrease")), ExtDcrButton, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Stable")), ExtFixButton, sticky="e")    
################################################################
tkgrid(buttonsFrame, sticky="w", columnspan=2)
    tkgrid.configure(ColCrsButton, sticky="w")
    tkgrid.configure(ColDcrButton, sticky="w")
    tkgrid.configure(ColFixButton, sticky="w")
    tkgrid.configure(ExtCrsButton, sticky="w")
    tkgrid.configure(ExtDcrButton, sticky="w")
    tkgrid.configure(ExtFixButton, sticky="w")
    dialogSuffix(rows=9, columns=2, focus=ColCrsButton)
}
#########################################
#bioGeoIsl=function(areas , dist , P , peso.A=.5 , a=1, b=-.01, c=1, d=-.01, e=0, f=.01, g=0, h=.01)
bioGeoIslDbox<- function()
{
#    Library("abind")
#bioGeoIsl(areas , dist , P , peso.A=.5 , b=-.01,  d=-.01,  f=.01, h=.01)
    env <- environment()
    initializeDialog(title=gettextRcmdr("Island Biogeography Model"))
    dsname <- tclVar("Do_Not_Save")
    ## incluido
	entryDsname <- tkentry(top, width="20", textvariable=dsname)
	PVar <- tclVar("100")
	PEntry <- tkentry(top, width = "4", textvariable = PVar)
	ncolsVar<-tclVar("2")
	bVar <- tclVar("-0.01")
	bEntry <- tkscale(top, from= -1, to=0, showvalue=TRUE, variable=bVar, resolution=0.01, orient="horizontal")
	dVar <- tclVar("-0.01")
	dEntry <- tkscale(top, from= -1, to=0, showvalue=TRUE, variable=dVar, resolution=0.01, orient="horizontal")
	fVar <- tclVar("0.01")
	fEntry <- tkscale(top, from= 0, to=1, showvalue=TRUE, variable=fVar, resolution=0.01, orient="horizontal")
	hVar <- tclVar("0.01")
	hEntry <- tkscale(top, from= 0, to=1, showvalue=TRUE, variable=hVar, resolution=0.01, orient="horizontal")
	peso.AVar <- tclVar("0.5")
	peso.AEntry <- tkscale(top, from= 0, to=1, showvalue=TRUE, variable=peso.AVar, resolution=0.01, orient="horizontal")
######
    outerTableFrame <- tkframe(top)
    assign(".tableFrame", tkframe(outerTableFrame), envir=env)
##################################
    	setUpTable <- function(...)
    	{
        tkdestroy(get(".tableFrame", envir=env))
        assign(".tableFrame", tkframe(outerTableFrame), envir=env)
        nrows <- as.numeric(tclvalue(rowsValue))
        ncols <- as.numeric(tclvalue(ncolsVar)) 
        make.col.names <- "labelRcmdr(.tableFrame, text='')"
        assign(".colname.1", tclVar("Distance"), envir=env)
        assign(".colname.2", tclVar("Size"), envir=env)
        for (j in 1:ncols) {
            col.varname <- paste(".colname.", j, sep="")
            make.col.names <- paste(make.col.names, ", ", "ttkentry(.tableFrame, width='8', textvariable= ",  col.varname, ")", sep="")
            }
        eval(parse(text=paste("tkgrid(", make.col.names, ")", sep="")), envir=env)
        for (i in 1:nrows)
        {
            varname <- paste(".tab.", i, ".1", sep="")
            assign(varname, tclVar("") , envir=env)
            row.varname <- paste(".rowname.", i, sep="")
            assign(row.varname, tclVar(paste("Island",i, sep="_")), envir=env) ## row names show (first table) 
            make.row <- paste("ttkentry(.tableFrame, width='8', textvariable=",
                row.varname, ")", sep="")
            make.row <- paste(make.row, ", ", "ttkentry(.tableFrame, width='8', textvariable=",
                varname, ")", sep="")
            for (j in 2:ncols){
                varname <- paste(".tab.", i, ".", j, sep="")
                assign(varname, tclVar(""), envir=env)
                make.row <- paste(make.row, ", ", "ttkentry(.tableFrame, width='8', textvariable=", varname, ")", sep="")
                }
            eval(parse(text=paste("tkgrid(", make.row, ")", sep="")), envir=env)
          }
        tkgrid(get(".tableFrame", envir=env), sticky="w")
		}
############## entry vector: time zero
#####################
    rowColFrame <- tkframe(top)
    rowsValue <- tclVar("2")
    rowsSlider <- tkscale(rowColFrame, from=2, to=10, showvalue=FALSE, variable=rowsValue,resolution=1, orient="horizontal", command=setUpTable)
    rowsShow <- labelRcmdr(rowColFrame, textvariable=rowsValue, width=2, justify="right")
    #colsValue <- tclVar("2")
    #colsSlider <- tkscale(rowColFrame, from=2, to=10, showvalue=FALSE, variable=colsValue,
 #       resolution=1, orient="horizontal", command=setUpTable)
    #colsShow <- labelRcmdr(rowColFrame, textvariable=colsValue, width=2, justify="right")
    onOK <- function()
    {
        PVar <- as.numeric(tclvalue(PVar))
        bVar <- as.numeric(tclvalue(bVar))
        dVar <- as.numeric(tclvalue(dVar))
        fVar <- as.numeric(tclvalue(fVar))
        hVar <- as.numeric(tclvalue(hVar))
        peso.AVar <- as.numeric(tclvalue(peso.AVar))
        nrows <- as.numeric(tclvalue(rowsValue))
        ncols <- as.numeric(tclvalue(ncolsVar))
      cell <- 0
      dist <- rep(0, nrows)
      size<- rep(0, nrows)
      row.names <- rep("", nrows)
      col.names <- rep("", nrows)
#### transition matrix
        for (i in 1:nrows) row.names[i] <-eval(parse(text=paste("tclvalue(", paste(".rowname.", i, sep=""),")", sep="")))
        for (j in 1:ncols) col.names[j] <-eval(parse(text=paste("tclvalue(", paste(".colname.", j, sep=""),")", sep="")))
####################################
            for (i in 1:nrows)
            {
                cell <- cell+1
                varname1 <- paste(".tab.", i, ".1", sep="")
                varname2 <- paste(".tab.", i, ".2", sep="")
                dist[cell] <- as.numeric(eval(parse(text=paste("tclvalue(", varname1,")", sep="")))) ## aqui ele guarda os valores das celulas
                size[cell] <- as.numeric(eval(parse(text=paste("tclvalue(", varname2,")", sep=""))))
				}
########################################################
#### 
command <- paste("c(", paste(size, collapse=","), ")", sep="") 
        assign(".size", justDoIt(command), envir=.GlobalEnv)
        logger(paste(".size <- ", command, sep=""))
        doItAndPrint(".size  # sizes")
command <- paste("c(", paste(dist, collapse=","), ")", sep="") 
        assign(".dist", justDoIt(command), envir=.GlobalEnv)
        logger(paste(".dist <- ", command, sep=""))
        doItAndPrint(".dist  # distances")           
#### ate aqui    				
##################################################				
################################# 
##bioGeoIsl(areas , dist , P , peso.A=.5 , b=-.01, d=-.01, f=.01, h=.01)
   dsnameValue <- trim.blanks(tclvalue(dsname))
        if (dsnameValue == "Do_Not_Save" | dsnameValue=="") 
        {
        	command <- paste("bioGeoIsl(areas=.size, dist  = .dist, P = ",PVar, ", b = ", bVar,", d = ", dVar,",f =", fVar,",h= ", hVar,")", sep = "")
        }
        else  
		  {
		  command <- paste(dsnameValue,"<- bioGeoIsl(areas=",size, ", dist  = ", dist, ", P = ",PVar, ", b = ", bVar,", d = ", dVar,",f =", fVar,",h= ", hVar,")", sep = "")
		  }
########
	doItAndPrint(command)
	tkfocus(CommanderWindow())
      }
    OKCancelHelp(helpSubject="multSp")
##############
tkgrid(tklabel(top, text="Enter name for simulation data set :", fg="blue"), sticky="w")
#    tkgrid(tklabel(top, text="\tEnter name for data set:"), entryDsname, sticky="e")
tkgrid(entryDsname,sticky="e" )
## incluido
tkgrid(tklabel(top, text="Models Parameters :", fg="blue"), sticky="w")
tkgrid(tklabel(top, text = "Mainland Number of Species"), PEntry, sticky = "w")
tkgrid(tklabel(top, text = "Extinction/Area coeficient "), bEntry, sticky = "w")
tkgrid(tklabel(top, text = "Extinction/Distance coeficient "), hEntry, sticky = "w")
tkgrid(tklabel(top, text = "Colonization/Area coeficient "), fEntry, sticky = "w")
tkgrid(tklabel(top, text = "Colonization/Distance coeficient "), dEntry, sticky = "w")
tkgrid(tklabel(top, text = "Ratio Area/Distance effect"), peso.AEntry, sticky = "w")
#tkgrid.configure(entryDsname, sticky = "w")
####    
tkgrid(labelRcmdr(top, text=gettextRcmdr("Island Size and Distance: "), fg="blue"), sticky="w")
    tkgrid(labelRcmdr(rowColFrame, text=gettextRcmdr("Number of Islands:")), rowsSlider, rowsShow, sticky="w")
   tkgrid(rowColFrame, sticky="w")
    tkgrid(outerTableFrame, sticky="e")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=10, columns=2)
}
#bioGeoIslDbox()
########################################
gr.abund=function(rq, fsp1, add=FALSE,...)
{
#	rq <- as.numeric(tclvalue(rqVar))
#	fsp1 <- as.numeric(tclvalue(fsp1Var))
#	pe <- as.numeric(tclvalue(peVar))
	rank=1:rq
	#ci= pe/(1-fsp1)^(2*rank-1)
	px= fsp1*(1-fsp1)^(rank-1)
		if(add==FALSE)
		{
		toff<-x11( width=5, height=5)
		}
	old<-par(mar=c(3,3,3,3))
	#plot(px~rank,col="red",ylim=c(0,max(ci)*1.1), type="b", ann=FALSE, axes=FALSE)
	plot(px~rank, ylim=c(0,fsp1),type="b", bty="n",  ann=FALSE, cex.axis=0.8)
	#axis(4, cex.axis=0.8)#, yaxp=c(0,3,3))
	par(new=TRUE)
	#yaxt="n", xaxp=c(0,10,5))
	#axis(2, cex.axis=0.8)#, yaxp=c(0,0.2,4))
	mtext("Specie abundance rank", 1, 2, cex=0.9)
	mtext("Abundance", 2, 2, cex=0.9)
	#mtext("Colonization rate", 4, 2, cex=0.9)
	mtext("Relative Species Abundance ", 3, 0, cex=1.2)
	par(old)
}

