#### Dialogs boxes for RcmdrPlugin.EcoVirtual package
### Alexandre Adalardo de Oliveira 17 fevereiro 2010
####################################################
.First.lib <- function(libname, pkgname){
    if (!interactive()) return()
    Rcmdr <- options()$Rcmdr
    plugins <- Rcmdr$plugins
    if ((!pkgname %in% plugins) && !getRcmdr("autoRestart")) {
        Rcmdr$plugins <- c(plugins, pkgname)
        options(Rcmdr=Rcmdr)
        closeCommander(ask=FALSE, ask.save=TRUE)
        Commander()
        }
    }
########################################################
estdemDb <-function () 
{
require(EcoVirtual)
initializeDialog(title = gettextRcmdr("Demographic Stochasticity"))
#### Salva dados
dsname <- tclVar("Do_Not_Save")
entryDsname <- tkentry(top, width="20", textvariable=dsname)
####
noVar <- tclVar("100")
noEntry <- tkentry(top, width = "4", textvariable = noVar)
bVar <- tclVar(0.55)
bEntry <- tkentry(top, width = "6", textvariable = bVar)
dVar <- tclVar(0.5)
dEntry <- tkentry(top, width = "6", textvariable = bVar)
tmaxVar <- tclVar(50)
tmaxEntry <- tkentry(top, width = "4", textvariable = tmaxVar)
	onOK <- function() 
	{
        closeDialog()
        N0 <- round(as.numeric(tclvalue(noVar)))
        if (is.na(N0) || N0 <= 0) 
        {
            errorCondition(message = "Number of individuos at the simulation start must be a positive integer")
            return()
        }
        tmax <- round(as.numeric(tclvalue(tmaxVar)))
        if (is.na(tmax) || tmax <= 0) 
        {
            errorCondition("Number of simulations must be a positive integer")
            return()
        }
        b=as.numeric(tclvalue(bVar))
        d=as.numeric(tclvalue(dVar))
############ Data name
#estDem(N0=100, b=0.55, d=0.5, tmax=50)
   dsnameValue <- trim.blanks(tclvalue(dsname))
        if (dsnameValue == "Do_Not_Save" | dsnameValue=="") 
        {
        	command <- paste("estDem(N0= ",N0, ", b = ", b,", d = ", d,", tmax =", tmax,")", sep = "")
        }
        else  
		  {
		  command <- paste(dsnameValue, " <- estDem(N0= ",N0, ", b = ", b,", d = ", d,", tmax =", tmax,")", sep = "")
		  }
	doItAndPrint(command)
	tkfocus(CommanderWindow())
	}
OKCancelHelp(helpSubject = "dynPop")
tkgrid(tklabel(top, text="Enter name for data set: "), entryDsname, sticky="e")
#tkgrid(tklabel(top, text="Simulation Conditions :  ", fg="blue"), sticky="w")
tkgrid(tklabel(top, text = "Number of simulations"), tmaxEntry, sticky = "e")
tkgrid(tklabel(top, text="Species parameters :", fg="blue"), sticky="w")
tkgrid(tklabel(top, text = "Initial population size  "), noEntry, sticky = "e")
tkgrid(tklabel(top, text = "Birth rate (b)  "), bEntry, sticky = "se")
tkgrid(tklabel(top, text = "Death rate (d)  "), dEntry, sticky = "e")
tkgrid(buttonsFrame, sticky = "w", columnspan = 2)
#tkgrid(tklabel(top, text="Enter name for data set:"), entryDsname, sticky="e")
tkgrid.configure(entryDsname, sticky = "w")
tkgrid.configure(tmaxEntry, sticky = "w")
tkgrid.configure(noEntry, sticky = "w")
tkgrid.configure(bEntry, sticky = "w")
tkgrid.configure(dEntry, sticky = "w")
dialogSuffix(rows = 6, columns = 2, focus = tmaxEntry)
}


#############################################################3
############################################################
#estExp(N0=1000,r=0.0488,varr=0.005,tmax=100) 
popExp <-function () 
{
require(EcoVirtual)
initializeDialog(title = gettextRcmdr("Exponential Growth"))
#### Salva dados
dsname <- tclVar("Do_Not_Save")
entryDsname <- tkentry(top, width="20", textvariable=dsname)
####
noVar <- tclVar("10")
noEntry <- tkentry(top, width = "4", textvariable = noVar)
rVar <- tclVar(0.05)
varrVar <- tclVar(0.02)
tmaxVar <- tclVar(100)
tmaxEntry <- tkentry(top, width = "4", textvariable = tmaxVar)
#################################################################
	set.gr=function(...)
	{
	command=paste("estExp(N0 = ", as.numeric(tclvalue(noVar)), ", r = ", as.numeric(tclvalue(rVar)), ", varr = ", as.numeric(tclvalue(varrVar)),", tmax = ", as.numeric(tclvalue(tmaxVar)), ")")
	doItAndPrint(command)
	tkfocus(CommanderWindow())
	}
##############################################################
rEntry<-tkscale(top, from=-5, to=5, showvalue=TRUE, variable=rVar, resolution=0.01, orient="horizontal", 
command=set.gr)
varrEntry <- tkscale(top, from=0, to=15, showvalue=TRUE, variable=varrVar, resolution=0.01, orient="horizontal", 
command=set.gr)
################################################
	onOK <- function() 
	{	
	command="dev.off(dev.cur()); x11()"
	doItAndPrint(command)
   closeDialog()
   N0 <- round(as.numeric(tclvalue(noVar)))
        if (is.na(N0) || N0 <= 0) 
        {
            errorCondition(message = "Number of individuos at the simulation start must be a positive integer")
            return()
        }
        tmax <- round(as.numeric(tclvalue(tmaxVar)))
        if (is.na(tmax) || tmax <= 0) 
        {
            errorCondition("Number of simulations must be a positive integer")
            return()
        }
        varr <- as.numeric(tclvalue(varrVar))
        if (varr < 0)
        {
            errorCondition(message = "r Variance must be zero (no stocatiscit) or positive value")
            return()
        }
        r=as.numeric(tclvalue(rVar))
############ Data name
   dsnameValue <- trim.blanks(tclvalue(dsname))
        if (dsnameValue == "Do_Not_Save" | dsnameValue=="") 
        {
        	command <- paste("estExp(N0= ",N0, ", r = ", r,", varr = ", varr,", tmax =", tmax,")", sep = "")
        }
        else  
		  {
		  command <- paste(dsnameValue, " <- estExp(N0= ",N0, ", r = ", r,", varr = ", varr,", tmax =", tmax,")", sep = "")
		  }
	doItAndPrint(command)
	tkfocus(CommanderWindow())
	}
OKCancelHelp(helpSubject = "dynPop")
tkgrid(tklabel(top, text="Enter name for last simulation data set: "), entryDsname, sticky="e")
#tkgrid(tklabel(top, text="Simulation Arena Conditions :  ", fg="blue"), sticky="w")
tkgrid(tklabel(top, text = "Maximum time"), tmaxEntry, sticky = "e")
tkgrid(tklabel(top, text="Species parameters :", fg="blue"), sticky="w")
tkgrid(tklabel(top, text = "Initial population size  "), noEntry, sticky = "e")
tkgrid(tklabel(top, text = "Intrinsic growth rate (r)  "), rEntry, sticky = "se")
tkgrid(tklabel(top, text="Environment stochasticity :", fg="blue"), sticky="w")
tkgrid(tklabel(top, text = "r variance  "), varrEntry, sticky = "e")
tkgrid(buttonsFrame, sticky = "w", columnspan = 2)
#tkgrid(tklabel(top, text="Enter name for data set:"), entryDsname, sticky="e")
tkgrid.configure(entryDsname, sticky = "w")
tkgrid.configure(tmaxEntry, sticky = "w")
tkgrid.configure(noEntry, sticky = "w")
tkgrid.configure(varrEntry, sticky = "w")
tkgrid.configure(rEntry, sticky = "w")
dialogSuffix(rows = 6, columns = 2, focus = tmaxEntry)
}


#########################################################
##########################################################
#crescLog(N0=10, r=0.05, K=80, tmax=100)
popLog<-function () 
{
require(EcoVirtual)
initializeDialog(title = gettextRcmdr("Logistic Growth"))
#### Salva dados
dsname <- tclVar("Do_Not_Save")
entryDsname <- tkentry(top, width="20", textvariable=dsname)
####
noVar <- tclVar("10")
noEntry <- tkentry(top, width = "4", textvariable = noVar)
rVar <- tclVar(0.05)
rEntry <-tkscale(top, from=-5, to=5, showvalue=TRUE, variable=rVar, resolution=0.01, orient="horizontal")
kVar <- tclVar("20")
kEntry <- tkentry(top, width = "4", textvariable = kVar)
tmaxVar <- tclVar(100)
tmaxEntry <- tkentry(top, width = "4", textvariable = tmaxVar)
	onOK <- function() 
	{
        closeDialog()
        N0 <- round(as.numeric(tclvalue(noVar)))
        if (is.na(N0) || N0 <= 0) 
        {
            errorCondition(message = "Number of individuals at the simulation start must be a positive integer")
            return()
        }
        tmax <- round(as.numeric(tclvalue(tmaxVar)))
        if (is.na(tmax) || tmax <= 0) 
        {
            errorCondition("Number of simulations must be a positive integer")
            return()
        }
        K <- as.numeric(tclvalue(kVar))
        if (is.na(K) || K <= 0)
        {
            errorCondition(message = "Carrying Capacity (K) must be a positive integer")
            return()
        }
        r=as.numeric(tclvalue(rVar))
############ Data name
   dsnameValue <- trim.blanks(tclvalue(dsname))
        if (dsnameValue == "Do_Not_Save" | dsnameValue=="") 
        {
        	command <- paste("crescLog(N0= ",N0, ", r = ", r,", K = ", K,", tmax =", tmax,")", sep = "")
        }
        else  
		  {
		  command <- paste(dsnameValue,"<-crescLog(N0= ",N0, ", r = ", r,", K = ", K,", tmax =", tmax,")", sep = "")
		  }
########
	doItAndPrint(command)
	tkfocus(CommanderWindow())
	}
OKCancelHelp(helpSubject = "dynPop")
tkgrid(tklabel(top, text="Enter name for data set:  "), entryDsname, sticky="e")
tkgrid(tklabel(top, text="Simulation Arena Conditions : ", fg="blue"), sticky="w")
tkgrid(tklabel(top, text = "Number of simulations  "), tmaxEntry, sticky = "e")
tkgrid(tklabel(top, text="Species parameters :", fg="blue"), sticky="w")
tkgrid(tklabel(top, text = "Initial population size  "), noEntry, sticky = "e")
tkgrid(tklabel(top, text = "Carrying capacity (K) "), kEntry, sticky = "e")
tkgrid(tklabel(top, text = "Intrinsic growth rate (r) "), rEntry, sticky = "se")
tkgrid(buttonsFrame, sticky = "w", columnspan = 2)
tkgrid.configure(entryDsname, sticky = "w")
tkgrid.configure(tmaxEntry, sticky = "w")
tkgrid.configure(noEntry, sticky = "w")
tkgrid.configure(kEntry, sticky = "w")
tkgrid.configure(rEntry, sticky = "w")
dialogSuffix(rows = 6, columns = 2, focus = tmaxEntry)
}

##########################################
##########################################
#compLV(n01=10, n02=10,r1=0.05, r2=0.03, k1=80, k2=50, alfa=1.2, beta=0.5, tmax=200)
compDb<-function () 
{
require(EcoVirtual)
initializeDialog(title = gettextRcmdr("Competition LV Model"))
#### Salva dados
dsname <- tclVar("Do_Not_Save")
entryDsname <- tkentry(top, width="20", textvariable=dsname)
####
n01Var <- tclVar("10")
n01Entry <- tkentry(top, width = "4", textvariable = n01Var)
n02Var <- tclVar("10")
n02Entry <- tkentry(top, width = "4", textvariable = n02Var)
r1Var <- tclVar(0.05)
r1Entry=tkscale(top, from=-5, to=5, showvalue=TRUE, variable=r1Var, resolution=0.01, orient="horizontal")
r2Var <- tclVar(0.05)
r2Entry=tkscale(top, from=-5, to=5, showvalue=TRUE, variable=r2Var, resolution=0.01, orient="horizontal")
k1Var <- tclVar("20")
k1Entry <- tkentry(top, width = "4", textvariable = k1Var)
k2Var <- tclVar("20")
k2Entry <- tkentry(top, width = "4", textvariable = k2Var)
alfaVar <- tclVar("1.2")
alfaEntry <- tkentry(top, width = "6", textvariable = alfaVar)
betaVar <- tclVar("0.5")
betaEntry <- tkentry(top, width = "6", textvariable = betaVar)
tmaxVar <- tclVar(100)
tmaxEntry <- tkentry(top, width = "4", textvariable = tmaxVar)
	onOK <- function() 
	{
        closeDialog()
        n01 <- round(as.numeric(tclvalue(n01Var)))
        if (is.na(n01) || n01 <= 0) 
        {
            errorCondition(message = "Number of individuals at the simulation start must be a positive integer")
            return()
        }
        n02 <- round(as.numeric(tclvalue(n02Var)))
        if (is.na(n02) || n02 <= 0) 
        {
            errorCondition(message = "Number of individuals at the simulation start must be a positive integer")
            return()
        }
        tmax <- round(as.numeric(tclvalue(tmaxVar)))
        if (is.na(tmax) || tmax <= 0) 
        {
            errorCondition("Number of simulations must be a positive integer")
            return()
        }
        k1 <- as.numeric(tclvalue(k1Var))
        if (is.na(k1) || k1 <= 0)
        {
            errorCondition(message = "Carrying Capacity (K) must be a positive integer")
            return()
        }
        k2 <- as.numeric(tclvalue(k2Var))
        if (is.na(k2) || k2 <= 0)
        {
            errorCondition(message = "Carrying Capacity (K) must be a positive integer")
            return()
        }
        r1=as.numeric(tclvalue(r1Var))
        r2=as.numeric(tclvalue(r2Var))
        alfa=as.numeric(tclvalue(alfaVar))
        beta=as.numeric(tclvalue(betaVar))
############ Data name
   dsnameValue <- trim.blanks(tclvalue(dsname))
        if (dsnameValue == "Do_Not_Save" | dsnameValue=="") 
        {
        	command <- paste("compLV(n01= ",n01, ",n02= ",n02, ", r1 = ", r1,", r2 = ", r2,", k1 = ", k1,", k2 = ", k2,", alfa = ", alfa,", beta = ", beta,", tmax =", tmax,")", sep = "")
        }
        else  
		  {
		  command <- paste("compLV(n01= ",n01, "n02= ",n02, ", r1 = ", r1,", r2 = ", r2,", k1 = ", k1,", k2 = ", k2,", alfa = ", alfa,", beta = ", beta,", tmax =", tmax,")", sep = "")
		  }
#compLV(n01=10, n02=10,r1=0.05, r2=0.03, k1=80, k2=50, alfa=1.2, beta=0.5, tmax=200)
########
	doItAndPrint(command)
	tkfocus(CommanderWindow())
	}
OKCancelHelp(helpSubject = "compLV")
tkgrid(tklabel(top, text="Enter name for data set:"), entryDsname, sticky="e")
tkgrid(tklabel(top, text = "Number of simulations  "), tmaxEntry, sticky = "e")
tkgrid(tklabel(top, text="Best competitor species  parameters : ", fg="blue"), sticky="w")
tkgrid(tklabel(top, text = "Initial population  "), n01Entry, sticky = "e")
tkgrid(tklabel(top, text = "Carrying capacity (K)  "), k1Entry, sticky = "e")
tkgrid(tklabel(top, text = "Intrinsic growth rate  "), r1Entry, sticky = "se")
tkgrid(tklabel(top, text = "Alfa coeficiente"), alfaEntry, sticky = "e")
tkgrid(tklabel(top, text="Worse Competitor Specie :", fg="blue"), sticky="w")
tkgrid(tklabel(top, text = "Initial population  "), n02Entry, sticky = "e")
tkgrid(tklabel(top, text = "Carrying capacity (K)  "), k2Entry, sticky = "e")
tkgrid(tklabel(top, text = "Intrinsic growth rate  "), r2Entry, sticky = "se")
tkgrid(tklabel(top, text = "Beta coeficiente"), betaEntry, sticky = "e")
tkgrid(buttonsFrame, sticky = "w", columnspan = 2)
tkgrid.configure(entryDsname, sticky = "w")
tkgrid.configure(tmaxEntry, sticky = "w")
tkgrid.configure(n01Entry, sticky = "w")
tkgrid.configure(n02Entry, sticky = "w")
tkgrid.configure(k1Entry, sticky = "w")
tkgrid.configure(k2Entry, sticky = "w")
tkgrid.configure(r1Entry, sticky = "w")
tkgrid.configure(r2Entry, sticky = "w")
tkgrid.configure(alfaEntry, sticky = "w")
tkgrid.configure(betaEntry, sticky = "w")
dialogSuffix(rows = 11, columns = 2, focus = tmaxEntry)
}


##########################################
######################################
popstrDb<-function () 
{
require(EcoVirtual)
initializeDialog(title = gettextRcmdr("Structured Population"))
#### Salva dados
dsname <- tclVar("Do_Not_Save")
entryDsname <- tkentry(top, width="20", textvariable=dsname)
####
tmaxVar <- tclVar("100")
tmaxEntry <- tkentry(top, width = "4", textvariable = tmaxVar)
clVar <- tclVar("20")
lnVar <- tclVar("20")
clEntry <- tkentry(top, width = "4", textvariable = clVar)
lnEntry <- tkentry(top, width = "4", textvariable = lnVar)
### controle de barra
sjVar <- tclVar(0.4)
sjEntry <- tkscale(top, from=0, to=1, showvalue=TRUE, variable=sjVar, resolution=0.01, orient="horizontal")
jjVar <- tclVar(0.4)
jjEntry <- tkscale(top, from=0, to=1, showvalue=TRUE, variable=jjVar, resolution=0.01, orient="horizontal")
jaVar <- tclVar(0.4)
jaEntry <- tkscale(top, from=0, to=1, showvalue=TRUE, variable=jaVar, resolution=0.01, orient="horizontal")
aaVar <- tclVar(0.4)
aaEntry <- tkscale(top, from=0, to=1, showvalue=TRUE, variable=aaVar, resolution=0.01, orient="horizontal")
#####
fecVar <- tclVar("1.2") 
fecEntry <- tkentry(top, width = "6", textvariable = fecVar)
nsVar <- tclVar("50") 
nsEntry <- tkentry(top, width = "6", textvariable = nsVar)
njVar <- tclVar("20") 
njEntry <- tkentry(top, width = "6", textvariable = njVar)
naVar <- tclVar("10") 
naEntry <- tkentry(top, width = "6", textvariable = naVar)
	onOK <- function() 
	{
	closeDialog()
   tmax <- round(as.numeric(tclvalue(tmaxVar)))
        if (is.na(tmax) || tmax <= 0) 
        {
        errorCondition(message = "Number of simulations must be a positive integer")
        return()
        }
  	cl <- round(as.numeric(tclvalue(clVar)))
        if (is.na(cl) || cl <= 0) 
        {
        errorCondition(message = "Number of columns on the simulated arena must be a positive integer.")
        return()
        }
	ln <- round(as.numeric(tclvalue(lnVar)))
        if (is.na(ln) || ln <= 0) 
        {
        errorCondition("Number of rows on the simulated arena must be a positive integer.")
        return()
        }
   ns <- as.numeric(tclvalue(nsVar))
        if (ns<0 ) 
        {
        errorCondition(message = "Mean number of propagulus must be positive")
        return()
        }
   nj <- as.numeric(tclvalue(njVar))
        if (nj<0 | nj > (cl*ln) ) 
        {
        errorCondition(message = "Number of juvenils must be positive and less than number of patchs")
        return()        
        }
   na <- as.numeric(tclvalue(naVar))
        if (na<0 | (na+nj) > (cl*ln) ) 
        {
        errorCondition(message = "Number of adults must be positive and adults plus juvenils less than number of patchs")
        return()
        }
   p.sj <- as.numeric(tclvalue(sjVar))
   p.jj <- as.numeric(tclvalue(jjVar))
   p.ja <- as.numeric(tclvalue(jaVar))
   p.aa <- as.numeric(tclvalue(aaVar))
   fec<- as.numeric(tclvalue(fecVar))
############ Data name
#popStr(p.sj, p.jj, p.ja, p.aa, fec, ns,nj,na, ln, cl, tmax)
   dsnameValue <- trim.blanks(tclvalue(dsname))
        if (dsnameValue == "Do_Not_Save" | dsnameValue=="") 
        {
        	command <- paste("popStr(tmax = ",tmax, ", cl = ", cl,", rw = ", ln, ", p.sj = ", p.sj,", p.jj = ", p.jj,", p.ja =", p.ja,", p.aa = ", p.aa,", fec = ", fec,", ns = ", ns,", nj = ", nj,", na = ", na, ")", sep = "")
        }
        #popStr=function(p.sj, p.jj, p.ja, p.aa, fec, ns,nj,na, ln, cl, tmax)
        else  
		  {
		  command <- paste(dsnameValue,"<- popStr(tmax = ",tmax, ", cl = ", cl,", rw = ", ln, ", p.sj = ", p.sj,", p.jj = ", p.jj,", p.ja =", p.ja,", p.aa = ", p.aa,", fec = ", fec,", ns = ", ns,", nj = ", nj,", na = ", na, ")", sep = "")
		  }
########
##popStr(p.sj, p.jj, p.ja, p.aa, fec, ns,nj,na, ln, cl, tmax)
	doItAndPrint(command)
	tkfocus(CommanderWindow())
	}
OKCancelHelp(helpSubject = "dynPop")
# data name
tkgrid(tklabel(top, text="Enter name for data set:"), entryDsname, sticky="e")
##
tkgrid(tklabel(top, text="Simulation Arena Conditions :", fg="blue"), sticky="w")
tkgrid(tklabel(top, text = "Maximum time"), tmaxEntry, sticky = "e")
tkgrid(tklabel(top, text = "Coluns"), clEntry, sticky = "e")
tkgrid(tklabel(top, text = "Rows"), lnEntry, sticky = "e")
#
tkgrid(tklabel(top, text="Initial Population Size :", fg="blue"), sticky="w")
tkgrid(tklabel(top, text = "Seeds "), nsEntry, sticky = "e")
tkgrid(tklabel(top, text = "Saplings "), njEntry, sticky = "e")
tkgrid(tklabel(top, text = "Adults "), naEntry, sticky = "e")
tkgrid(tklabel(top, text="Transitions Probabilities :", fg="blue"), sticky="w")
tkgrid(tklabel(top, text="Seed to sapling "), sjEntry, sticky="se")
tkgrid(tklabel(top, text="Remaing sapling "), jjEntry, sticky="se")
tkgrid(tklabel(top, text="Sapling to adult "), jaEntry, sticky="se")
tkgrid(tklabel(top, text="Adult survivel "), aaEntry, sticky="se")
tkgrid(tklabel(top, text="Fecundity "), fecEntry, sticky="e")
##
tkgrid(buttonsFrame, sticky = "w", columnspan = 2)
##
tkgrid.configure(entryDsname, sticky = "w")
tkgrid.configure(tmaxEntry, sticky = "w")
tkgrid.configure(clEntry, sticky = "w")
tkgrid.configure(lnEntry, sticky = "w")
tkgrid.configure(sjEntry, sticky = "w")
tkgrid.configure(jjEntry, sticky = "w")
tkgrid.configure(jaEntry, sticky = "w")
tkgrid.configure(aaEntry, sticky = "w")
tkgrid.configure(fecEntry, sticky = "w")
tkgrid.configure(nsEntry, sticky = "w")
tkgrid.configure(njEntry, sticky = "w")
tkgrid.configure(naEntry, sticky = "w")
dialogSuffix(rows = 14, columns = 2, focus = tmaxEntry)
}
############################################################

