

tfrPredTab <- function(tfr.pred, main.win, wpp.year=wpp.year.default) {
	eTFRp <- new.env()


	eTFRp$wpp.year <- wpp.year
	
	sim.g <- ggroup(horizontal=TRUE, cont=tfr.pred)
	glabel("Simulation directory:", cont=sim.g)
	glabel("<span color='red'>*</span>", markup=TRUE, cont=sim.g)
	eTFRp$sim.dir <- gfilebrowse(eval(formals(run.tfr.mcmc)$output.dir), type='selectdir', 
					  width=40, quote=FALSE, 
					  cont=sim.g)
	create.info.button('sim.dir', sim.g, main.win, eTFRp)
	
	eTFRp$nb <- gnotebook(cont=tfr.pred, expand=TRUE)
	# Run MCMC group
	# color chosen from http://html-color-codes.info
	eTFRp$mcmc.g <- ggroup(label="<span color='#B40404'>Run MCMC</span>", markup=TRUE, 
							horizontal=FALSE, cont=eTFRp$nb, spacing=10)
	eTFRp$mcmc.g.env <- TFRrunMCMCgroup(eTFRp$mcmc.g, main.win, parent=eTFRp)	
	# Continue MCMC group
	eTFRp$cont.mcmc.g <- ggroup(label="<span color='#B40404'>Continue MCMC</span>", markup=TRUE, 
							horizontal=FALSE, cont=eTFRp$nb, spacing=10)
	eTFRp$cont.mcmc.g.env <- TFRcontinueMCMCgroup(eTFRp$cont.mcmc.g, main.win, parent=eTFRp)

	# Predictions group
	eTFRp$pred.g <- ggroup(label="<span color='#B40404'>Make predictions</span>", markup=TRUE,
							horizontal=FALSE, cont=eTFRp$nb, spacing=10)
	eTFRp$pred.g.env <- TFRnewPred.group(eTFRp$pred.g, main.win, parent=eTFRp)
	
	# Data group
	eTFRp$result.g <- ggroup(label="<span color='#B40404'>Explore results</span>", markup=TRUE,
							horizontal=FALSE, cont=eTFRp$nb)
	eTFRp$result.g.env <- TFRresults.group(eTFRp$result.g, main.win, parent=eTFRp)
	
	svalue(eTFRp$nb) <- 1
	label <- glabel(paste('Dependency in use: bayesTFR  v.', installed.packages()["bayesTFR", "Version"]), cont=tfr.pred)
	font(label) <- c(style='italic', family='serif')
}






