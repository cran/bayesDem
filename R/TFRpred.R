

tfrPredTab <- function(tfr.pred, main.win, wpp.year) {
	eTFRp <- new.env()

	eTFRp$wpp.year <- wpp.year
	
	create.sim.dir.widget(env=eTFRp, parent=tfr.pred, type='tfr', 
				main.win=main.win,
				default=eval(formals(run.tfr.mcmc)$output.dir))
	
	eTFRp$nb <- gnotebook(container=tfr.pred, expand=TRUE)
	# Run MCMC group
	# color chosen from http://html-color-codes.info
	eTFRp$mcmc.g <- ggroup(label="<span color='#B40404'>Run MCMC</span>", markup=TRUE, 
							horizontal=FALSE, container=eTFRp$nb, spacing=10)
	eTFRp$mcmc.g.env <- TFRrunMCMCgroup(eTFRp$mcmc.g, main.win, parent=eTFRp)	
	# Continue MCMC group
	eTFRp$cont.mcmc.g <- ggroup(label="<span color='#B40404'>Continue MCMC</span>", markup=TRUE, 
							horizontal=FALSE, container=eTFRp$nb, spacing=10)
	eTFRp$cont.mcmc.g.env <- TFRcontinueMCMCgroup(eTFRp$cont.mcmc.g, main.win, parent=eTFRp)

	# Predictions group
	eTFRp$pred.g <- ggroup(label="<span color='#B40404'>Make predictions</span>", markup=TRUE,
							horizontal=FALSE, container=eTFRp$nb, spacing=10)
	eTFRp$pred.g.env <- TFRnewPred.group(eTFRp$pred.g, main.win, parent=eTFRp)
	
	# Result group
	eTFRp$result.g <- ggroup(label="<span color='#B40404'>Explore results</span>", markup=TRUE,
							horizontal=FALSE, container=eTFRp$nb)
	eTFRp$result.g.env <- TFRresults.group(eTFRp$result.g, main.win, parent=eTFRp)
	
	svalue(eTFRp$nb) <- 1
	label <- glabel(paste('Dependency in use: bayesTFR  v.', packageVersion("bayesTFR")), container=tfr.pred)
	font(label) <- c(style='italic', family='serif')
}






