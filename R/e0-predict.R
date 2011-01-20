e0NewPred.group <- function(g, main.win, parent) {
	nb <- gnotebook(cont=g, expand=TRUE)
	all.c.g <- ggroup(label="<span color='#0B6138'>All Countries</span>", markup=TRUE, 
					horizontal=FALSE, expand=TRUE, cont=nb)
	pred.all <- e0.pred.all.countries.group(all.c.g, main.win, parent)
	extra.c.g <- ggroup(label="<span color='#0B6138'>Extra Areas &amp; Regions</span>",
					 markup=TRUE, horizontal=FALSE, expand=TRUE, cont=nb)
	pred.extra <- e0.pred.extra.countries.group(extra.c.g, main.win, parent)
	edit.g <- ggroup(label="<span color='#0B6138'>Edit Predictions</span>", markup=TRUE, horizontal=FALSE, expand=TRUE, cont=nb)
	pred.edit <- edit.predictions.group(edit.g, main.win, parent, type='e0')
	svalue(nb) <- 1
	return(pred.all)
}

e0.pred.all.countries.group <- function(g, main.win, parent) {
	enable.pred.settings <- function(not.use.diag) {
		enabled(e$burnin) <- not.use.diag
		enabled(e$nr.traj) <- not.use.diag
		enabled(e$thin) <- not.use.diag	
	}
	e <- new.env()
	defaults <- formals(e0.predict) # default argument values
	e$sim.dir <- parent$sim.dir
	
	pred.g <- gframe("<span color='blue'>Prediction</span>", markup=TRUE, horizontal=FALSE, cont=g)
	pred.g1 <- ggroup(horizontal=TRUE, cont=pred.g)
	glabel("End year:", cont=pred.g1)
	glabel("<span color='red'>*</span>", markup=TRUE, cont=pred.g1)
	e$end.year <- gedit(defaults$end.year, width=4, cont=pred.g1)
	e$use.diagnostics <- gcheckbox("Use diagnostics", checked = defaults$use.diagnostics, 
									handler=function(h, ...) enable.pred.settings(!svalue(h$obj)), 
									cont=pred.g1)
    enabled(e$use.diagnostics) <- FALSE
	addSpace(pred.g1, 20)
	glabel("RNG seed:", cont=pred.g1)
	e$seed <- gedit(defaults$seed, width=4, cont=pred.g1)
	addSpace(pred.g1, 10)
	e$verbose <- gcheckbox("Verbose", checked=defaults$verbose, cont=pred.g1)

	pred.g2 <- ggroup(horizontal=TRUE, cont=pred.g)
	glabel("Burnin:", cont=pred.g2)
	glabel("<span color='red'>*</span>", markup=TRUE, cont=pred.g2)
	e$burnin <- gedit(defaults$burnin, width=7, cont=pred.g2)
	glabel("Nr. of trajectories:", cont=pred.g2)
	e$nr.traj <- gedit(defaults$nr.traj, width=5, cont=pred.g2)
	glabel("OR  Thin:", cont=pred.g2)
	e$thin <- gedit(defaults$thin, width=5, cont=pred.g2)
	#enable.pred.settings(!defaults$use.diagnostics)
	enable.pred.settings(TRUE)
	
	sim.g <- gframe("<span color='blue'>Output settings</span>", markup=TRUE, horizontal=FALSE, cont=g)
	out.g1 <- ggroup(horizontal=TRUE, cont=sim.g)
	e$replace.output <- gcheckbox("Overwrite existing prediction in simulation directory", 
									checked=defaults$replace.output, cont=out.g1)
	sim.g2 <- ggroup(horizontal=TRUE, cont=sim.g)

	glabel("Nr. of ascii trajectories:", cont=sim.g2)
	e$save.as.ascii <- gedit(defaults$save.as.ascii, width=5, cont=sim.g2)
		
	addSpring(g)
	predict.g <- ggroup(horizontal=TRUE, cont=g)
	create.help.button(topic='e0.predict', package='bayesLife', 
				parent.group=predict.g,
						parent.window=main.win)
	addSpring(predict.g)
	gbutton(' Generate Script ', cont=predict.g, handler=run.e0.prediction,
				action=list(mw=main.win, env=e, script=TRUE))
	gbutton(action=gaction(label=' Make Prediction ', icon='evaluate', handler=run.e0.prediction, 
				action=list(mw=main.win, env=e, script=FALSE)), cont=predict.g)
	return(e)

}

run.e0.prediction <- function(h, ...)
{
	e <- h$action$env
	if(!has.required.arguments(list(sim.dir='Simulation directory',
									end.year='End year', burnin='Burnin'), env=e)) return()
	param.names <- list(numeric=c('end.year', 'burnin', 'seed', 'nr.traj', 'thin'), 
						text=c('sim.dir'),
						logical=c('verbose', 'replace.output'),
						numtext=c('save.as.ascii') #can be both - numeric or text
						)
	params <- get.parameters(param.names, e, h$action$script)
	#if(params$use.diagnostics) {
	#	params[['burnin']] <- NULL
	#	params[['thin']] <- NULL
	#	params[['nr.traj']] <- NULL
	#}
	if (h$action$script) {
		script.text <- gwindow('bayesLife commands', parent=h$action$mw)
		gtext(paste('e0.predict(', paste(paste(names(params), params, sep='='), collapse=', '),
											 ')',sep=' '), 
					cont=script.text)
	} else {
		if(!params[['replace.output']] & has.tfr.prediction(sim.dir=params[['sim.dir']])) {
				gmessage(paste('Prediction for', params[['sim.dir']], 
								'already exists.\nCheck "Overwrite existing prediction" to delete it.'))
				return()
		}
#		if(params$use.diagnostics) {
#			mcmc.set <- get.tfr.mcmc(params[['sim.dir']])
#			diag.list <- get.tfr.convergence.all(mcmc.set$meta$output.dir)
#			if(length(diag.list) == 0) {
#				gmessage(paste('There is no diagnostics available for', params[['sim.dir']],
#							'. Use manual settings for Nr. of trajectories or Thin.'))
#				return()
#			}
#		}
		do.call('e0.predict', params)
	}
}

e0.pred.extra.countries.group <- function(g, main.win, parent) {
	e <- new.env()
	defaults <- formals(tfr.predict.extra) # default argument values
		
	e$countries.g <- gframe("<span color='blue'>Countries/Regions selection</span>", markup=TRUE, 
							horizontal=FALSE, cont=g)
	e$countries.g1 <- ggroup(horizontal=TRUE, cont=e$countries.g)
	e$all.countries <- gcheckbox("All without prediction", checked=TRUE, cont=e$countries.g1,
									handler=function(h,...){
										enabled(e$e.countries.gb) <- !svalue(h$obj)
										})
	addSpace(e$countries.g1, 20)
	e$e.countries.gb <- gbutton("  Select specific countries/regions  ", cont=e$countries.g1,
				handler=selectCountryMenuPred,
				action=list(mw=main.win, env=e, not.predicted=TRUE, multiple=TRUE, sorted=FALSE,
							type='e0'))
	enabled(e$e.countries.gb) <- !svalue(e$all.countries)
	
	e$sim.g <- gframe("<span color='blue'>Output</span>", markup=TRUE, horizontal=FALSE, cont=g)
	e$sim.dir <- parent$sim.dir
	e$sim.g2 <- ggroup(horizontal=TRUE, cont=e$sim.g)
	glabel("# ascii trajectories:", cont=e$sim.g2)
	e$save.as.ascii <- gedit(defaults$save.as.ascii, width=5, cont=e$sim.g2)
	addSpace(e$sim.g2, 30)
	e$verbose <- gcheckbox("Verbose", checked=defaults$verbose, cont=e$sim.g2)

	addSpring(g)
	predict.g <- ggroup(horizontal=TRUE, cont=g)
	create.help.button(topic='e0.predict.extra', package='bayesLife', 
				parent.group=predict.g,
						parent.window=main.win)
	addSpring(predict.g)
	gbutton(' Generate Script ', cont=predict.g, handler=run.e0.prediction.extra,
				action=list(mw=main.win, env=e, script=TRUE))
	gbutton(action=gaction(label=' Make Prediction ', icon='evaluate', handler=run.e0.prediction.extra, 
				action=list(mw=main.win, env=e, script=FALSE)), cont=predict.g)

	return(e)		  
}

run.e0.prediction.extra <- function(h, ...)
{
	e <- h$action$env
	if(!has.required.arguments(list(sim.dir='Simulation directory'), env=e)) return()
	param.names <- list(text=c('sim.dir'),
						logical=c('verbose'),
						numtext=c('save.as.ascii') #can be both - numeric or text
						)
	params <- get.parameters(param.names, e, h$action$script)
	params[['countries']] <- if(svalue(e$all.countries)) NULL else e$selected.countries
	
	if (h$action$script) {
		script.text <- gwindow('bayesLife commands', parent=h$action$mw)
		gtext(paste('e0.predict.extra(', paste(paste(names(params), params, sep='='), collapse=', '),
											 ')',sep=' '), 
					cont=script.text)
	} else {
		do.call('e0.predict.extra', params)
	}
}

