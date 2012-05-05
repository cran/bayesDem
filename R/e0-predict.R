e0NewPred.group <- function(g, main.win, parent) {
	nb <- gnotebook(container=g, expand=TRUE)
	all.c.g <- ggroup(label="<span color='#0B6138'>All Countries</span>", markup=TRUE, 
					horizontal=FALSE, expand=TRUE, container=nb)
	pred.all <- e0.pred.all.countries.group(all.c.g, main.win, parent)
	extra.c.g <- ggroup(label="<span color='#0B6138'>Extra Areas &amp; Regions</span>",
					 markup=TRUE, horizontal=FALSE, expand=TRUE, container=nb)
	pred.extra <- e0.pred.extra.countries.group(extra.c.g, main.win, parent)
	joint.male.g <- ggroup(label="<span color='#0B6138'>Joint Male</span>",
					 markup=TRUE, horizontal=FALSE, expand=TRUE, container=nb)
	joint.male <- e0.joint.male.group(joint.male.g, main.win, parent)
	edit.g <- ggroup(label="<span color='#0B6138'>Edit Predictions</span>", markup=TRUE, horizontal=FALSE, expand=TRUE, container=nb)
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
	
	pred.g <- gframe("<span color='blue'>Prediction</span>", markup=TRUE, horizontal=FALSE, container=g)
	pred.g1 <- ggroup(horizontal=TRUE, container=pred.g)
	glabel("End year:", container=pred.g1)
	glabel("<span color='red'>*</span>", markup=TRUE, container=pred.g1)
	e$end.year <- gedit(defaults$end.year, width=4, container=pred.g1)
	e$use.diagnostics <- gcheckbox("Use diagnostics", checked = defaults$use.diagnostics, 
									handler=function(h, ...) enable.pred.settings(!svalue(h$obj)), 
									container=pred.g1)
    enabled(e$use.diagnostics) <- FALSE
	addSpace(pred.g1, 20)
	glabel("RNG seed:", container=pred.g1)
	e$seed <- gedit(defaults$seed, width=4, container=pred.g1)
	addSpace(pred.g1, 10)
	e$verbose <- gcheckbox("Verbose", checked=defaults$verbose, container=pred.g1)

	pred.g2 <- ggroup(horizontal=TRUE, container=pred.g)
	glabel("Burnin:", container=pred.g2)
	glabel("<span color='red'>*</span>", markup=TRUE, container=pred.g2)
	e$burnin <- gedit(defaults$burnin, width=7, container=pred.g2)
	glabel("Nr. of trajectories:", container=pred.g2)
	e$nr.traj <- gedit(defaults$nr.traj, width=5, container=pred.g2)
	glabel("OR  Thin:", container=pred.g2)
	e$thin <- gedit(defaults$thin, width=5, container=pred.g2)
	#enable.pred.settings(!defaults$use.diagnostics)
	enable.pred.settings(TRUE)
	
	sim.g <- gframe("<span color='blue'>Output settings</span>", markup=TRUE, horizontal=FALSE, container=g)
	out.g1 <- ggroup(horizontal=TRUE, container=sim.g)
	e$replace.output <- gcheckbox("Overwrite existing prediction in simulation directory", 
									checked=defaults$replace.output, container=out.g1)
	sim.g2 <- ggroup(horizontal=TRUE, container=sim.g)

	glabel("Nr. of ascii trajectories:", container=sim.g2)
	e$save.as.ascii <- gedit(defaults$save.as.ascii, width=5, container=sim.g2)
		
	addSpring(g)
	predict.g <- ggroup(horizontal=TRUE, container=g)
	create.help.button(topic='e0.predict', package='bayesLife', 
				parent.group=predict.g,
						parent.window=main.win)
	addSpring(predict.g)
	gbutton(' Generate Script ', container=predict.g, handler=run.e0.prediction,
				action=list(mw=main.win, env=e, script=TRUE))
	gbutton(action=gaction(label=' Make Prediction ', icon='evaluate', handler=run.e0.prediction, 
				action=list(mw=main.win, env=e, script=FALSE)), container=predict.g)
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
	if (h$action$script) {
		script.text <- gwindow('bayesLife commands', parent=h$action$mw)
		gtext(paste('e0.predict(', paste(paste(names(params), params, sep='='), collapse=', '),
											 ')',sep=' '), 
					container=script.text)
	} else {
		if(!params[['replace.output']] && has.e0.prediction(sim.dir=params[['sim.dir']])) {
				gmessage(paste('Prediction for', params[['sim.dir']], 
								'already exists.\nCheck "Overwrite existing prediction" to delete it.'))
				return()
		}
		do.call('e0.predict', params)
	}
}

e0.pred.extra.countries.group <- function(g, main.win, parent) {
	e <- new.env()
	defaults <- formals(tfr.predict.extra) # default argument values
		
	e$countries.g <- gframe("<span color='blue'>Countries/Regions selection</span>", markup=TRUE, 
							horizontal=FALSE, container=g)
	e$countries.g1 <- ggroup(horizontal=TRUE, container=e$countries.g)
	e$all.countries <- gcheckbox("All without prediction", checked=TRUE, container=e$countries.g1,
									handler=function(h,...){
										enabled(e$e.countries.gb) <- !svalue(h$obj)
										})
	addSpace(e$countries.g1, 20)
	e$e.countries.gb <- gbutton("  Select specific countries/regions  ", container=e$countries.g1,
				handler=selectCountryMenuPred,
				action=list(mw=main.win, env=e, not.predicted=TRUE, multiple=TRUE, sorted=FALSE,
							type='e0'))
	enabled(e$e.countries.gb) <- !svalue(e$all.countries)
	
	e$sim.g <- gframe("<span color='blue'>Output</span>", markup=TRUE, horizontal=FALSE, container=g)
	e$sim.dir <- parent$sim.dir
	e$sim.g2 <- ggroup(horizontal=TRUE, container=e$sim.g)
	glabel("# ascii trajectories:", container=e$sim.g2)
	e$save.as.ascii <- gedit(defaults$save.as.ascii, width=5, container=e$sim.g2)
	addSpace(e$sim.g2, 30)
	e$verbose <- gcheckbox("Verbose", checked=defaults$verbose, container=e$sim.g2)

	addSpring(g)
	predict.g <- ggroup(horizontal=TRUE, container=g)
	create.help.button(topic='e0.predict.extra', package='bayesLife', 
				parent.group=predict.g,
						parent.window=main.win)
	addSpring(predict.g)
	gbutton(' Generate Script ', container=predict.g, handler=run.e0.prediction.extra,
				action=list(mw=main.win, env=e, script=TRUE))
	gbutton(action=gaction(label=' Make Prediction ', icon='evaluate', handler=run.e0.prediction.extra, 
				action=list(mw=main.win, env=e, script=FALSE)), container=predict.g)

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
					container=script.text)
	} else {
		do.call('e0.predict.extra', params)
	}
}

e0.joint.male.group <- function(g, main.win, parent) {
	e <- new.env()
	e$sim.dir <- parent$sim.dir
	defaults <- formals(e0.jmale.predict) # default argument values
	defaults.est <- formals(e0.jmale.estimate)
	estim.f <- gframe("<span color='blue'>Estimation Settings</span>", markup=TRUE, 
							horizontal=FALSE, container=g)
	g1 <- ggroup(horizontal=TRUE, container=estim.f)
	est.lo <- glayout(container=g1)
	est.lo[1,1] <- ''
	est.lo[1,2] <- 'Estimate DoF'
	est.lo[1,3] <- 'DoF'
	est.lo[1,4] <- 'Min. e0'
	est.lo[2,1] <- 'Equation 1'
	est.lo[2,2, anchor=c(0,0)] <- e$estDof.eq1 <- gcheckbox('', checked=defaults.est$estDof.eq1, container=est.lo)
	est.lo[2,3] <- e$eq1.dof <- gedit(defaults.est$start.eq1$dof, container=est.lo, width=5)
	est.lo[3,1] <- 'Equation 2'
	est.lo[3,2, anchor=c(0,0)] <- e$estDof.eq2 <- gcheckbox('', checked=defaults.est$estDof.eq2, container=est.lo)
	est.lo[3,3] <- e$eq2.dof <- gedit(defaults.est$start.eq2$dof, container=est.lo, width=5)
	est.lo[3,4] <- e$min.e0.eq2 <- gedit(defaults.est$min.e0.eq2, container=est.lo, width=5)
	
	g2 <- ggroup(horizontal=TRUE, container=estim.f)
	glabel("User-defined male e0 file:", container=g2)
	e$my.e0.file <- gfilebrowse(eval(defaults$my.e0.file), type='open', 
					  width=40, quote=FALSE, container=g2)
					  
	pred.f <- gframe("<span color='blue'>Prediction Settings</span>", markup=TRUE, 
							horizontal=FALSE, container=g)
	g3 <- ggroup(horizontal=TRUE, container=pred.f)
	glabel('Female-Male Gap limits:', container=g3)
	e$gap.lim <- gedit("0, 18", container=g3, width=10)
	addSpace(g3, 20)
	e$verbose <- gcheckbox("Verbose", checked=defaults$verbose, container=g3)
	
	addSpring(g)
	predict.g <- ggroup(horizontal=TRUE, container=g)
	create.help.button(topic=c('e0.jmale.predict', 'e0.jmale.estimate'), package='bayesLife', 
				parent.group=predict.g, parent.window=main.win)
	addSpring(predict.g)
	gbutton(' Generate Script ', container=predict.g, handler=joint.male.prediction,
				action=list(mw=main.win, env=e, script=TRUE))
	gbutton(action=gaction(label=' Make Prediction ', icon='evaluate', handler=joint.male.prediction, 
				action=list(mw=main.win, env=e, script=FALSE)), container=predict.g)
	return(e)
}

joint.male.prediction <- function(h, ...)
{
	e <- h$action$env
	if(!has.required.arguments(list(sim.dir='Simulation directory'), env=e)) return()
	param.names <- list(numeric=c('eq1.dof', 'eq2.dof', 'min.e0.eq2'), 
						numvector=c('gap.lim'),
						text=c('sim.dir', 'my.e0.file'),
						logical=c('verbose', 'estDof.eq1', 'estDof.eq2')
						)
	params <- get.parameters(param.names, e, h$action$script)
	if(!is.null(params$eq1.dof)) {
		params$start.eq1 <- list(dof=params$eq1.dof)
		params$eq1.dof <- NULL
	}
	if(!is.null(params$eq2.dof)) {
		params$start.eq2 <- list(dof=params$eq2.dof)
		params$eq2.dof <- NULL
	}
	
	if (h$action$script) {
		script.text <- gwindow('bayesLife commands', parent=h$action$mw)
		cmd <- paste('pred <- get.e0.prediction(sim.dir=', params$sim.dir, 
						')\n', sep='')
		params$sim.dir <- NULL
		cmd <- paste(cmd, 'e0.jmale.predict(pred,', paste(paste(names(params), params, sep='='), collapse=', '),
											 ')',sep=' ')
		gtext(cmd, container=script.text)
	} else {
		pred <- get.e0.prediction(params$sim.dir)
		params$sim.dir <- NULL
		do.call('e0.jmale.predict', c(list(e0.pred=pred), params))
	}
}