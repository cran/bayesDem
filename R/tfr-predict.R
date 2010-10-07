TFRnewPred.group <- function(g, main.win, parent) {
	nb <- gnotebook(cont=g, expand=TRUE)
	all.c.g <- ggroup(label="<span color='#0B6138'>All Countries</span>", markup=TRUE, horizontal=FALSE, expand=TRUE, cont=nb)
	pred.all <- tfr.pred.all.countries.group(all.c.g, main.win, parent)
	extra.c.g <- ggroup(label="<span color='#0B6138'>Extra Areas &amp; Regions</span>", markup=TRUE, horizontal=FALSE, expand=TRUE, cont=nb)
	pred.extra <- tfr.pred.extra.countries.group(extra.c.g, main.win, parent)
	svalue(nb) <- 1
	return(pred.all)
}

tfr.pred.all.countries.group <- function(g, main.win, parent) {
	enable.pred.settings <- function(not.use.diag) {
		enabled(e$burnin) <- not.use.diag
		enabled(e$nr.traj) <- not.use.diag
		enabled(e$thin) <- not.use.diag	
	}
	e <- new.env()
	defaults <- formals(tfr.predict) # default argument values
	e$sim.dir <- parent$sim.dir
	
	pred.g <- gframe("<span color='blue'>Prediction</span>", markup=TRUE, horizontal=FALSE, cont=g)
	pred.g1 <- ggroup(horizontal=TRUE, cont=pred.g)
	glabel("End year:", cont=pred.g1)
	glabel("<span color='red'>*</span>", markup=TRUE, cont=pred.g1)
	e$end.year <- gedit(defaults$end.year, width=4, cont=pred.g1)
	e$use.diagnostics <- gcheckbox("Use diagnostics", checked = defaults$use.diagnostics, 
									handler=function(h, ...) enable.pred.settings(!svalue(h$obj)), 
									cont=pred.g1)
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
	enable.pred.settings(!defaults$use.diagnostics)
	
	sim.g <- gframe("<span color='blue'>Output settings</span>", markup=TRUE, horizontal=FALSE, cont=g)
	out.g1 <- ggroup(horizontal=TRUE, cont=sim.g)
	e$replace.output <- gcheckbox("Overwrite existing prediction in simulation directory", 
									checked=defaults$replace.output, cont=out.g1)
	sim.g2 <- ggroup(horizontal=TRUE, cont=sim.g)

	glabel("Nr. of ascii trajectories:", cont=sim.g2)
	e$save.as.ascii <- gedit(defaults$save.as.ascii, width=5, cont=sim.g2)
	
	ar.g <- gframe("<span color='blue'>AR(1) Process</span>", markup=TRUE, horizontal=TRUE, cont=g)
	glabel("mu:", cont=ar.g)
	e$mu <- gedit(defaults$mu, width=5, cont=ar.g)
	glabel("rho:", cont=ar.g)
	e$rho <- gedit(defaults$rho, width=6, cont=ar.g)
	glabel("sigma:", cont=ar.g)
	e$sigmaAR1 <- gedit(defaults$sigmaAR1, width=15, cont=ar.g)
	
	addSpring(g)
	predict.g <- ggroup(horizontal=TRUE, cont=g)
	create.help.button(topic='tfr.predict', package='bayesTFR', 
				parent.group=predict.g,
						parent.window=main.win)
	addSpring(predict.g)
	gbutton(' Generate Script ', cont=predict.g, handler=run.tfr.prediction,
				action=list(mw=main.win, env=e, script=TRUE))
	gbutton(action=gaction(label=' Make Prediction ', icon='evaluate', handler=run.tfr.prediction, 
				action=list(mw=main.win, env=e, script=FALSE)), cont=predict.g)
	return(e)

}

run.tfr.prediction <- function(h, ...)
{
	e <- h$action$env
	if(!has.required.arguments(list(sim.dir='Simulation directory',
									end.year='End year', burnin='Burnin'), env=e)) return()
	param.names <- list(numeric=c('mu', 'rho', 'end.year', 'burnin', 'seed', 'nr.traj', 'thin'), 
						numvector=c('sigmaAR1'),
						text=c('sim.dir'),
						logical=c('verbose', 'replace.output', 'use.diagnostics'),
						numtext=c('save.as.ascii') #can be both - numeric or text
						)
	params <- get.parameters(param.names, e, h$action$script)
	if(params$use.diagnostics) {
		params[['burnin']] <- NULL
		params[['thin']] <- NULL
		params[['nr.traj']] <- NULL
	}
	if (h$action$script) {
		script.text <- gwindow('bayeTFR commands', parent=h$action$mw)
		gtext(paste('tfr.predict(', paste(paste(names(params), params, sep='='), collapse=', '),
											 ')',sep=' '), 
					cont=script.text)
	} else {
		if(!params[['replace.output']] & has.tfr.prediction(sim.dir=params[['sim.dir']])) {
				gmessage(paste('Prediction for', params[['sim.dir']], 
								'already exists.\nCheck "Overwrite existing prediction" to delete it.'))
				return()
		}
		if(params$use.diagnostics) {
			mcmc.set <- get.tfr.mcmc(params[['sim.dir']])
			diag.list <- get.tfr.convergence.all(mcmc.set$meta$output.dir)
			if(length(diag.list) == 0) {
				gmessage(paste('There is no diagnostics available for', params[['sim.dir']],
							'. Use manual settings for Nr. of trajectories or Thin.'))
				return()
			}
		}
		do.call('tfr.predict', params)
	}
}
	
tfr.pred.extra.countries.group <- function(g, main.win, parent) {
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
				handler=multiSelectCountryMenuPred,
				action=list(mw=main.win, env=e))
	enabled(e$e.countries.gb) <- !svalue(e$all.countries)
	
	e$sim.g <- gframe("<span color='blue'>Output</span>", markup=TRUE, horizontal=FALSE, cont=g)
	e$sim.dir <- parent$sim.dir
	e$sim.g2 <- ggroup(horizontal=TRUE, cont=e$sim.g)
	glabel("# ascii trajectories:", cont=e$sim.g2)
	e$save.as.ascii <- gedit(defaults$save.as.ascii, width=5, cont=e$sim.g2)
	addSpace(e$sim.g2, 30)
	e$verbose <- gcheckbox("Verbose", checked=defaults$verbose, cont=e$sim.g2)

	addSpring(g)
	e$predict.g <- ggroup(horizontal=TRUE, cont=g)
	create.help.button(topic='tfr.predict.extra', package='bayesTFR', 
				parent.group=e$predict.g,
						parent.window=main.win)
	addSpring(e$predict.g)
	gbutton(' Generate Script ', cont=e$predict.g, handler=run.tfr.prediction.extra,
				action=list(mw=main.win, env=e, script=TRUE))
	gbutton(action=gaction(label=' Make Prediction ', icon='evaluate', handler=run.tfr.prediction.extra, 
				action=list(mw=main.win, env=e, script=FALSE)), cont=e$predict.g)

	return(e)
		  
}

run.tfr.prediction.extra <- function(h, ...)
{
	e <- h$action$env
	if(!has.required.arguments(list(sim.dir='Simulation directory'), env=e)) return()
	param.names <- list(text=c('sim.dir'),
						logical=c('verbose'),
						numtext=c('save.as.ascii') #can be both - numeric or text
						)
	params <- get.parameters(param.names, e, h$action$script)
	params[['countries']] <- if(svalue(e$all.countries)) NULL else e$selected.extra.countries
	
	if (h$action$script) {
		script.text <- gwindow('bayeTFR commands', parent=h$action$mw)
		gtext(paste('tfr.predict.extra(', paste(paste(names(params), params, sep='='), collapse=', '),
											 ')',sep=' '), 
					cont=script.text)
	} else {
		do.call('tfr.predict.extra', params)
	}
}

get.table.of.countries.not.predicted <- function(sim.dir, sorted=TRUE) {
	loc.data.pred <- get.table.of.countries.from.meta(sim.dir, prediction=TRUE, sorted=sorted)
	if(is.null(loc.data.pred)) return(NULL)
	loc.data.sim <- get.table.of.countries.from.meta(sim.dir, prediction=FALSE, sorted=sorted)
	if(is.null(loc.data.sim)) return(NULL)
	mcmc.set <- get.tfr.mcmc(sim.dir=sim.dir)
	loc.data.sim.extra <- loc.data.sim[(mcmc.set$meta$nr_countries_estimation+1):mcmc.set$meta$nr_countries,]

	# find countries without a prediction
	is.predicted <- is.element(loc.data.sim.extra[,'code'], loc.data.pred[,'code'])
	not.predicted.idx <- (1:dim(loc.data.sim.extra)[1])[!is.predicted]
	pr <- rep('yes', dim(loc.data.sim.extra)[1])
	pr[not.predicted.idx] <- 'no'
	loc.data<-cbind(loc.data.sim.extra, predicted=pr)
	
	if(sorted) {
		ord.idx <- order(loc.data[,'name'])
		loc.data <- loc.data[ord.idx,]
	}
	return(loc.data)
}

multiSelectCountryMenuPred <- function(h, ...) {
	country.selected <- function(h1, ...) {
		h$action$env$selected.extra.countries <- svalue(h$action$env$sel.extra.country.gt)
		visible(h$action$env$extra.country.sel.win) <- FALSE
	}
	new.window <- TRUE
	if (!is.null(h$action$env$extra.country.sel.win)) {
		# if anything has changed (sim.dir or the data), the window needs to be re-built
		if (svalue(h$action$env$sim.dir) != h$action$env$sim.dir.used) {
			dispose(h$action$env$extra.country.sel.win)
			new.window <- TRUE
		} else {
			extra.country.table <- get.table.of.countries.from.locfile(
												sim.dir=svalue(h$action$env$sim.dir),
												sorted=FALSE)
			if(dim(extra.country.table)[1] != dim(h$action$env$extra.country.table)[1]) {
				dispose(h$action$env$extra.country.sel.win)
				new.window <- TRUE
			} else {
				new.window <- FALSE
				visible(h$action$env$extra.country.sel.win) <- TRUE
			}
		}
	}
	if(new.window) {
		sim.dir.used <- svalue(h$action$env$sim.dir)
		country.table <- get.table.of.countries.not.predicted(sim.dir=sim.dir.used, sorted=FALSE)
		if (is.null(country.table)) return(NULL)
		h$action$env$sim.dir.used <- sim.dir.used
		h$action$env$extra.country.table <- country.table
		h$action$env$extra.country.sel.win <- win <- gwindow('Select countries & regions', 
							parent=h$action$mw, height=450,
							handler=function(h, ...) {
								h$action$env$extra.country.sel.win<-NULL;
								h$action$env$sel.extra.country.ok.handler <- NULL
							},
							action=list(env=h$action$env))
		t.group <- ggroup(horizontal=FALSE, cont=win)
		h$action$env$sel.extra.country.gt <- gtable(h$action$env$extra.country.table, cont=t.group, 
					expand=TRUE, multiple=TRUE, handler=country.selected)
		b.group <- ggroup(horizontal=TRUE, cont=t.group)
		gbutton('Cancel', cont=b.group, handler=function(h, ...) 
					visible(win) <- FALSE)
		addSpring(b.group)
		h$action$env$sel.extra.country.okbutton <- gbutton('OK', cont=b.group)
	}
	if(!is.null(h$action$env$sel.extra.country.ok.handler)) 
		removehandler(h$action$env$sel.extra.country.okbutton, h$action$env$sel.extra.country.ok.handler)
	h$action$env$sel.extra.country.ok.handler <- addhandlerclicked(
						h$action$env$sel.extra.country.okbutton, handler=country.selected)

}
