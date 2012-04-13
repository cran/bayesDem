TFRnewPred.group <- function(g, main.win, parent) {
	nb <- gnotebook(container=g, expand=TRUE)
	all.c.g <- ggroup(label="<span color='#0B6138'>All Countries</span>", 
							markup=TRUE, horizontal=FALSE, expand=TRUE, container=nb)
	pred.all <- tfr.pred.all.countries.group(all.c.g, main.win, parent)
	extra.c.g <- ggroup(label="<span color='#0B6138'>Extra Areas &amp; Regions</span>", 
							markup=TRUE, horizontal=FALSE, expand=TRUE, container=nb)
	pred.extra <- tfr.pred.extra.countries.group(extra.c.g, main.win, parent)
	edit.g <- ggroup(label="<span color='#0B6138'>Edit Predictions</span>", 
							markup=TRUE, horizontal=FALSE, expand=TRUE, container=nb)
	pred.edit <- edit.predictions.group(edit.g, main.win, parent)
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
	
	pred.g <- gframe("<span color='blue'>Prediction</span>", markup=TRUE, horizontal=FALSE, container=g)
	pred.g1 <- ggroup(horizontal=TRUE, container=pred.g)
	glabel("End year:", container=pred.g1)
	glabel("<span color='red'>*</span>", markup=TRUE, container=pred.g1)
	e$end.year <- gedit(defaults$end.year, width=4, container=pred.g1)
	e$use.diagnostics <- gcheckbox("Use diagnostics", checked = defaults$use.diagnostics, 
									handler=function(h, ...) enable.pred.settings(!svalue(h$obj)), 
									container=pred.g1)
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
	enable.pred.settings(!defaults$use.diagnostics)
	
	sim.g <- gframe("<span color='blue'>Output settings</span>", markup=TRUE, horizontal=FALSE, container=g)
	out.g1 <- ggroup(horizontal=TRUE, container=sim.g)
	e$replace.output <- gcheckbox("Overwrite existing prediction in simulation directory", 
									checked=defaults$replace.output, container=out.g1)
	sim.g2 <- ggroup(horizontal=TRUE, container=sim.g)

	glabel("Nr. of ascii trajectories:", container=sim.g2)
	e$save.as.ascii <- gedit(defaults$save.as.ascii, width=5, container=sim.g2)
	
	ar.g <- gframe("<span color='blue'>AR(1) Process</span>", markup=TRUE, horizontal=TRUE, container=g)
	glabel("mu:", container=ar.g)
	e$mu <- gedit(defaults$mu, width=5, container=ar.g)
	glabel("rho:", container=ar.g)
	e$rho <- gedit(defaults$rho, width=6, container=ar.g)
	glabel("sigma:", container=ar.g)
	e$sigmaAR1 <- gedit(defaults$sigmaAR1, width=15, container=ar.g)
	
	addSpring(g)
	predict.g <- ggroup(horizontal=TRUE, container=g)
	create.help.button(topic='tfr.predict', package='bayesTFR', 
				parent.group=predict.g,
						parent.window=main.win)
	addSpring(predict.g)
	gbutton(' Generate Script ', container=predict.g, handler=run.tfr.prediction,
				action=list(mw=main.win, env=e, script=TRUE))
	gbutton(action=gaction(label=' Make Prediction ', icon='evaluate', handler=run.tfr.prediction, 
				action=list(mw=main.win, env=e, script=FALSE)), container=predict.g)
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
	if(is.null(params$rho)) params$rho <- NA
	if(is.null(params$sigmaAR1)) params$sigmaAR1 <- NA
	if(params$use.diagnostics) {
		params[['burnin']] <- NULL
		params[['thin']] <- NULL
		params[['nr.traj']] <- NULL
	}
	if (h$action$script) {
		script.text <- gwindow('bayesTFR commands', parent=h$action$mw)
		gtext(paste('tfr.predict(', paste(paste(names(params), params, sep='='), collapse=', '),
											 ')',sep=' '), 
					container=script.text)
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
							horizontal=FALSE, container=g)
	e$countries.g1 <- ggroup(horizontal=TRUE, container=e$countries.g)
	e$all.countries <- gcheckbox("All without prediction", checked=TRUE, container=e$countries.g1,
									handler=function(h,...){
										enabled(e$e.countries.gb) <- !svalue(h$obj)
										})
	addSpace(e$countries.g1, 20)
	e$e.countries.gb <- gbutton("  Select specific countries/regions  ", container=e$countries.g1,
				handler=selectCountryMenuPred,
				action=list(mw=main.win, env=e, not.predicted=TRUE, multiple=TRUE, sorted=FALSE))
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
	create.help.button(topic='tfr.predict.extra', package='bayesTFR', 
				parent.group=predict.g,
						parent.window=main.win)
	addSpring(predict.g)
	gbutton(' Generate Script ', container=predict.g, handler=run.tfr.prediction.extra,
				action=list(mw=main.win, env=e, script=TRUE))
	gbutton(action=gaction(label=' Make Prediction ', icon='evaluate', handler=run.tfr.prediction.extra, 
				action=list(mw=main.win, env=e, script=FALSE)), container=predict.g)

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
	params[['countries']] <- if(svalue(e$all.countries)) NULL else e$selected.countries
	
	if (h$action$script) {
		script.text <- gwindow('bayesTFR commands', parent=h$action$mw)
		gtext(paste('tfr.predict.extra(', paste(paste(names(params), params, sep='='), collapse=', '),
											 ')',sep=' '), 
					container=script.text)
	} else {
		do.call('tfr.predict.extra', params)
	}
}

edit.predictions.group <- function(g, main.win, parent, type='tfr') {
	e <- new.env()
	e$sim.dir <- parent$sim.dir
	e$edit.frame <- gframe("<span color='blue'>Edit Median</span>", markup=TRUE, 
						horizontal=FALSE, container=g, expand=TRUE)
	gbutton("  Select specific country/region  ", container=e$edit.frame,
				handler=selectCountryMenuPred,
				action=list(mw=main.win, env=e, not.predicted=FALSE, multiple=FALSE,
							edit.median=TRUE, sorted=TRUE, type=type))

	#addSpring(g)
	button.g <- ggroup(horizontal=TRUE, container=g)
	addSpring(button.g)
	e$restoreb <- gbutton(action=gaction(label='Restore BHM medians', 
				handler=restore.bhm.medians, action=list(mw=main.win, env=e, type=type)), container=button.g)
	enabled(e$restoreb) <- FALSE
	e$applyb <- gbutton(action=gaction(label='Save', handler=edit.prediction, 
				action=list(mw=main.win, env=e, type=type)), container=button.g)
	enabled(e$applyb) <- FALSE
	
	addSpring(g)
	convert.frame <- gframe("<span color='blue'>Convert trajectories to ASCII</span>", markup=TRUE, 
						horizontal=TRUE, container=g)
	glabel("Nr. of ascii trajectories:", container=convert.frame)
	e$save.as.ascii <- gedit(formals(paste(type,'.predict', sep=''))$save.as.ascii, width=5, container=convert.frame)
	e$write.summary <- gcheckbox("Write summary files", checked=TRUE, container=convert.frame)
	e$verbose <- gcheckbox("Verbose", checked=FALSE, container=convert.frame)
	addSpring(convert.frame)
	button.conv <- gbutton(action=gaction(label='Convert', handler=.convert.trajectories, 
				action=list(mw=main.win, env=e, type=type)), container=convert.frame)
}

edit.prediction <- function(h, ...) {
	e <- h$action$env
	values <- unlist(e$median.df[1,])
	where.modified <- values != e$medians
	do.call(paste(h$action$type,'.median.set', sep=''), list(e$sim.dir.value, e$edit.country.obj$code, 
					values=values[where.modified], years=as.numeric(names(values)[where.modified])))
	h$action$env$medians <- values
	enabled(e$applyb) <- FALSE
}

restore.bhm.medians <- function(h, ...) {
	e <- h$action$env
	new.pred <- do.call(paste(h$action$type, '.median.shift', sep=''), list(e$sim.dir.value, 
						e$edit.country.obj$code, reset=TRUE))
	data <- .get.data.for.median.editor(new.pred, e$edit.country.obj)
	e$median.df[,] <- data
}

.convert.trajectories <- function(h, ...) {
	e <- h$action$env
	sim.dir <- svalue(e$sim.dir)
	nr.traj <- as.numeric(svalue(e$save.as.ascii))
	do.call(paste('convert.', h$action$type, '.trajectories', sep=''), 
				list(dir=sim.dir, n=nr.traj, verbose=svalue(e$verbose)))
	if(svalue(e$write.summary)) {
		if(h$action$type == 'tfr')
			write.projection.summary(dir=sim.dir)
		else write.e0.projection.summary(dir=sim.dir)
	}
}

get.table.of.countries.from.prediction <- function(sim.dir, not.predicted=TRUE, sorted=TRUE, type='tfr') {
	loc.data.pred <- get.table.of.countries.from.meta(sim.dir, prediction=TRUE, sorted=sorted, pred.type=type)
	if(is.null(loc.data.pred)) return(NULL)
	if(!not.predicted) return(loc.data.pred)
	loc.data.sim <- get.table.of.countries.from.meta(sim.dir, prediction=FALSE, sorted=sorted, pred.type=type)
	if(is.null(loc.data.sim)) return(NULL)
	mcmc.set <- do.call(paste('get.', type, '.mcmc', sep=''), list(sim.dir=sim.dir))
	if (bayesTFR:::get.nr.countries(mcmc.set$meta) <= bayesTFR:::get.nr.countries.est(mcmc.set$meta)) {
		gmessage("No countries/regions without a prediction available.")
		return(NULL)
	}
	loc.data.sim.extra <- loc.data.sim[(bayesTFR:::get.nr.countries.est(mcmc.set$meta)+1):(bayesTFR:::get.nr.countries(mcmc.set$meta)),]
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

.get.data.for.median.editor <- function(pred, country.obj) {
	pred.years <- dimnames(pred$quantiles)[[3]]
	medians <- get.median.from.prediction(pred, country.obj$index, country.obj$code)
	data <- matrix(medians, ncol=length(medians))
	colnames(data) <- pred.years
	rownames(data) <- 'medians'
	return(data)
}

show.median.editor <- function(e, type='tfr'){
	sim.dir <- svalue(e$sim.dir)
	pred <- do.call(paste('get.', type, '.prediction', sep=''), list(sim.dir))
	country.obj <- get.country.object(e$selected.countries, meta=pred$mcmc.set$meta)
	if(!is.null(e$country.label)) svalue(e$country.label) <- country.obj$name
	else {
		addSpring(e$edit.frame)	
		e$country.label <- glabel(country.obj$name, container=e$edit.frame)
	}
	e$sim.dir.value <- sim.dir
	e$edit.country.obj <- country.obj
	data <- .get.data.for.median.editor(pred, country.obj)
	e$medians <- data[1,]
	if(!is.null(e$median.df)) e$median.df[,] <- data
	else {
		f <- function(env) {
				enabled(env$applyb) <- TRUE; enabled(env$restoreb) <- TRUE
				}
		df.view <- makeDFView(data, e$edit.frame, f, e)
		e$median.df <- df.view$model
		#e$median.df <- gdf(data, container=e$edit.frame)
		#addhandlerchanged(e$median.df, handler=function(h, ...) {
		#		enabled(e$applyb) <- TRUE; enabled(e$restoreb) <- TRUE
		#		})
	}
}

selectCountryMenuPred <- function(h, ...) {
	country.selected <- function(h1, ...) {
		h$action$env$selected.countries <- svalue(h$action$env$sel.extra.country.gt)
		if(!is.null(h$action$edit.median) && h$action$edit.median) show.median.editor(h$action$env, h$action$type) 
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
												sorted=h$action$sorted, 
												type=if(is.null(h$action$type)) 'tfr' else h$action$type)
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
		country.table <- get.table.of.countries.from.prediction(sim.dir=sim.dir.used, 
							not.predicted=h$action$not.predicted, sorted=h$action$sorted,
							type=if(is.null(h$action$type)) 'tfr' else h$action$type)
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
		t.group <- ggroup(horizontal=FALSE, container=win)
		h$action$env$sel.extra.country.gt <- gtable(h$action$env$extra.country.table, container=t.group, 
					expand=TRUE, multiple=h$action$multiple, handler=country.selected)
		b.group <- ggroup(horizontal=TRUE, container=t.group)
		gbutton('Cancel', container=b.group, handler=function(h, ...) 
					visible(win) <- FALSE)
		addSpring(b.group)
		h$action$env$sel.extra.country.okbutton <- gbutton('OK', container=b.group)
	}
	if(!is.null(h$action$env$sel.extra.country.ok.handler)) 
		removehandler(h$action$env$sel.extra.country.okbutton, h$action$env$sel.extra.country.ok.handler)
	h$action$env$sel.extra.country.ok.handler <- addhandlerclicked(
						h$action$env$sel.extra.country.okbutton, handler=country.selected)

}
