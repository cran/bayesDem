TFRcontinueMCMCgroup <- function(g, main.win, parent=NULL) {
	e <- new.env()
	defaults <- formals(continue.tfr.mcmc) # default argument values
	e$output.dir <- parent$sim.dir
	e$run.prediction <- FALSE
	addSpace(g, 10)
	g2 <- ggroup(horizontal=TRUE, container=g)
	.create.autoconf.cont.group(g2, e, main.win, defaults)
	.create.process.group(g2, e, defaults, show.buffer.size=FALSE)
	addSpace(g, 10)
	.create.status.label(g, e)

	addSpring(g)
	cont.g <- ggroup(horizontal=TRUE, container=g)
	create.help.button(topic='continue.tfr.mcmc', package='bayesTFR', parent.group=cont.g,
						parent.window=main.win)
	addSpring(cont.g)
	create.generate.script.button(handler=mcmc.continue, action=list(mw=main.win, env=e, script=TRUE),
								container=cont.g)
	bDem.gbutton(action=gaction(label=' Continue MCMC ', icon='execute', handler=mcmc.continue, 
				action=list(mw=main.win, env=e, script=FALSE)), container=cont.g)
	return(e)
}

.create.autoconf.cont.group <- function(g, e, main.win, defaults) {
	leftcenter <- c(-1,0)
	mcmc.g <- gframe("<span color='blue'>MCMC</span>", markup=TRUE, horizontal=FALSE, spacing=10, container=g)
	mclo <- glayout(container=mcmc.g)
	mclo[1,1] <- e$run.auto <- gcheckbox("Auto simulation", checked=FALSE, 
							container=mclo, handler=function(h,...){.enable.auto.cont(svalue(h$obj), e)})
	mclo[1,2] <- e$auto.conf.b <- bDem.gbutton(' Configure auto run ', container=mclo, handler=configure.auto.run, 
				action=list(mw=main.win, env=e, cont.run=TRUE))
	mclo[2,1, anchor=leftcenter] <- glabel("Number of iterations:", container=mclo)
	mclo[2,2] <- e$iter <- gedit(defaults$iter, width=7, container=mclo)
	mclo[3,1, anchor=leftcenter] <- glabel("Chain ids:", container=mclo)
	mclo[3,2] <- e$chain.ids <- gedit(defaults$chain.ids, width=10, container=mclo)
	.enable.auto.cont(FALSE, e)
}

.enable.auto.cont <- function(enable, e) {
	enabled(e$auto.conf.b) <- enable
	enabled(e$chain.ids) <- !enable
	enabled(e$iter) <- !enable 
}

.get.defaults.for.auto.cont.tfr <- function(e) {
	mcmc.set <- get.tfr.mcmc(sim.dir=svalue(e$output.dir))
	if(is.null(mcmc.set)) {
		gmessage('Simulation directory contains no valid TFR MCMCs.', title='Input Error',
					icon='error')
		return(NULL)
	}
	return(mcmc.set$meta$auto.conf)
}

mcmc.continue <- function(h, ...) {
	e <- h$action$env
	if(!has.required.arguments(list(output.dir='Simulation directory'), env=e)) return()
	run.auto <- svalue(e$run.auto)
	if (!run.auto && !has.required.arguments(list(iter='Number of iterations'), env=e)) return()
	params <- list()
	param.names <- list(numeric=c('iter', 'nr.nodes'),
						text=c('output.dir'),
						logical=c('verbose', 'parallel'),
						numvector=c('chain.ids'))
	params <- get.parameters(param.names, e, quote=h$action$script)
	if (run.auto) {
		params[['auto.conf']] <- e$auto.conf
		params[['iter']] <- if (h$action$script) sQuote('auto') else 'auto'
	}
	if (h$action$script) {
		commands <- paste('m <- continue.tfr.mcmc(', paste(paste(names(params), params, sep='='), collapse=', '),
											 ')',sep=' ')
		if(run.auto && e$run.prediction) 
			commands <- paste(commands, '\n\ntfr.predict(m, use.diagnostics=TRUE, replace.output=TRUE)', sep='')
		create.script.widget(commands, h$action$mw, package="bayesTFR")
	} else {
		m <- .run.simulation(e, handler=get.tfr.simulation.status, option='bDem.TFRmcmc', 
								call='continue.tfr.mcmc', params=params, 
								sim.name='TFR MCMC simulation', main.win=h$action$mw,
								action=list(sb=e$statuslabel, sim.dir=params[['output.dir']]),
								interval=5000)
		if(run.auto && e$run.prediction)
			.run.prediction(e, handler=get.tfr.prediction.status, option='bDem.TFRpred', 
								call='tfr.predict', params=list(m, use.diagnostics=TRUE, replace.output=TRUE), 
								sim.name='TFR prediction', main.win=h$action$mw,
								action=list(sb=e$statuslabel),
								interval=1000)
	}
}
