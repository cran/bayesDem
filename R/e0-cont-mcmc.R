e0ContinueMCMCgroup <- function(g, main.win, parent=NULL) {
	e <- new.env()
	defaults <- formals(continue.e0.mcmc) # default argument values

	e$output.dir <- parent$sim.dir
	#mcmc.g <- gframe("<span color='blue'>MCMC</span>", markup=TRUE, horizontal=FALSE, container=g)
	#auto.g <- ggroup(horizontal=TRUE, container=mcmc.g)
	e$run.prediction <- FALSE
	addSpace(g, 10)
	g2 <- ggroup(horizontal=TRUE, container=g)
	.create.autoconf.cont.group(g2, e, main.win, defaults)
	# e$run.auto <- gcheckbox("Auto simulation", checked=FALSE, 
							# container=auto.g, handler=function(h,...){.enable.auto.cont(svalue(h$obj), e)})
	# e$auto.conf.b <- gbutton(' Configure auto run ', container=auto.g, handler=configure.auto.run, 
				# action=list(mw=main.win, env=e, cont.run=TRUE))
	# #enabled(e$run.auto) <- FALSE
	# #enabled(e$auto.conf.b) <- FALSE
	# iter.g <- ggroup(horizontal=TRUE, container=mcmc.g)
	# glabel("Number of iterations:", container=iter.g)
	# glabel("<span color='red'>*</span>", markup=TRUE, container=iter.g)
	# e$iter <- gedit(defaults$iter, width=7, container=iter.g)
	# glabel("Chain ids:", container=iter.g)
	# e$chain.ids <- gedit(defaults$chain.ids, width=10, container=iter.g)
	# .enable.auto.cont(FALSE, e)
	.create.process.group(g2, e, defaults, show.buffer.size=FALSE)
	# paral.g <- gframe("<span color='blue'>Process control</span>", markup=TRUE,
					 # horizontal=TRUE, container=g)
	# e$verbose <- gcheckbox("Verbose", checked=defaults$verbose, container=paral.g)
	# addSpace(paral.g, 5)
	# e$parallel <- gcheckbox("Parallel", checked=defaults$parallel, container=paral.g)
	# glabel("  Number of nodes:", container=paral.g)
	# e$nr.nodes <- gedit(defaults$nr.nodes, width=2, container=paral.g)
	addSpace(g, 10)
	.create.status.label(g, e)
	addSpring(g)
	cont.g <- ggroup(horizontal=TRUE, container=g)
	create.help.button(topic='continue.e0.mcmc', package='bayesLife', parent.group=cont.g,
						parent.window=main.win)
	addSpring(cont.g)
	create.generate.script.button(handler=e0mcmc.continue, action=list(mw=main.win, env=e, script=TRUE),
								container=cont.g)
	bDem.gbutton(action=gaction(label=' Continue MCMC ', icon='execute', handler=e0mcmc.continue, 
				action=list(mw=main.win, env=e, script=FALSE)), container=cont.g)
	return(e)

}

e0mcmc.continue <- function(h, ...) {
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
		commands <- paste('m <- continue.e0.mcmc(', paste(paste(names(params), params, sep='='), collapse=', '),
											 ')',sep=' ')
		if(run.auto && e$run.prediction) 
			commands <- paste(commands, '\n\ne0.predict(m, use.diagnostics=TRUE, replace.output=TRUE)', sep='')
		create.script.widget(commands, h$action$mw, package="bayesLife")
	} else {
		m <- .run.simulation(e, handler=get.e0.simulation.status, option='bDem.e0mcmc', 
								call='continue.e0.mcmc', params=params, 
								sim.name='e0 MCMC simulation', main.win=h$action$mw,
								action=list(sb=e$statuslabel, sim.dir=params[['output.dir']]),
								interval=5000)
		if(run.auto && e$run.prediction)
			.run.prediction(e, type='e0', handler=get.e0.prediction.status, option='bDem.e0pred', 
								call='tfr.predict', params=list(m, use.diagnostics=TRUE, replace.output=TRUE), 
								sim.name='e0 prediction', main.win=h$action$mw,
								action=list(sb=e$statuslabel),
								interval=1000)
	}
}

.get.defaults.for.auto.cont.e0 <- function(e) {
	mcmc.set <- get.e0.mcmc(sim.dir=svalue(e$output.dir))
	if(is.null(mcmc.set)) {
		gmessage('Simulation directory contains no valid e0 MCMCs.', title='Input Error',
					icon='error')
		return(NULL)
	}
	return(mcmc.set$meta$auto.conf)
}