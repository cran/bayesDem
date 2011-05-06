e0ContinueMCMCgroup <- function(g, main.win, parent=NULL) {
	e <- new.env()
	defaults <- formals(continue.e0.mcmc) # default argument values

	e$output.dir <- parent$sim.dir
	mcmc.g <- gframe("<span color='blue'>MCMC</span>", markup=TRUE, horizontal=FALSE, cont=g)
	auto.g <- ggroup(horizontal=TRUE, cont=mcmc.g)
	e$run.prediction <- FALSE
	e$run.auto <- gcheckbox("Auto simulation", checked=FALSE, 
							cont=auto.g, handler=function(h,...){.enable.auto.cont(svalue(h$obj), e)})
	e$auto.conf.b <- gbutton(' Configure auto run ', cont=auto.g, handler=configure.auto.run, 
				action=list(mw=main.win, env=e, cont.run=TRUE))
	enabled(e$run.auto) <- FALSE
	enabled(e$auto.conf.b) <- FALSE
	iter.g <- ggroup(horizontal=TRUE, cont=mcmc.g)
	glabel("Number of iterations:", cont=iter.g)
	glabel("<span color='red'>*</span>", markup=TRUE, cont=iter.g)
	e$iter <- gedit(defaults$iter, width=7, cont=iter.g)
	glabel("Chain ids:", cont=iter.g)
	e$chain.ids <- gedit(defaults$chain.ids, width=10, cont=iter.g)
	#.enable.auto.cont(FALSE, e)
	
	paral.g <- gframe("<span color='blue'>Process control</span>", markup=TRUE,
					 horizontal=TRUE, cont=g)
	e$verbose <- gcheckbox("Verbose", checked=defaults$verbose, cont=paral.g)
	addSpace(paral.g, 5)
	e$parallel <- gcheckbox("Parallel", checked=defaults$parallel, cont=paral.g)
	glabel("  Number of nodes:", cont=paral.g)
	e$nr.nodes <- gedit(defaults$nr.nodes, width=2, cont=paral.g)
	
	addSpring(g)
	cont.g <- ggroup(horizontal=TRUE, cont=g)
	create.help.button(topic='continue.e0.mcmc', package='bayesLife', parent.group=cont.g,
						parent.window=main.win)
	addSpring(cont.g)
	gbutton(' Generate Script ', cont=cont.g, handler=e0mcmc.continue,
				action=list(mw=main.win, env=e, script=TRUE))
	gbutton(action=gaction(label=' Continue MCMC ', icon='execute', handler=e0mcmc.continue, 
				action=list(mw=main.win, env=e, script=FALSE)), cont=cont.g)
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
		script.text <- gwindow('bayesLife commands', parent=h$action$mw)
		commands <- paste('m <- continue.e0.mcmc(', paste(paste(names(params), params, sep='='), collapse=', '),
											 ')',sep=' ')
		#if(run.auto && e$run.prediction) 
		#	commands <- paste(commands, '\n\ne0.predict(m, use.diagnostics=TRUE, replace.output=TRUE)', sep='')
		gtext(commands, cont=script.text)
	} else {
		m <- do.call('continue.e0.mcmc', params)
		#if(run.auto && e$run.prediction)
		#	e0.predict(m, use.diagnostics=TRUE, replace.output=TRUE)
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