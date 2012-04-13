TFRrunMCMCgroup <- function(g, main.win, parent) {
	# parent needed for wpp set in the parent function
	e <- new.env()
	e$nb <- gnotebook(container=g, expand=TRUE)
	all.c.g <- ggroup(label="<span color='#0B6138'>All Countries</span>", markup=TRUE, 
						horizontal=FALSE, container=e$nb)
	e$all.c.env <- mcmc.all.countries.group(all.c.g, main.win, parent)
	extra.c.g <- ggroup(label="<span color='#0B6138'>Extra Areas &amp; Regions</span>", 
						markup=TRUE, horizontal=FALSE, container=e$nb)
	e$extra.c.env <- mcmc.extra.countries.group(extra.c.g, main.win, parent)
	svalue(e$nb) <- 1
}

.enable.auto.run <- function(enable, e) {
	enabled(e$auto.conf.b) <- enable
	enabled(e$nr.chains) <- !enable
	enabled(e$iter) <- !enable 
}

mcmc.all.countries.group <- function(g, main.win, parent) {
	e <- new.env()
	defaults <- formals(run.tfr.mcmc) # default argument values

	out.g <- gframe("<span color='blue'>Output settings</span>", markup=TRUE, horizontal=FALSE, container=g)
	#font(e$out.g) <- c(color='blue')
	e$output.dir <- parent$sim.dir				
	
	out.g2 <- ggroup(horizontal=TRUE, container=out.g)
	e$replace.output <- gcheckbox("Overwrite existing results in simulation directory", 
									checked=defaults$replace.output, container=out.g2)
	addSpace(out.g2, 20)
	glabel("Buffer size:", container=out.g2)
	e$buffer.size <- gedit(defaults$buffer.size, width=4, container=out.g2)

	mcmc.g <- gframe("<span color='blue'>MCMC</span>", markup=TRUE, horizontal=FALSE, container=g)
	auto.g <- ggroup(horizontal=TRUE, container=mcmc.g)
	e$run.prediction <- FALSE
	e$run.auto <- gcheckbox("Auto simulation", checked=defaults$iter=='auto', container=auto.g,
							handler=function(h,...){.enable.auto.run(svalue(h$obj), e)})
	e$auto.conf.b <- gbutton(' Configure auto run ', container=auto.g, handler=configure.auto.run, 
				action=list(mw=main.win, env=e, cont.run=FALSE))
	iter.g1 <- ggroup(horizontal=TRUE, container=mcmc.g)
	glabel("Number of chains:", container=iter.g1)
	e$nr.chains <- gedit(defaults$nr.chains, width=2, container=iter.g1)
	glabel("Number of iterations:", container=iter.g1)
	e$iter <- gedit(defaults$iter, width=7, container=iter.g1)
	glabel("Thin:", container=iter.g1)
	e$thin <- gedit(defaults$thin, width=2, container=iter.g1)
	.enable.auto.run(defaults$iter=='auto', e)
	glabel("RNG seed:", container=iter.g1)
	e$seed <- gedit(defaults$seed, width=4, container=iter.g1)
		
	time.g <- gframe("<span color='blue'>TFR time series</span>", markup=TRUE, horizontal=FALSE, container=g)
	time.g1 <- ggroup(horizontal=TRUE, container=time.g)
	glabel("Start year:", container=time.g1)
	e$start.year <- gedit(defaults$start.year, width=4, container=time.g1)
	glabel("     Present year:", container=time.g1)
	e$present.year <- gedit(defaults$present.year, width=4, container=time.g1)
	glabel("     WPP year:", container=time.g1)
	glabel(parent$wpp.year, container=time.g1)
	
	time.g2 <- ggroup(horizontal=TRUE, container=time.g)
	glabel("User-defined TFR file:", container=time.g2)
	e$my.tfr.file <- gfilebrowse(eval(defaults$my.tfr.file), type='open', 
					  width=40, quote=FALSE, container=time.g2)

	paral.g <- gframe("<span color='blue'>Process control</span>", markup=TRUE, horizontal=TRUE, container=g)
	e$verbose <- gcheckbox("Verbose", checked=defaults$verbose, container=paral.g)
	addSpace(paral.g, 10)
	e$parallel <- gcheckbox("Parallel", checked=defaults$parallel, container=paral.g)
	glabel("  Number of nodes:", container=paral.g)
	e$nr.nodes <- gedit(svalue(e$nr.chains), width=2, container=paral.g)

	addSpring(g)
	adv.g <- ggroup(horizontal=TRUE, container=g)
	create.help.button(topic=c('run.tfr.mcmc', 'bayesTFR-package'), package='bayesTFR', parent.group=adv.g,
						parent.window=main.win)
	gbutton('  Advanced Settings  ', container=adv.g, handler=mcmc.advance.settings, 
				action=list(mw=main.win, env=e))
				
	addSpring(adv.g)
	gbutton(' Generate Script ', container=adv.g, handler=mcmc.run,
						action=list(mw=main.win, env=e, script=TRUE, wpp.year=parent$wpp.year))
	gbutton(action=gaction(label=' Run MCMC ', icon='execute', handler=mcmc.run, 
				action=list(mw=main.win, env=e, script=FALSE, wpp.year=parent$wpp.year, parent.group=g)), 
				container=adv.g)
	e$statusbar <- gstatusbar() # don't display now
	return(e)
	}


mcmc.run <- function(h, ...) {
	e <- h$action$env
	if(!has.required.arguments(list(output.dir='Simulation directory'), env=e)) return()
	param.names <- list(numeric=c('buffer.size', 'nr.nodes', 'iter', 'thin', 'nr.chains', 'start.year', 
									'present.year', 'seed'),
						text=c('output.dir', 'my.tfr.file'),
						logical=c('replace.output', 'verbose', 'parallel'))
	params <- get.parameters(param.names, e, quote=h$action$script)
	params[['wpp.year']] <- h$action$wpp.year
	
	run.auto <- svalue(e$run.auto)
	if (run.auto) {
		params[['auto.conf']] <- e$auto.conf
		params[['iter']] <- if (h$action$script) sQuote('auto') else 'auto'
	}
	if (h$action$script) {
		script.text <- gwindow('bayesTFR commands', parent=h$action$mw)
		commands <- paste('m <- run.tfr.mcmc(', paste(paste(names(params), params, sep='='), collapse=', '), ',',
					paste(paste(names(e$params), e$params, sep='='), collapse=', '),
											 ')',sep=' ')
		if(run.auto && e$run.prediction) 
			commands <- paste(commands, '\n\ntfr.predict(m, use.diagnostics=TRUE)', sep='')
		gtext(commands, container=script.text)
	} else {
		if(!params[['replace.output']] & file.exists(params[['output.dir']])) {
			if(length(list.files(params[['output.dir']])) > 0) {
				gmessage(paste('Non-empty directory', params[['output.dir']], 
								'already exists.\nCheck "Overwrite existing results" to delete its content.'))
				return()
			}
		}
		run <- FALSE
		if ((params[['iter']] == 'auto' && ((!is.null(params[['auto.conf']]) 
				&& params[['auto.conf']]$iter > 100) || is.null(params[['auto.conf']]))) 
					|| (params[['iter']] != 'auto' && params[['iter']] > 100)) {
			gconfirm('Running MCMC with these settings can take a very long time. Do you want to continue?',
					icon='question', parent=h$action$mw,
					handler=function(h, ...) run <<- TRUE)
		} else run <- TRUE
		if(run) {
			#add(h$action$mw, e$statusbar)
			#svalue(e$statusbar) <- 'Starting MCMC simulation ... move mouse to update status ...'
			#if(file.exists(params[['output.dir']])) unlink(params[['output.dir']], recursive=TRUE)
			#handler.id <- addHandlerIdle(e$statusbar, interval=1000, handler = get.simulation.status,
			#					action=list(sb=e$statusbar, sim.dir=params[['output.dir']]))
			#handler.id <- addHandlerMouseMotion(h$action$mw, handler = get.simulation.status,
			#					action=list(sb=e$statusbar, sim.dir=params[['output.dir']]))
			m <- do.call('run.tfr.mcmc', c(params, e$params))
			if(run.auto && e$run.prediction)
				tfr.predict(m, use.diagnostics=TRUE)
			#svalue(e$statusbar) <- 'Simulation finished.'
			#removeHandler(h$action$mw, handler.id)
			#gSourceRemove(handler.id)
			#delete(h$action$mw, e$statusbar)
		}
	}
}

get.simulation.status <- function(h, ...) {
	sb <- h$action$sb
	sim.dir <- h$action$sim.dir
	warn <- getOption('warn')
	options(warn=-1) # disable warning messages
	mcmc.set <- get.tfr.mcmc(sim.dir)
	options(warn=warn)
	if(is.null(mcmc.set)) return()
	get.item <- function(x) return(x$finished.iter)
	finished <- sapply(mcmc.set$mcmc.list, get.item)
	svalue(sb) <- paste('Running simulation ... at iteration:', 
				 	paste('chain ', 1:length(finished), ': ', finished, sep='', collapse=', '))
	
}

configure.auto.run <- function(h, ...) {
	cont.run <- h$action$cont.run
	type <- if(is.null(h$action$type)) 'tfr' else h$action$type
	if (!cont.run)
		defaults <- eval(formals(paste("run.", type, ".mcmc", sep=''))$auto.conf)
	else {
		defaults <- do.call(paste('.get.defaults.for.auto.cont.', type, sep=''), list(h$action$env))
		if(is.null(defaults)) defaults <- eval(formals(paste("run.", type, ".mcmc", sep=''))$auto.conf)
	}
	set.defaults <- function(h2, ...) {
		for (par in names(defaults)) {
			svalue(h$action$env$auto.conf.env[[par]]) <- defaults[[par]]
		}
		svalue(h$action$env$auto.conf.env[['run.prediction']]) <- FALSE
	}
	set.auto.conf <- function(h2, ...) {
		params <- get.parameters(list(numvector=names(defaults)), env=h$action$env$auto.conf.env)
		h$action$env$auto.conf <- params
		h$action$env$run.prediction <- svalue(h$action$env$auto.conf.env$run.prediction)
		visible(h$action$env$auto.conf.win) <- FALSE
	}
	
	if (!is.null(h$action$env$auto.conf.win) && !h$action$env$auto.conf.env$window.destroyed) { # window exists
		if(!is.null(h$action$env$auto.conf)) { # OK button previously clicked 
			for (par in names(defaults)) 
				svalue(h$action$env$auto.conf.env[[par]]) <- h$action$env$auto.conf[[par]]
			svalue(h$action$env$auto.conf.env$run.prediction) <- h$action$env$run.prediction
		} else { # OK button not clicked yet, values are set to defaults
			for (par in names(defaults)) 
				svalue(h$action$env$auto.conf.env[[par]]) <- defaults[[par]]
				#svalue(h$action$env$auto.conf.env[[par]]) <- h$action$env$auto.conf.env$defaults[[par]]
			svalue(h$action$env$auto.conf.env[['run.prediction']]) <- FALSE
		}
		visible(h$action$env$auto.conf.win) <- TRUE
	} else { # create the window
		
	h$action$env$auto.conf.win <- auto.conf.win <- 
					gwindow('Configuration of Auto Run',
						parent=h$action$mw, visible=FALSE,
						handler=function(h, ...) {
							h$action$env$auto.run.okhandler <- NULL
						})
	e <- new.env()
	g <- ggroup(container=auto.conf.win, horizontal=FALSE)
	mcmc.g <- gframe("<span color='blue'>MCMC</span>", markup=TRUE, horizontal=FALSE, container=g)
	mcmc.g1 <- ggroup(container=mcmc.g, horizontal=TRUE)
	glabel("Number of iterations:", container=mcmc.g1)
	e$iter <- gedit(defaults$iter, width=7, container=mcmc.g1)
	glabel("Number of chains:", container=mcmc.g1)
	e$nr.chains <- gedit(defaults$nr.chains, width=2, container=mcmc.g1)
	enabled(e$nr.chains) <- !cont.run

	mcmc.g2 <- ggroup(container=mcmc.g, horizontal=TRUE)
	glabel("Iteration increments: ", container=mcmc.g2)
	e$iter.incr <- gedit(defaults$iter.incr, width=7, container=mcmc.g2)
	
	conv.g <- gframe("<span color='blue'>Convergence diagnostics</span>", markup=TRUE, horizontal=TRUE, container=g)
	glabel("Burnin:", container=conv.g)
	e$burnin <- gedit(defaults$burnin, width=7, container=conv.g)
	glabel("Thin:", container=conv.g)
	e$thin <- gedit(defaults$thin, width=7, container=conv.g)
	
	setup.g <- gframe("<span color='blue'>Run setup</span>", markup=TRUE, horizontal=TRUE, container=g)
	glabel("Maximum loops:", container=setup.g)
	e$max.loops <- gedit(defaults$max.loops, width=2, container=setup.g)
	e$run.prediction <- gcheckbox("Make predictions", checked=FALSE, container=setup.g)
	addSpring(g)
	# Buttons
	button.g <- ggroup(container=g, horizontal=TRUE)
	gbutton('Cancel', container=button.g, handler=function(h, ...) 
					visible(auto.conf.win) <- FALSE)
	addSpring(button.g)
	gbutton('  Set to Default Values  ', container=button.g, handler=set.defaults)
	e$auto.conf.okbutton <- gbutton('OK', container=button.g)
	
		if(!is.null(h$action$env$auto.conf)) { # OK button previously clicked 
			for (par in names(defaults)) 
				svalue(e[[par]]) <- h$action$env$auto.conf[[par]]
			svalue(e$run.prediction) <- h$action$env$run.prediction
		}
			
	visible(auto.conf.win) <- TRUE
	e$window.destroyed <- FALSE
	h$action$env$auto.conf.env <- e
	addHandlerDestroy(auto.conf.win, 
							handler=function(h1, ...) h$action$env$auto.conf.env$window.destroyed <- TRUE)

	}
	if(!is.null(h$action$env$auto.conf.okhandler)) 
		removehandler(h$action$env$auto.conf.env$auto.conf.okbutton, h$action$env$auto.conf.okhandler)
	h$action$env$auto.conf.okhandler <- addhandlerclicked(h$action$env$auto.conf.env$auto.conf.okbutton, 
												handler=set.auto.conf)

}

mcmc.advance.settings <- function(h, ...) {
	param.names <- list('Triangle_c4.low', 'Triangle_c4.up', 'Triangle_c4.trans.width', 'Triangle4.0',
						'mean.eps.tau0', 'sd.eps.tau0', 'delta4.0',
						'delta0', 'nu.delta0', 'd.low', 'd.up', 'd.trans.width',
						'dl.p1', 'dl.p2', 'U.c.low', 'U.up', 'U.width', 'S.low', 'S.up', 'S.width', 
						'a.low', 'a.up', 'a.width', 'b.low', 'b.up', 'b.width', 
						'const.low', 'const.up', 'const.width', 
						'sigma0.low', 'sigma0.up', 'sigma0.width', 'sigma0.min',
						'chi0', 'psi0', 'nu.psi0', 'alpha0.p', 'nu4', 'nu.tau0',
						'S.ini', 'a.ini', 'b.ini', 'const.ini', 'gamma.ini', 'sigma0.ini', 'Triangle_c4.ini'
						)
	get.defaults <- function() {
		all.defaults <- formals(run.tfr.mcmc) # default argument values
		defaults <- list()
		for (par in param.names) { 
			defaults[[par]] <- all.defaults[[par]]
			if (length(defaults[[par]]) > 1) {
				if (defaults[[par]][[1]] == '-')  # handle negative values
					defaults[[par]] <- paste(defaults[[par]], sep='', collapse='')
			}
		}
		# special cases
		defaults$b.low <- defaults$a.low
		defaults$b.up <- defaults$a.up
#		defaults$S.ini <- (as.numeric(defaults$S.low)+as.numeric(defaults$S.up))/2
#		defaults$a.ini <- (as.numeric(defaults$a.low)+as.numeric(defaults$a.up))/2
#		defaults$b.ini <- (as.numeric(defaults$b.low)+as.numeric(defaults$b.up))/2
#		defaults$const.ini <- (as.numeric(defaults$const.low)+as.numeric(defaults$const.up))/2
#		defaults$sigma0.ini <- (as.numeric(defaults$sigma0.low)+as.numeric(defaults$sigma0.up))/2
#		defaults$Triangle_c4.ini <- (as.numeric(defaults$Triangle_c4.low)+as.numeric(defaults$Triangle_c4.up))/2
		defaults$alpha0.p <- '-1, 0.5, 1.5'
		return(defaults)
	}

	set.defaults <- function(h2, ...) {
		defaults <- get.defaults()
		for (par in param.names) {
			svalue(h$action$env$adv.set.env[[par]]) <- defaults[[par]]
		}
		for (par in names(linked.pars.list)) svalue(h$action$env$adv.set.env[[par]]) <- defaults[[linked.pars.list[[par]]]]
	}
	
	set.advance.pars <- function(h2, ...) {
		params <- get.parameters(list(numvector=param.names), env=h$action$env$adv.set.env)
		#params <- list()
		#for (par in param.names) {
		#	params[[par]] <- as.numeric(strsplit(svalue(h$action$env$adv.set.env[[par]]), ',')[[1]])
		#	}
		h$action$env$params <- params
		visible(h$action$env$adv.set.win) <- FALSE
	}
		
	if (!is.null(h$action$env$adv.set.win) && !h$action$env$adv.set.env$window.destroyed) { #Advanced Parameters window exists
		if(!is.null(h$action$env$params)) { # OK button previously clicked 
			for (par in param.names) {
				#print (par)
				#print(h$action$env$params[[par]])
				#print(svalue(h$action$env$adv.set.env[[par]]))
				svalue(h$action$env$adv.set.env[[par]]) <- paste(h$action$env$params[[par]], collapse=', ')
			}
		} else { # OK button not clicked yet, values are set to defaults
			for (par in param.names) {
				svalue(h$action$env$adv.set.env[[par]]) <- h$action$env$adv.set.env$defaults[[par]]
			}
		}
		visible(h$action$env$adv.set.win) <- TRUE
	} else { # create the Advanced Parameters window
		h$action$env$adv.set.win <- adv.set.win <- 
					gwindow('Settings for Bayesian Hierarchical TFR Model ',
						parent=h$action$mw, visible=FALSE,
						handler=function(h, ...) {
							h$action$env$adv.set.okhandler <- NULL
						})
		e <- new.env()
		e$defaults <- defaults <- get.defaults()
		e$adv.g <- ggroup(container=adv.set.win, horizontal=FALSE)
	
	linked.pars.list <- list()
	
	modelpars.f <- gframe("<span color='blue'>Model parameters</span>", markup=TRUE, container=e$adv.g, horizontal=TRUE)
	modelpars.flo <- glayout(container=modelpars.f)
	l <- 1 # row 1
	modelpars.flo[l,1] <- ''
	modelpars.flo[l,2] <- '   lower bound   '
	modelpars.flo[l,3] <- '   upper bound   '
	modelpars.flo[l,4] <- 'width (trans)'
		
	l <- l+1 # new row
	#modelpars.flo[l,1] <- glabel('<span>&#916;<sub>c4</sub>:</span>', markup=TRUE, container=modelpars.flo)
	modelpars.flo[l,1] <- glabel('<span>Triangle<sub>c4</sub>:</span>', markup=TRUE, container=modelpars.flo)
	modelpars.flo[l,2] <- e$Triangle_c4.low <- gedit(defaults$Triangle_c4.low, width=5, container=modelpars.flo)
	modelpars.flo[l,3] <- e$Triangle_c4.up <- gedit(defaults$Triangle_c4.up, width=5, container=modelpars.flo)
	modelpars.flo[l,4] <- e$Triangle_c4.trans.width <- gedit(defaults$Triangle_c4.trans.width, width=5, container=modelpars.flo)
	#modelpars.flo[l,5] <- '      '
	
	
	l <- l+1 # new row
	modelpars.flo[l,1] <- 'd:'
	modelpars.flo[l,2] <- e$d.low <- gedit(defaults$d.low, width=5, container=modelpars.flo)
	modelpars.flo[l,3] <- e$d.up <- gedit(defaults$d.up, width=5, container=modelpars.flo)
	modelpars.flo[l,4] <- e$d.trans.width <- gedit(defaults$d.trans.width, width=5, container=modelpars.flo)
	#modelpars.flo[l,5] <- '(trans)'
	
	addSpace(modelpars.f, 30)
	modelpars.g2 <- glayout(container=modelpars.f)
	l <- 1
	modelpars.g2[l,1] <- ''
	l <- l+1
	modelpars.g2[l,1] <- glabel('p1:', container=modelpars.g2)
	modelpars.g2[l,2] <- e$dl.p1 <- gedit(defaults$dl.p1, width=5, container=modelpars.g2)
	#addSpace(modelpars.g2, 10)
	l <- l+1
	modelpars.g2[l,1] <- glabel('p2:', container=modelpars.g2)
	modelpars.g2[l,2] <- e$dl.p2 <- gedit(defaults$dl.p2, width=5, container=modelpars.g2)
	
	# Priors
	priors.f <- gframe("<span color='blue'>Prior distributions</span>", markup=TRUE, container=e$adv.g, horizontal=TRUE)
	
	priors.uniform.f <- gframe("<span  color='#0B6138'>Uniform Priors</span>", markup=TRUE, container=priors.f)
	uniform.flo <- glayout(container=priors.uniform.f)
	
	l <- 1 # row 1
	uniform.flo[l,1] <- ''
	uniform.flo[l,2] <- '   lower   '
	uniform.flo[l,3] <- '   upper   '
	uniform.flo[l,4] <- ' width '
	
	l <- l+1 # new row
	uniform.flo[l,1] <- 'U:'
	uniform.flo[l,2] <- e$U.c.low <- gedit(defaults$U.c.low, width=5, container=uniform.flo)
	uniform.flo[l,3] <- e$U.up <- gedit(defaults$U.up, width=5, container=uniform.flo)
	uniform.flo[l,4] <- e$U.width <- gedit(defaults$U.width, width=5, container=uniform.flo)

	l <- l+1 # new row
	uniform.flo[l,1] <- 'S:'
	uniform.flo[l,2] <- e$S.low <- gedit(defaults$S.low, width=5, container=uniform.flo)
	uniform.flo[l,3] <- e$S.up <- gedit(defaults$S.up, width=5, container=uniform.flo)
	uniform.flo[l,4] <- e$S.width <- gedit(defaults$S.width, width=5, container=uniform.flo)
	
	l <- l+1 # new row
	uniform.flo[l,1] <- 'a:'
	uniform.flo[l,2] <- e$a.low <- gedit(defaults$a.low, width=5, container=uniform.flo)
	uniform.flo[l,3] <- e$a.up <- gedit(defaults$a.up, width=5, container=uniform.flo)
	uniform.flo[l,4] <- e$a.width <- gedit(defaults$a.width, width=5, container=uniform.flo)

	l <- l+1 # new row
	uniform.flo[l,1] <- 'b:'
	uniform.flo[l,2] <- e$b.low <- gedit(defaults$b.low, width=5, container=uniform.flo)
	uniform.flo[l,3] <- e$b.up <- gedit(defaults$b.up, width=5, container=uniform.flo)
	uniform.flo[l,4] <- e$b.width <- gedit(defaults$b.width, width=5, container=uniform.flo)

	l <- l+1 # new row
	uniform.flo[l,1] <- glabel('<span>sigma<sub>0</sub>:</span>', markup=TRUE, container=uniform.flo)
	uniform.flo[l,2] <- e$sigma0.low <- gedit(defaults$sigma0.low, width=5, container=uniform.flo)
	uniform.flo[l,3] <- e$sigma0.up <- gedit(defaults$sigma0.up, width=5, container=uniform.flo)
	uniform.flo[l,4] <- e$sigma0.width <- gedit(defaults$sigma0.width, width=5, container=uniform.flo)
	uniform.flo[l,5] <- ' min:'
	uniform.flo[l,6] <- e$sigma0.min <- gedit(defaults$sigma0.min, width=5, container=uniform.flo)

	l <- l+1 # new row
	uniform.flo[l,1] <- 'const (c):'
	uniform.flo[l,2] <- e$const.low <- gedit(defaults$const.low, width=5, container=uniform.flo)
	uniform.flo[l,3] <- e$const.up <- gedit(defaults$const.up, width=5, container=uniform.flo)
	uniform.flo[l,4] <- e$const.width <- gedit(defaults$const.width, width=5, container=uniform.flo)
	
	normal.gamma.g <- ggroup(horizontal=TRUE, container=priors.f)
	priors.normal.f <- gframe("<span  color='#0B6138'>Normal Priors</span>", markup=TRUE, container=normal.gamma.g,
								horizontal=FALSE)
	normal.flo <- glayout(container=priors.normal.f)
	
	l <- 1 # row 1
	normal.flo[l,1] <- ''
	normal.flo[l,2] <- ' mean '
	normal.flo[l,3] <- ' sd '
		
	l <- l+1 # new row
	normal.flo[l,1] <- glabel('<span>chi:</span>', markup=TRUE, container=normal.flo)
	normal.flo[l,2] <- e$chi0 <- gedit(defaults$chi0, width=5, container=normal.flo)
	normal.flo[l,3] <- e$l.psi0 <- glabel(width=5, container=normal.flo)

	l <- l+1 # new row
	normal.flo[l,1] <- glabel('<span>alpha<sub>i</sub>:</span>', markup=TRUE, container=normal.flo)
	normal.flo[l,2] <- e$alpha0.p <- gedit(defaults$alpha0.p, width=8, container=normal.flo)
	normal.flo[l,3] <- e$l.delta0 <- glabel(width=5, container=normal.flo)
	
	l <- l+1 # new row
	normal.flo[l,1] <- glabel('<span>Triangle<sub>4</sub>:</span>', markup=TRUE, container=normal.flo)
	normal.flo[l,2] <- e$Triangle4.0 <- gedit(defaults$Triangle4.0, width=5, container=normal.flo)
	normal.flo[l,3] <- e$l.delta4.0 <- glabel(width=5, container=normal.flo)
	
	l <- l+1 # new row
	normal.flo[l,1] <- glabel('<span>m<sub>tau</sub>:</span>', markup=TRUE, container=normal.flo)
	normal.flo[l,2] <- e$mean.eps.tau0 <- gedit(defaults$mean.eps.tau0, width=5, container=normal.flo)
	normal.flo[l,3] <- e$l.sd.eps.tau0 <- glabel(width=5, container=normal.flo)

	addSpace(priors.normal.f, 10)
	normal.g2 <- ggroup(container=priors.normal.f)
	glabel('sd given by s_0 in Gamma prior', container=normal.g2)

	priors.gamma.f <- gframe("<span  color='#0B6138'>Gamma Priors</span>", markup=TRUE, container=priors.f,
								horizontal=FALSE)
	gamma.flo <- glayout(container=priors.gamma.f)
	
	l <- 1 # row 1
	gamma.flo[l,1] <- ''
	gamma.flo[l,2] <- ' nu_0 '
	gamma.flo[l,3] <- ' s_0  '

	l <- l+1 # new row
	gamma.flo[l,1] <- glabel('<span>1/psi<sup>2</sup>:</span>', markup=TRUE, container=gamma.flo)
	gamma.flo[l,2] <- e$nu.psi0 <- gedit(defaults$nu.psi0, width=5, container=gamma.flo)
	gamma.flo[l,3] <- e$psi0 <- gedit(defaults$psi0, width=5, container=normal.flo)
	svalue(e$l.psi0) <- svalue(e$psi0)
	addHandlerChanged(e$psi0, handler=function(h,...) svalue(e$l.psi0) <- svalue(e$psi0))
	linked.pars.list[['l.psi0']] <- 'psi0'
	
	l <- l+1 # new row
	gamma.flo[l,1] <- glabel('<span>1/delta<sub>i</sub><sup>2</sup>:</span>', markup=TRUE, container=gamma.flo)
	gamma.flo[l,2] <- e$nu.delta0 <- gedit(defaults$nu.delta0, width=5, container=gamma.flo)
	gamma.flo[l,3] <- e$delta0 <- gedit(defaults$delta0, width=5, container=gamma.flo)
	svalue(e$l.delta0) <- svalue(e$delta0)
	addHandlerChanged(e$delta0, handler=function(h,...) svalue(e$l.delta0) <- svalue(e$delta0))
	linked.pars.list[['l.delta0']] <- 'delta0'
	
	l <- l+1 # new row
	gamma.flo[l,1] <- glabel('<span>1/delta<sub>4</sub><sup>2</sup>:</span>', markup=TRUE, container=gamma.flo)
	gamma.flo[l,2] <- e$nu4 <- gedit(defaults$nu4, width=5, container=gamma.flo)
	gamma.flo[l,3] <- e$delta4.0 <- gedit(defaults$delta4.0, width=5, container=gamma.flo)
	svalue(e$l.delta4.0) <- svalue(e$delta4.0)
	addHandlerChanged(e$delta4.0, handler=function(h,...) svalue(e$l.delta4.0) <- svalue(e$delta4.0))
	linked.pars.list[['l.delta4.0']] <- 'delta4.0'
	
	l <- l+1 # new row
	gamma.flo[l,1] <- glabel('<span>1/s<sub>tau</sub><sup>2</sup>:</span>', markup=TRUE, container=gamma.flo)
	gamma.flo[l,2] <- e$nu.tau0 <- gedit(defaults$nu.tau0, width=5, container=gamma.flo)
	gamma.flo[l,3] <- e$sd.eps.tau0 <- gedit(defaults$sd.eps.tau0, width=5, container=gamma.flo)
	svalue(e$l.sd.eps.tau0) <- svalue(e$sd.eps.tau0)
	addHandlerChanged(e$sd.eps.tau0, handler=function(h,...) svalue(e$l.sd.eps.tau0) <- svalue(e$sd.eps.tau0))
	linked.pars.list[['l.sd.eps.tau0']] <- 'sd.eps.tau0'
	
	addSpace(priors.gamma.f, 10)
	gamma.g2 <- glayout(container=priors.gamma.f, horizontal=FALSE)
	gamma.g2[1,1, expand=FALSE, anchor=c(-1,0)] <- glabel('Gamma shape: nu_0/2', container=gamma.g2)
	gamma.g2[2,1, expand=FALSE] <- glabel('Gamma rate: nu_0/2*s_0^2', container=gamma.g2)
	
	# Starting values
	start.f <- gframe("<span color='blue'>Starting Values</span>", markup=TRUE, container=e$adv.g, horizontal=FALSE)
	start.g1 <- ggroup(container=start.f, horizontal=TRUE)
	glabel('Leave empty for starting values being equally-spaced between lower and upper bound.', container=start.g1)
	
	start.flo <- ggroup(container=start.f, horizontal=TRUE)
	glabel('S:', container=start.flo)
	e$S.ini <- gedit(defaults$S.ini, width=5, container=start.flo)
	glabel('a:', container=start.flo)
	e$a.ini <- gedit(defaults$a.ini, width=5, container=start.flo)
	glabel('b:', container=start.flo)
	e$b.ini <- gedit(defaults$b.ini, width=5, container=start.flo)
	glabel('<span>sigma<sub>0</sub>:</span>', markup=TRUE, container=start.flo)
	e$sigma0.ini <- gedit(defaults$sigma0.ini, width=5, container=start.flo)
	glabel('const (c):', container=start.flo)
	e$const.ini <- gedit(defaults$const.ini, width=5, container=start.flo)
	glabel('<span>Triangle<sub>c4</sub>:</span>', markup=TRUE, container=start.flo)
	e$Triangle_c4.ini <- gedit(defaults$Triangle_c4.ini, width=5, container=start.flo)
	addSpace(start.flo, 20)
	glabel('gamma:', container=start.flo)
	e$gamma.ini <- gedit(defaults$gamma.ini, width=5, container=start.flo)

	# Buttons
	button.g <- ggroup(container=e$adv.g, horizontal=TRUE)
	gbutton('Cancel', container=button.g, handler=function(h, ...) 
					visible(adv.set.win) <- FALSE)
	addSpring(button.g)
	e$adv.set.defaultbutton <- gbutton('  Set to Default Values  ', container=button.g, handler=set.defaults)
	e$adv.set.okbutton <- gbutton('OK', container=button.g)
	
		if(!is.null(h$action$env$params)) { # OK button previously clicked 
			for (par in param.names) 
				svalue(e[[par]]) <- paste(h$action$env$params[[par]], collapse=', ')
			
		}
		
	visible(adv.set.win) <- TRUE
	e$window.destroyed <- FALSE
	h$action$env$adv.set.env <- e
	
	addHandlerDestroy(adv.set.win, 
					handler=function(h1, ...) h$action$env$adv.set.env$window.destroyed <- TRUE)

	}
	if(!is.null(h$action$env$adv.set.okhandler)) 
		removehandler(h$action$env$adv.set.env$adv.set.okbutton, h$action$env$adv.set.okhandler)
	h$action$env$adv.set.okhandler <- addhandlerclicked(h$action$env$adv.set.env$adv.set.okbutton, 
												handler=set.advance.pars)
}

mcmc.extra.countries.group <- function(g, main.win, parent) {
	e <- new.env()
	defaults <- formals(run.tfr.mcmc.extra) # default argument values
	e$sim.dir <- parent$sim.dir
	
	e$tfr.g <- gframe("<span color='blue'>TFR time series</span>", markup=TRUE, horizontal=FALSE, container=g)
	e$tfr.g1 <- ggroup(horizontal=TRUE, container=e$tfr.g)
	#glabel('WPP', container=e$tfr.g1)
	#wpps <- get.wpp.years()
	#e$wpp.year <- gdroplist(wpps, container=e$tfr.g1)
	gbutton("  Select countries/regions from the UN TFR-file  ", container=e$tfr.g1,
				handler=multiSelectCountryMenu,
				action=list(mw=main.win, env=e))
	
	e$tfr.g2 <- ggroup(horizontal=TRUE, container=e$tfr.g)
	glabel("User-defined TFR-file:", container=e$tfr.g2)
	e$my.tfr.file <- gfilebrowse(eval(defaults$my.tfr.file), type='open', 
					  width=40, quote=FALSE, container=e$tfr.g2)
					  
	e$iter.g <- gframe("<span color='blue'>MCMC</span>", markup=TRUE, horizontal=TRUE, container=g)
	glabel("Number of iterations:", container=e$iter.g)
	e$iter <- gedit(defaults$iter, width=7, container=e$iter.g)
	glabel("Thin:", container=e$iter.g)
	e$thin <- gedit(defaults$thin, width=2, container=e$iter.g)
	glabel("Burnin:", container=e$iter.g)
	e$burnin <- gedit(defaults$burnin, width=4, container=e$iter.g)
					  					  
	e$paral.g <- gframe("<span color='blue'>Process control</span>", markup=TRUE, horizontal=TRUE, container=g)
	e$verbose <- gcheckbox("Verbose", checked=defaults$verbose, container=e$paral.g)
	addSpace(e$paral.g, 10)
	e$parallel <- gcheckbox("Parallel", checked=defaults$parallel, container=e$paral.g)
	glabel("  Number of nodes:", container=e$paral.g)
	e$nr.nodes <- gedit(defaults$nr.nodes, width=2, container=e$paral.g)
	
	addSpring(g)
	e$button.g <- ggroup(horizontal=TRUE, container=g)
	create.help.button(topic='run.tfr.mcmc.extra', package='bayesTFR', parent.group=e$button.g,
						parent.window=main.win)	
	addSpring(e$button.g)
	gbutton(' Generate Script ', container=e$button.g, handler=mcmc.run.extra,
						action=list(mw=main.win, env=e, script=TRUE))
	gbutton(action=gaction(label=' Run MCMC ', icon='execute', handler=mcmc.run.extra, 
				action=list(mw=main.win, env=e, script=FALSE)), container=e$button.g)

	return(e)
}

mcmc.run.extra <- function(h, ...) {
	e <- h$action$env
	if(!has.required.arguments(list(sim.dir='Simulation directory'), env=e)) return()
	param.names <- list(numeric=c('nr.nodes', 'iter', 'thin', 'burnin'),
						text=c('sim.dir', 'my.tfr.file'),
						logical=c('verbose', 'parallel'))
	params <- get.parameters(param.names, e, quote=h$action$script)
	params[['countries']] <- e$selected.extra.countries
	if (h$action$script) {
		script.text <- gwindow('bayesTFR commands', parent=h$action$mw)
		gtext(paste('run.tfr.mcmc.extra(', paste(paste(names(params), params, sep='='), collapse=', '), ',',
											 ')',sep=' '), 
					container=script.text)
	} else {
		do.call('run.tfr.mcmc.extra', params)
	}
}

get.wpp.years <- function() {
	pkg.data.dir <- file.path(.find.package("bayesTFR"), "data")
	files <- list.files(pkg.data.dir)
	#filter UNx.txt files
	un.files <- grep('^UN[0-9]{4}.txt$', files, value=TRUE)
	years <- as.integer(substr(un.files, 3,6))
	return(years)
}

get.table.of.countries.from.locfile <- function(sim.dir, sorted=TRUE, type='tfr') {
	mcmc.set <- do.call(paste('get.', type, '.mcmc', sep=''), list(sim.dir=sim.dir))
	if(is.null(mcmc.set)) {
		gmessage('Simulation directory contains no valid MCMC results.', title='Input Error',
					icon='error')
		return(NULL)
	}
	wpp.year <- mcmc.set$meta$wpp.year
	
	path <- get.data.path(type)
	# get the UN TFR-file
	tfr.data <- do.call(paste('get.', type, '.UN.data', sep=''), list(type=type, mcmc.set=mcmc.set))
	codes <- tfr.data[,'country_code']
	# filter out countries used already for an estimation
	used.codes <- mcmc.set$meta$regions$country_code[1:(bayesTFR:::get.nr.countries.est(mcmc.set$meta))]
	codes <- codes[!is.element(codes, used.codes)] # codes not used in the estimation
			
	# get the UN location file
	loc.file.name <- file.path(path, paste('WPP', wpp.year, '_LOCATIONS', '.txt', sep=''))
	loc.data <- read.tfr.file(file=loc.file.name)
	loc.data <- loc.data[,c("country_code", "Major area, region, country or area")]
	colnames(loc.data) <- c('code', 'name')
	
	#include only those that are contained in codes
	loc.data <- loc.data[is.element(loc.data[,'code'], codes),]
	loc.data[,'name'] <- gsub(' +$', '', loc.data[,'name']) # remove trailing space
	loc.data[,'name'] <- gsub('^ +', '', loc.data[,'name']) # remove spaces at the beginning
	if(sorted) {
		ord.idx <- order(loc.data[,'name'])
		loc.data <- loc.data[ord.idx,]
	}
	return(loc.data)
}

multiSelectCountryMenu <- function(h, ...) {
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
												sorted=FALSE, 
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
		country.table <- get.table.of.countries.from.locfile(sim.dir=sim.dir.used, sorted=FALSE,
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
					expand=TRUE, multiple=TRUE, handler=country.selected)
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