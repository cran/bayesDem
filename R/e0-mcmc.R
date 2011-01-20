e0RunMCMCgroup <- function(g, main.win, parent) {
	# parent needed for wpp set in the parent function
	e <- new.env()
	nb <- gnotebook(cont=g, expand=TRUE)
	all.c.g <- ggroup(label="<span color='#0B6138'>All Countries</span>", markup=TRUE, 
						horizontal=FALSE, cont=nb)
	all.c.env <- e0mcmc.all.countries.group(all.c.g, main.win, parent)
	extra.c.g <- ggroup(label="<span color='#0B6138'>Extra Areas &amp; Regions</span>", 
						markup=TRUE, horizontal=FALSE, cont=nb)
	extra.c.env <- e0mcmc.extra.countries.group(extra.c.g, main.win, parent)
	svalue(nb) <- 1
}

e0mcmc.all.countries.group <- function(g, main.win, parent) {
	e <- new.env()
	defaults <- formals(run.e0.mcmc) # default argument values

	out.g <- gframe("<span color='blue'>Output settings</span>", markup=TRUE, horizontal=FALSE, cont=g)
	e$output.dir <- parent$sim.dir				
	
	out.g2 <- ggroup(horizontal=TRUE, cont=out.g)
	e$replace.output <- gcheckbox("Overwrite existing results in simulation directory", 
									checked=defaults$replace.output, cont=out.g2)
	addSpace(out.g2, 20)
	glabel("Buffer size:", cont=out.g2)
	e$buffer.size <- gedit(defaults$buffer.size, width=4, cont=out.g2)

	mcmc.g <- gframe("<span color='blue'>MCMC</span>", markup=TRUE, horizontal=FALSE, cont=g)
	auto.g <- ggroup(horizontal=TRUE, cont=mcmc.g)
	e$run.prediction <- FALSE
	e$run.auto <- gcheckbox("Auto simulation", checked=defaults$iter=='auto', cont=auto.g,
							handler=function(h,...){.enable.auto.run(svalue(h$obj), e)})
	e$auto.conf.b <- gbutton(' Configure auto run ', cont=auto.g, handler=configure.auto.run, 
				action=list(mw=main.win, env=e, cont.run=FALSE))
	enabled(e$run.auto) <- FALSE
	enabled(e$auto.conf.b) <- FALSE
	iter.g1 <- ggroup(horizontal=TRUE, cont=mcmc.g)
	glabel("Number of chains:", cont=iter.g1)
	e$nr.chains <- gedit(defaults$nr.chains, width=2, cont=iter.g1)
	glabel("Number of iterations:", cont=iter.g1)
	e$iter <- gedit(defaults$iter, width=7, cont=iter.g1)
	glabel("Thin:", cont=iter.g1)
	e$thin <- gedit(defaults$thin, width=2, cont=iter.g1)
	#.enable.auto.run(defaults$iter=='auto', e)
	glabel("RNG seed:", cont=iter.g1)
	e$seed <- gedit(defaults$seed, width=4, cont=iter.g1)
		
	time.g <- gframe("<span color='blue'>e0 time series</span>", markup=TRUE, horizontal=FALSE, cont=g)
	time.g1 <- ggroup(horizontal=TRUE, cont=time.g)
	glabel("Gender:", cont=time.g1)
	e$gender <- gdroplist(c('Female', 'Male'), cont=time.g1, selected=2)
	addSpace(time.g1, 10)
	glabel("Start year:", cont=time.g1)
	e$start.year <- gedit(defaults$start.year, width=4, cont=time.g1)
	glabel("     Present year:", cont=time.g1)
	e$present.year <- gedit(defaults$present.year, width=4, cont=time.g1)
	glabel("     WPP year:", cont=time.g1)
	glabel(parent$wpp.year, cont=time.g1)
	
	time.g2 <- ggroup(horizontal=TRUE, cont=time.g)
	glabel("User-defined e0 file:", cont=time.g2)
	e$my.e0.file <- gfilebrowse(eval(defaults$my.tfr.file), type='open', 
					  width=40, quote=FALSE, cont=time.g2)
	
	paral.g <- gframe("<span color='blue'>Process control</span>", markup=TRUE, horizontal=TRUE, cont=g)
	e$verbose <- gcheckbox("Verbose", checked=defaults$verbose, cont=paral.g)
	addSpace(paral.g, 10)
	e$parallel <- gcheckbox("Parallel", checked=defaults$parallel, cont=paral.g)
	glabel("  Number of nodes:", cont=paral.g)
	e$nr.nodes <- gedit(svalue(e$nr.chains), width=2, cont=paral.g)

	addSpring(g)
	adv.g <- ggroup(horizontal=TRUE, cont=g)
	create.help.button(topic='run.e0.mcmc', package='bayesLife', parent.group=adv.g,
						parent.window=main.win)
	gbutton('  Advanced Settings  ', cont=adv.g, handler=e0mcmc.advance.settings, 
				action=list(mw=main.win, env=e))
				
	addSpring(adv.g)
	gbutton(' Generate Script ', cont=adv.g, handler=e0mcmc.run,
						action=list(mw=main.win, env=e, script=TRUE, wpp.year=parent$wpp.year))
	gbutton(action=gaction(label=' Run MCMC ', icon='execute', handler=e0mcmc.run, 
				action=list(mw=main.win, env=e, script=FALSE, wpp.year=parent$wpp.year, parent.group=g)), 
				cont=adv.g)
	#e$statusbar <- gstatusbar() # don't display now
	return(e)
	}

e0mcmc.extra.countries.group <- function(g, main.win, parent) {
	e <- new.env()
	defaults <- formals(run.e0.mcmc.extra) # default argument values
	e$sim.dir <- parent$sim.dir
	
	e$ts.g <- gframe("<span color='blue'>e0 time series</span>", markup=TRUE, horizontal=FALSE, cont=g)
	e$ts.g1 <- ggroup(horizontal=TRUE, cont=e$ts.g)
	gbutton("  Select countries/regions from the UN e0-file  ", cont=e$ts.g1,
				handler=multiSelectCountryMenu,
				action=list(mw=main.win, env=e, type='e0'))
	
	e$ts.g2 <- ggroup(horizontal=TRUE, cont=e$ts.g)
	glabel("User-defined e0-file:", cont=e$ts.g2)
	e$my.e0.file <- gfilebrowse(eval(defaults$my.e0.file), type='open', 
					  width=40, quote=FALSE, cont=e$ts.g2)
					  
	e$iter.g <- gframe("<span color='blue'>MCMC</span>", markup=TRUE, horizontal=TRUE, cont=g)
	glabel("Number of iterations:", cont=e$iter.g)
	e$iter <- gedit(defaults$iter, width=7, cont=e$iter.g)
	glabel("Thin:", cont=e$iter.g)
	e$thin <- gedit(defaults$thin, width=2, cont=e$iter.g)
	glabel("Burnin:", cont=e$iter.g)
	e$burnin <- gedit(defaults$burnin, width=4, cont=e$iter.g)
					  					  
	e$paral.g <- gframe("<span color='blue'>Process control</span>", markup=TRUE, horizontal=TRUE, cont=g)
	e$verbose <- gcheckbox("Verbose", checked=defaults$verbose, cont=e$paral.g)
	addSpace(e$paral.g, 10)
	e$parallel <- gcheckbox("Parallel", checked=defaults$parallel, cont=e$paral.g)
	glabel("  Number of nodes:", cont=e$paral.g)
	e$nr.nodes <- gedit(defaults$nr.nodes, width=2, cont=e$paral.g)
	
	addSpring(g)
	e$button.g <- ggroup(horizontal=TRUE, cont=g)
	create.help.button(topic='run.e0.mcmc.extra', package='bayesLife', parent.group=e$button.g,
						parent.window=main.win)	
	addSpring(e$button.g)
	gbutton(' Generate Script ', cont=e$button.g, handler=e0mcmc.run.extra,
						action=list(mw=main.win, env=e, script=TRUE))
	gbutton(action=gaction(label=' Run MCMC ', icon='execute', handler=e0mcmc.run.extra, 
				action=list(mw=main.win, env=e, script=FALSE)), cont=e$button.g)

	return(e)
}

e0mcmc.run <- function(h, ...) {
	e <- h$action$env
	if(!has.required.arguments(list(output.dir='Simulation directory'), env=e)) return()
	param.names <- list(numeric=c('buffer.size', 'nr.nodes', 'iter', 'thin', 'nr.chains', 'start.year', 
									'present.year', 'seed'),
						text=c('output.dir', 'my.e0.file',
								'gender'),
						logical=c('replace.output', 'verbose', 'parallel'))
	params <- get.parameters(param.names, e, quote=h$action$script)
	params[['wpp.year']] <- h$action$wpp.year
	params[['gender']] <- if(h$action$script) sQuote(substr(params[['gender']], 2, 2)) else substr(params[['gender']], 1, 1)
	run.auto <- svalue(e$run.auto)
	if (run.auto) {
		params[['auto.conf']] <- e$auto.conf
		params[['iter']] <- if (h$action$script) sQuote('auto') else 'auto'
	}
	if (h$action$script) {
		script.text <- gwindow('bayesLife commands', parent=h$action$mw)
		commands <- paste('m <- run.e0.mcmc(', paste(paste(names(params), params, sep='='), collapse=', '), ',',
					paste(paste(names(e$params), e$params, sep='='), collapse=', '),
											 ')',sep=' ')
		#if(run.auto && e$run.prediction) 
		#	commands <- paste(commands, '\n\ne0.predict(m, use.diagnostics=TRUE)', sep='')
		gtext(commands, cont=script.text)
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
				&& params[['auto.conf']]$iter > 5000) || is.null(params[['auto.conf']]))) 
					|| (params[['iter']] != 'auto' && params[['iter']] > 5000)) {
			gconfirm('Running MCMC with these settings can take a very long time. Do you want to continue?',
					icon='question', parent=h$action$mw,
					handler=function(h, ...) run <<- TRUE)
		} else run <- TRUE
		if(run) {
			m <- do.call('run.e0.mcmc', c(params, e$params))
			#if(run.auto && e$run.prediction)
			#	e0.predict(m, use.diagnostics=TRUE)
		}
	}
}

e0mcmc.run.extra <- function(h, ...) {
	e <- h$action$env
	if(!has.required.arguments(list(sim.dir='Simulation directory'), env=e)) return()
	param.names <- list(numeric=c('nr.nodes', 'iter', 'thin', 'burnin'),
						text=c('sim.dir', 'my.e0.file'),
						logical=c('verbose', 'parallel'))
	params <- get.parameters(param.names, e, quote=h$action$script)
	params[['countries']] <- e$selected.extra.countries
	if (h$action$script) {
		script.text <- gwindow('bayesLife commands', parent=h$action$mw)
		gtext(paste('run.e0.mcmc.extra(', paste(paste(names(params), params, sep='='), collapse=', '), ',',
											 ')',sep=' '), 
					cont=script.text)
	} else {
		do.call('run.e0.mcmc.extra', params)
	}
}

e0mcmc.advance.settings <- function(h, ...) {
	param.names <- list('a', 'delta', 'tau', 'Triangle.ini', 'k.ini', 'z.ini',
					'lambda.ini', 'lambda.k.ini', 'lambda.z.ini', 'omega.ini',
					'Triangle.ini.low', 'Triangle.ini.up', 
					'k.ini.low', 'z.ini.low', 'k.ini.up', 'z.ini.up',
					'lambda.ini.low', 'lambda.k.ini.low', 'lambda.z.ini.low', 
					'lambda.ini.up', 'lambda.k.ini.up', 'lambda.z.ini.up',
					'omega.ini.low', 'omega.ini.up',
					'Triangle.c.width', 'k.c.width', 'z.c.width', 'nu', 'dl.p1', 'dl.p2'
						)
	get.defaults <- function() {
		all.defaults <- formals(run.e0.mcmc) # default argument values
		defaults <- list()
		for (par in param.names) { 
			defaults[[par]] <- eval(all.defaults[[par]])
		}
		# special cases
		defaults$Triangle.c.ini.norm <- list(round(defaults$Triangle.ini.low + 
			(defaults$Triangle.ini.up - defaults$Triangle.ini.low)/2),c(2,2,2,2))
		defaults$k.c.ini.norm <- c(round(defaults$k.ini.low + (defaults$k.ini.up - defaults$k.ini.low)/2),2)
		defaults$z.c.ini.norm <- c(round(defaults$z.ini.low + (defaults$z.ini.up - defaults$z.ini.low)/2, 2), 0.2)
		return(defaults)
	}

	set.defaults <- function(h2, ...) {
		defaults <- get.defaults()
		for (par in param.names) {
			if(!is.null(h$action$env$adv.set.env[[par]])) {
			if(length(defaults[[par]])>1) {
				for (i in 1:length(defaults[[par]])) svalue(h$action$env$adv.set.env[[par]][[i]]) <- defaults[[par]][i]
			} else 
				svalue(h$action$env$adv.set.env[[par]]) <- defaults[[par]]
			}
		}
		widget.defaults <- h$action$env$adv.set.env$widget.defaults
		for (par in names(widget.defaults)) {
			if(length(widget.defaults[[par]])>1) {
				for (i in 1:length(widget.defaults[[par]])) svalue(h$action$env$adv.set.env[[par]][[i]]) <- widget.defaults[[par]][i]
			} else svalue(h$action$env$adv.set.env[[par]]) <- widget.defaults[[par]]
		}
	}
	
	set.advance.pars <- function(h2, ...) {
		params <- widget.value <- list()		
		array.pars <- c('a', 'delta', 'tau')
		for (i in 1:6) {
			for(par in array.pars) {
				value <- svalue(h$action$env$adv.set.env[[par]][[i]])
				params[[par]] <- c(params[[par]], as.numeric(value))
				widget.value <- c(widget.value, list(c(h$action$env$adv.set.env[[par]][[i]], value)))
			}
		}
		linked.pars.list <- h$action$env$adv.set.env$linked.pars.list
		this.par.list <- linked.pars.list
		for(par in names(linked.pars.list)) {
			if(is.list(linked.pars.list[[par]])) {
				for(i in 1:length(linked.pars.list[[par]])) {
					value <- svalue(linked.pars.list[[par]][[i]])
					params[[par]] <- c(params[[par]], if(nchar(value)==0) NULL else as.numeric(value))
					widget.value <- c(widget.value, list(c(linked.pars.list[[par]][[i]], value)))
				} 
			} else {
				value <- svalue(linked.pars.list[[par]])
				params[[par]] <- if(nchar(value)==0) NULL else as.numeric(value)
				widget.value <- c(widget.value, list(c(linked.pars.list[[par]], value)))
			}
		}
		linked.pars.tuple <- h$action$env$adv.set.env$linked.pars.tuple
		this.par.tuple <- linked.pars.tuple
		for(par in names(linked.pars.tuple)) {
			for(item in 1:2) {
				if(is.list(linked.pars.tuple[[par]][[item]])) {
					if(item==1) params[[par]] <- list()
					params[[par]][[item]] <- rep(NA, length(linked.pars.tuple[[par]][[item]]))
					for(i in 1:length(linked.pars.tuple[[par]][[item]])) {
						value <- svalue(linked.pars.tuple[[par]][[item]][[i]])
						params[[par]][[item]][i] <-if(nchar(value)==0) NULL else as.numeric(value)
						widget.value <- c(widget.value, list(c(linked.pars.tuple[[par]][[item]][[i]], value)))
					} 
				} else {
					if(item==1) params[[par]] <- c()
					value <- svalue(linked.pars.tuple[[par]][[item]])
					params[[par]][item] <- if(nchar(value)==0) NULL else as.numeric(value)
					widget.value <- c(widget.value, list(c(linked.pars.tuple[[par]][[item]], value)))
				}
			}
		}
		rest.par.names <- setdiff(param.names, 
								union(union(array.pars, names(linked.pars.list)), names(linked.pars.tuple)))
		params <- c(params, get.parameters(list(numvector=rest.par.names), env=h$action$env$adv.set.env))
		for (par in rest.par.names) widget.value <- c(widget.value, list(c(h$action$env$adv.set.env[[par]], 
				if(is.null(params[[par]])) '' else params[[par]])))
		h$action$env$params <- params
		h$action$env$adv.set.env$widget.value.pairs <- widget.value
		visible(h$action$env$adv.set.win) <- FALSE
	}
	
	
	if (!is.null(h$action$env$adv.set.win)) { #Advanced Parameters window exists
		if(!is.null(h$action$env$params)) { # OK button previously clicked 
			for (i in 1:length(h$action$env$adv.set.env$widget.value.pairs)) 
				svalue(h$action$env$adv.set.env$widget.value.pairs[[i]][[1]]) <- h$action$env$adv.set.env$widget.value.pairs[[i]][[2]]
		} else { # OK button not clicked yet, values are set to defaults
			#for (par in param.names) {
			#	svalue(h$action$env$adv.set.env[[par]]) <- h$action$env$adv.set.env$defaults[[par]]
			#}
			set.defaults(h)
		}
		visible(h$action$env$adv.set.win) <- TRUE
	} else { # create the Advanced Parameters window
		h$action$env$adv.set.win <- adv.set.win <- gwindow('Settings for Bayesian Hierarchical Model of Life Expectancy',
						parent=h$action$mw, visible=FALSE,
						handler=function(h, ...) {
							h$action$env$adv.set.okhandler <- NULL
						})
		e <- new.env()
		e$defaults <- defaults <- get.defaults()
		e$adv.g <- ggroup(cont=adv.set.win, horizontal=FALSE)
	
	linked.pars.list <- linked.pars.tuple <- widget.defaults <- list()

	priors.f <- gframe("<span color='blue'>Prior parameters and initial values</span>", markup=TRUE, cont=e$adv.g, horizontal=FALSE)
	normal.g <- ggroup(horizontal=TRUE, cont=priors.f)
	priors.normal.f <- gframe("<span  color='#0B6138'>Normal priors</span>", markup=TRUE, cont=normal.g,
								horizontal=FALSE)
	normal.flo <- glayout(cont=priors.normal.f)
	
	l <- 1 # row 1
	normal.flo[l,1] <- ''
	normal.flo[l,2] <- 'prior mean (a)'
	normal.flo[l,3] <- 'prior sd (delta)'
	normal.flo[l,4] <- 'init lower'
	normal.flo[l,5] <- 'init upper'
	normal.flo[l,6] <- 'init values'
		
	lower <- c(defaults$Triangle.ini.low, defaults$k.ini.low, defaults$z.ini.low)
	upper <- c(defaults$Triangle.ini.up, defaults$k.ini.up, defaults$z.ini.up)
	ini <- c(unlist(defaults$Triangle.ini), defaults$k.ini, defaults$z.ini)
	e$a <- e$delta <- e$lower <- e$upper <- e$init <- NULL
	labels <- paste('<span>', c(paste('Triangle<sub>', 1:4, '</sub>', sep=''), 'k', 'z'), ':</span>', sep='')
	for (i in 1:6) {
		row <- l + i
		normal.flo[row,1] <- glabel(labels[i], markup=TRUE, cont=normal.flo)
		e$a <- c(e$a, normal.flo[row,2] <- gedit(defaults$a[i], width=7, cont=normal.flo))
		e$delta <- c(e$delta, normal.flo[row,3] <- gedit(defaults$delta[i], width=7, cont=normal.flo))
		e$lower <- c(e$lower, normal.flo[row,4] <- gedit(lower[i], width=7, cont=normal.flo))
		e$upper <- c(e$upper, normal.flo[row,5] <-  gedit(upper[i], width=7, cont=normal.flo))
		e$init  <- c(e$init, normal.flo[row,6] <- gedit(ini[i], width=12, cont=normal.flo))
		addHandlerChanged(e$init[[i]], action=list(idx=i), handler=function(h1,...) {
						isempty <- nchar(svalue(e$init[[h1$action$idx]]))==0
						enabled(e$lower[[h1$action$idx]]) <- isempty
						enabled(e$upper[[h1$action$idx]]) <- isempty
						})
	}
	widget.defaults[['lower']] <- lower
	widget.defaults[['upper']] <- upper
	widget.defaults[['init']] <- if(is.null(ini)) rep('', 6) else ini
	
	linked.pars.list[['Triangle.ini']] <- e$init[1:4]
	linked.pars.list[['k.ini']] <- e$init[[5]]
	linked.pars.list[['z.ini']] <- e$init[[6]]
	linked.pars.list[['Triangle.ini.low']] <- e$lower[1:4]
	linked.pars.list[['Triangle.ini.up']] <- e$upper[1:4]
	linked.pars.list[['k.ini.low']] <- e$lower[[5]]
	linked.pars.list[['k.ini.up']] <- e$upper[[5]]
	linked.pars.list[['z.ini.low']] <- e$lower[[6]]
	linked.pars.list[['z.ini.up']] <- e$upper[[6]]
		
	ini.normal.f <- gframe("<span  color='#0B6138'>Normal initial values</span>", markup=TRUE, cont=normal.g,
								horizontal=FALSE)
	nini.flo <- glayout(cont=ini.normal.f)
	
	l <- 1 # row 1
	nini.flo[l,1] <- ''
	nini.flo[l,2] <- 'init mean'
	nini.flo[l,3] <- 'init sd'
	nini.flo[l,4] <- 'width'

	ini.c.means <- c(defaults$Triangle.c.ini.norm[[1]], 
					defaults$k.c.ini.norm[1], defaults$z.c.ini.norm[1])
	ini.c.sd <- c(defaults$Triangle.c.ini.norm[[2]], 
					defaults$k.c.ini.norm[2], defaults$z.c.ini.norm[2])
	c.width <- c(defaults$Triangle.c.width, defaults$k.c.width, defaults$z.c.width)
	e$c.means <- e$c.sd <- e$c.width <- NULL
	c.labels <- paste('<span>', c(paste('Triangle<sup>c</sup><sub>', 1:4, '</sub>', sep=''), 
						'k<sup>c</sup>', 'z<sup>c</sup>'), ':</span>', sep='')
	for (i in 1:6) {
		row <- l + i
		nini.flo[row,1] <- glabel(c.labels[i], markup=TRUE, cont=nini.flo)
		e$c.means <- c(e$c.means, nini.flo[row,2] <- gedit(ini.c.means[i], width=5, cont=nini.flo))
		e$c.sd <- c(e$c.sd, nini.flo[row,3] <- gedit(ini.c.sd[i], width=5, cont=nini.flo))
		e$c.width <- c(e$c.width, nini.flo[row,4] <- gedit(c.width[i], width=5, cont=nini.flo))
	}
	widget.defaults[['c.means']] <- ini.c.means
	widget.defaults[['c.sd']] <- ini.c.sd
	widget.defaults[['c.width']] <- c.width

	linked.pars.tuple[['Triangle.c.ini.norm']] <- list(e$c.means[1:4], e$c.sd[1:4])
	linked.pars.tuple[['k.c.ini.norm']] <- c(e$c.means[[5]], e$c.sd[[5]])
	linked.pars.tuple[['z.c.ini.norm']] <- c(e$c.means[[6]], e$c.sd[[6]])
	linked.pars.list[['Triangle.c.width']] <-  e$c.width[1:4]
	linked.pars.list[['k.c.width']] <-  e$c.width[[5]]
	linked.pars.list[['z.c.width']] <-  e$c.width[[6]]

	gamma.uniform.g <- ggroup(horizontal=TRUE, cont=priors.f)
	priors.gamma.f <- gframe("<span  color='#0B6138'>Gamma priors</span>", markup=TRUE, cont=gamma.uniform.g,
								horizontal=FALSE)
	shape.g <- ggroup(horizontal=TRUE, cont=priors.gamma.f)
	glabel('shape: nu', cont=shape.g)
	e$nu <- gedit(defaults$nu, width=3, cont=shape.g)
	glabel('/2', cont=shape.g)
	
	gamma.flo <- glayout(cont=priors.gamma.f)
	
	l <- 1 # row 1
	gamma.flo[l,1] <- ''
	gamma.flo[l,2] <- 'sqrt{rate} (tau)'
	gamma.flo[l,3] <- 'center'
	gamma.flo[l,4] <- 'init lower'
	gamma.flo[l,5] <- 'init upper'
	gamma.flo[l,6] <- 'init values'
	
	
	centers <- round(sqrt(2*defaults$tau/defaults$nu),2)
	lambda.lower <- c(defaults$lambda.ini.low, defaults$lambda.k.ini.low, defaults$lambda.z.ini.low)
	lambda.upper <- c(defaults$lambda.ini.up, defaults$lambda.k.ini.up, defaults$lambda.z.ini.up)
	lambda.ini <- c(unlist(defaults$lambda.ini), defaults$lambda.k.ini, defaults$lambda.z.ini)

	g.labels <- paste('<span>', paste('lambda<sub>', c(1:4, 'k', 'z'), '</sub>', sep=''), 
						':</span>', sep='')
	e$tau <- e$center <- NULL
	for (i in 1:6) {
		row <- l + i
		gamma.flo[row,1] <- glabel(g.labels[i], markup=TRUE, cont=gamma.flo)
		e$tau <- c(e$tau, gamma.flo[row,2] <- gedit(defaults$tau[i], width=5, cont=gamma.flo))
		e$center <- c(e$center, gamma.flo[row,3] <- gedit(centers[i], width=5, cont=gamma.flo))
		addHandlerChanged(e$tau[[i]], action=list(idx=i), handler=function(h1,...) {
			blockHandler(e$center[[h1$action$idx]])
			svalue(e$center[[h1$action$idx]]) <- round(sqrt(2*as.numeric(svalue(e$tau[[h1$action$idx]]))/as.numeric(svalue(e$nu))),2)
			unblockHandler(e$center[[h1$action$idx]])
			})
		addHandlerChanged(e$center[[i]], action=list(idx=i), handler=function(h1,...) {
			blockHandler(e$tau[[h1$action$idx]])
			svalue(e$tau[[h1$action$idx]]) <- as.numeric(svalue(e$center[[h1$action$idx]]))^2 * as.numeric(svalue(e$nu))/2
			unblockHandler(e$tau[[h1$action$idx]])
			})
		e$lambda.lower <- c(e$lambda.lower, gamma.flo[row,4] <- gedit(lambda.lower[i], width=7, cont=gamma.flo))
		e$lambda.upper <- c(e$lambda.upper, gamma.flo[row,5] <-  gedit(lambda.upper[i], width=7, cont=gamma.flo))
		e$lambda.init  <- c(e$lambda.init, gamma.flo[row,6] <- gedit(lambda.ini[i], width=12, cont=gamma.flo))
		addHandlerChanged(e$lambda.init[[i]], action=list(idx=i), handler=function(h1,...) {
						isempty <- nchar(svalue(e$lambda.init[[h1$action$idx]]))==0
						enabled(e$lambda.lower[[h1$action$idx]]) <- isempty
						enabled(e$lambda.upper[[h1$action$idx]]) <- isempty
						})

	}
	addHandlerChanged(e$nu, handler=function(h1,...) for(i in 1:6) {
			blockHandler(e$center[[i]])
			svalue(e$center[[i]]) <- round(sqrt(2*as.numeric(svalue(e$tau[[i]]))/as.numeric(svalue(e$nu))),2)
			unblockHandler(e$center[[i]])
			})
	widget.defaults[['lambda.lower']] <- lambda.lower
	widget.defaults[['lambda.upper']] <- lambda.upper
	widget.defaults[['lambda.init']] <- if(is.null(lambda.ini)) rep('', 6) else lambda.ini
	
	linked.pars.list[['lambda.ini']] <- e$lambda.init[1:4]
	linked.pars.list[['lambda.k.ini']] <- e$lambda.init[[5]]
	linked.pars.list[['lambda.z.ini']] <- e$lambda.init[[6]]
	linked.pars.list[['lambda.ini.low']] <- e$lambda.lower[1:4]
	linked.pars.list[['lambda.ini.up']] <- e$lambda.upper[1:4]
	linked.pars.list[['lambda.k.ini.low']] <- e$lambda.lower[[5]]
	linked.pars.list[['lambda.k.ini.up']] <- e$lambda.upper[[5]]
	linked.pars.list[['lambda.z.ini.low']] <- e$lambda.lower[[6]]
	linked.pars.list[['lambda.z.ini.up']] <- e$lambda.upper[[6]]

	info.gamma.glo <- glayout(cont=priors.gamma.f)
	info.gamma.glo[1,1, expand=FALSE, anchor=c(-1,0)] <- glabel('"center" is the prior center for sigma: sqrt(2*tau/nu)', 
					cont =  info.gamma.glo)
	info.gamma.glo[2,1, expand=FALSE] <- glabel('After editing a cell, hit Enter and wait for the other cells to be updated.', 
				cont =  info.gamma.glo)
	#info.gamma.glo[4,1, expand=FALSE] <- glabel('', cont =  info.gamma.glo)
	
	uniform.dlf.g <- ggroup(horizontal=FALSE, cont=gamma.uniform.g, expand=TRUE)

	
	priors.unif.f <- gframe("<span  color='#0B6138'>Uniform priors</span>", markup=TRUE, cont=uniform.dlf.g,
								horizontal=TRUE)
	unif.flo <- glayout(cont=priors.unif.f)
	l <- 1
	unif.flo[l,1] <- ''
	unif.flo[l,2] <- 'init lower'
	unif.flo[l,3] <- 'init upper'
	unif.flo[l,4] <- 'init values'
	
	l<-l+1
	unif.flo[l,1] <- 'omega'
	e$omega.ini.low <- unif.flo[l,2] <- gedit(defaults$omega.ini.low, width=5, cont=unif.flo)
	e$omega.ini.up <- unif.flo[l,3] <- gedit(defaults$omega.ini.up, width=5, cont=unif.flo)
	e$omega.ini <- unif.flo[l,4] <- gedit(defaults$omega.ini, width=10, cont=unif.flo)
	addHandlerChanged(e$omega.ini, handler=function(h1,...) {
						isempty <- nchar(svalue(e$omega.ini))==0
						enabled(e$omega.ini.low) <- isempty
						enabled(e$omega.ini.up) <- isempty
						})
	widget.defaults[['omega.ini']] <- if(is.null(defaults$omega.ini)) '' else defaults$omega.ini
	
	addSpace(uniform.dlf.g, 10)
	dlf.f <- gframe("<span  color='blue'>Double-logistic parameters</span>", markup=TRUE, cont=uniform.dlf.g,
								horizontal=TRUE)
	glabel('p1:', cont=dlf.f)
	e$dl.p1 <- gedit(defaults$dl.p1, width=5, cont=dlf.f)
	addSpace(dlf.f, 10)
	glabel('p2:', cont=dlf.f)
	e$dl.p2 <- gedit(defaults$dl.p2, width=5, cont=dlf.f)

	addSpring(uniform.dlf.g)
	info.ini.group <- ggroup(horizontal=FALSE, cont=uniform.dlf.g)
	glabel('NOTE:', cont=info.ini.group, anchor=c(-1,0))
	glabel('Leave "init values" blank for the starting values', cont=info.ini.group, anchor=c(-1,0))
	glabel('being equally distributed between "init lower" and', cont=info.ini.group, anchor=c(-1,0))
	glabel('"init upper". For specific initial values enter', cont=info.ini.group, anchor=c(-1,0))
	glabel('one value per chain separated by commas.', cont=info.ini.group, anchor=c(-1,0))

	
	# Buttons
	button.g <- ggroup(cont=e$adv.g, horizontal=TRUE)
	gbutton('Cancel', cont=button.g, handler=function(h, ...) 
					visible(adv.set.win) <- FALSE)
	addSpring(button.g)
	e$adv.set.defaultbutton <- gbutton('  Set to Default Values  ', cont=button.g, handler=set.defaults)
	e$adv.set.okbutton <- gbutton('OK', cont=button.g)
	
	e$linked.pars.list <- linked.pars.list
	e$linked.pars.tuple <- linked.pars.tuple
	e$widget.defaults <- widget.defaults
	visible(adv.set.win) <- TRUE
	h$action$env$adv.set.env <- e
	
	}
	if(!is.null(h$action$env$adv.set.okhandler)) 
		removehandler(h$action$env$adv.set.env$adv.set.okbutton, h$action$env$adv.set.okhandler)
	h$action$env$adv.set.okhandler <- addhandlerclicked(h$action$env$adv.set.env$adv.set.okbutton, 
											handler=set.advance.pars)

}