e0Results.group <- function(g, main.win, parent) {
	e <- new.env()
								
	e$sim.dir <- parent$sim.dir
	graph.defaults <- formals(png)

	nb <- gnotebook(container=g, expand=TRUE)
	
	traj.g <- ggroup(label="<span color='#0B6138'>e0 trajectories</span>", 
							markup=TRUE, horizontal=FALSE, container=nb)
	traj.env <- e0.show.trajectories.group(traj.g, main.win, e)
	map.g <- ggroup(label="<span color='#0B6138'>e0 world maps</span>", 
							markup=TRUE, horizontal=FALSE, container=nb)
	map.env <- e0.show.map.group(map.g, main.win, e)
	dl.g <- ggroup(label="<span color='#0B6138'>DL curve</span>", 
							markup=TRUE, horizontal=FALSE, container=nb)
	dl.env <- e0.show.dl.group(dl.g, main.win, e)
	traces.g <- ggroup(label="<span color='#0B6138'>Parameter Traces</span>", 
							markup=TRUE, horizontal=FALSE, container=nb)
	traces.env <- e0.show.traces.group(traces.g, main.win, e)
	convergence.g <- ggroup(label="<span color='#0B6138'>Convergence</span>", markup=TRUE, horizontal=FALSE, container=nb)
	create.convergence.tab(convergence.g, e$sim.dir, type='e0', package='bayesLife', main.win)

	svalue(nb) <- 1
}

e0.show.trajectories.group <- function(g, main.win, parent.env) {
	e <- new.env()
	e$sim.dir <- parent.env$sim.dir
	e$pred.type <- 'e0'
	defaults.pred <- formals(e0.predict)
	defaults.traj <- formals(e0.trajectories.plot)
	defaults.traj.all <- formals(e0.trajectories.plot.all)
		
	country.f <- gframe("<span color='blue'>Country settings</span>", markup=TRUE, 
									horizontal=FALSE, container=g)
	e$show.traj.country <- create.country.widget(country.f, defaults.traj.all, 
									main.win, prediction=TRUE, parent.env=e)
		
	traj.settings.f <- gframe("<span color='blue'>Trajectories settings</span>", markup=TRUE, 
								horizontal=TRUE, container=g)
	glabel('CI (%):', container=traj.settings.f)
	e$pi <- gedit('80, 95', width=7, container=traj.settings.f)

	addSpace(traj.settings.f, 15)
	glabel('# trajectories:', container=traj.settings.f)
	e$nr.traj <- gedit(20, width=6, container=traj.settings.f)
	addSpace(traj.settings.f, 15)
	e$sex <- gdroplist(c('Female', 'Male', 'Both'), container=traj.settings.f, selected=1,
				handler=function(h,...) {
					if(svalue(h$obj) == 'Both') {svalue(e$nr.traj) <- 0; svalue(e$pi) <- 95}
					else {svalue(e$nr.traj) <- 20; svalue(e$pi) <- '80, 95'}
					enabled(e$TableB.show.traj) <- svalue(h$obj) != 'Both'
				})	
	time.f <- gframe("<span color='blue'>Time range</span>", markup=TRUE, 
									horizontal=TRUE, container=g)
	glabel('From year:', container=time.f)
	e$start.year <- gedit('', width=4, container=time.f)
	glabel('To year:', container=time.f)
	e$end.year <- gedit('', width=4, container=time.f)
	
	graph.f <- gframe("<span color='blue'>Advanced graph parameters</span>", markup=TRUE, 
									horizontal=FALSE, container=g)
	e$graph.pars <- create.graph.pars.widgets(graph.f, main.win=main.win)
	addSpring(g)
	button.g <- ggroup(horizontal=TRUE, container=g)
	create.help.button(topic='e0.trajectories.plot', package='bayesLife', parent.group=button.g,
						parent.window=main.win)
	addSpring(button.g)
	
	gbutton('Generate Script', container=button.g, handler=show.e0.traj, 
								action=list(mw=main.win, env=e, type='plot', script=TRUE))
	addSpace(button.g, 5)
	TableB.show.traj.act <- gaction(label='Table', icon='dataframe', handler=show.e0.traj, 
						action=list(mw=main.win, env=e, type='table', script=FALSE))
	GraphB.show.traj.act <- gaction(label='Graph', icon='lines', handler=show.e0.traj, 
						action=list(mw=main.win, env=e, type='plot', script=FALSE))
	e$TableB.show.traj <- gbutton(action=TableB.show.traj.act, container=button.g)
	gbutton(action=GraphB.show.traj.act, container=button.g)
	return(e)
}

get.additional.e0.param <- function(e, ...) {
	sex <- svalue(e$sex)
	param <- list(both.sexes= sex=='Both', joint.male= sex == 'Male')
	return(list(add=param, plot=c('pi', 'xlim', 'nr.traj', 'both.sexes'), 
					pred=c('joint.male'),
					table=c('pi', 'country'), table.decimal=2))
	}
	
show.e0.traj <- function(h, ...) {
	e <- h$action$env
	pred.type <- if(is.null(h$action$pred.type)) 'e0' else h$action$pred.type
	package <- if(is.null(h$action$package)) 'bayesLife' else h$action$package
	if(!has.required.arguments(list(sim.dir='Simulation directory'), env=e)) return()
	show.type <- h$action$type
	country.pars <- get.country.code.from.widget(e$show.traj.country$country.w, e$show.traj.country, 
							force.country.spec=show.type!='plot')
	if(is.null(country.pars)) return(NULL)
	param.names.all <- list(text='sim.dir', numvector='pi', 
							numeric=c('nr.traj', 'start.year', 'end.year'))
	param.env <- get.parameters(param.names.all, env=e, quote=h$action$script)
	param.env.rest <- list(country=country.pars$code, output.dir=country.pars$output.dir,
							output.type=country.pars$output.type, verbose=TRUE)
	param.env <- c(param.env, 
					get.parameters(list(text=c('output.dir', 'output.type'), 
										logical='verbose', numeric='country'), 
									param.env.rest, quote=TRUE,
									retrieve.from.widgets=FALSE))
	add.param.names <- do.call(paste('get.additional.',pred.type, '.param', sep=''), 
									list(e, script=h$action$script, type=show.type))
	param.env <- c(param.env, add.param.names[['add']])
	param.pred <- param.env[c('sim.dir', add.param.names[['pred']])]
	
	pred <- do.call(paste('get.', pred.type, '.prediction', sep=''), param.pred)
	if(h$action$script) {
		cmd <- paste('pred <- get.', pred.type, '.prediction(', 
					paste(paste(names(param.pred), param.pred, sep='='), collapse=', '), 
						')\n', sep='')
	} else {	
		cmd <- ''
	}
	xmin <- param.env$start.year
	xmax <- param.env$end.year
	if(!is.null(xmin) || !is.null(xmax)) {
		param.env.xlim <- list(xlim=paste(if(!is.null(xmin)) xmin else pred$mcmc.set$meta$start.year,
									 if(!is.null(xmax)) xmax else pred$end.year, sep=', '))
		param.env <- c(param.env, get.parameters(list(numvector='xlim'), param.env.xlim, quote=h$action$script,
									retrieve.from.widgets=FALSE))
	}

	pars.value <- svalue(e$graph.pars)
	if (show.type == 'plot') {
		param.plot1c <- param.env[add.param.names[['plot']][is.element(add.param.names[['plot']], names(param.env))]]
		if(is.element('country', names(param.env))) param.plot1c <- c(param.plot1c, param.env['country'])
		if(!is.null(param.env$country)) { # one country
			cmd <- paste(cmd, paste(pred.type, '.trajectories.plot(pred,', sep=''),
						paste(paste(names(param.plot1c), param.plot1c, sep='='), collapse=', '), ',',
						pars.value, ')')
			if (h$action$script) {
				script.text <- gwindow(paste(package,'commands'), parent=h$action$mw)
				gtext(cmd, container=script.text)
			} else {
				create.graphics.window(parent=h$action$mw, title=paste("Trajectories for", country.pars$name))
				eval(parse(text=cmd))
			}
		} else { # all countries
			param.plot.allc <- param.env[c(names(param.plot1c), 'output.dir', 'output.type',  'verbose')]
			cmd <- paste(cmd, paste(pred.type, '.trajectories.plot.all(pred, ', sep=''), 
						paste(paste(names(param.plot.allc), param.plot.allc, sep='='), collapse=', '), sep='')
			if(!is.null(pars.value)) {
				if(nchar(pars.value)>0)
					cmd <- paste(cmd, ',', pars.value)
			}
			cmd <- paste(cmd, ')', sep='')
			if (h$action$script) {
				script.text <- gwindow(paste(package,'commands'), parent=h$action$mw)
				gtext(cmd, container=script.text)
			} else {
				eval(parse(text=cmd))
			}
		}
	} else {
		# Table
		param.table <- param.env[add.param.names[['table']][is.element(add.param.names[['table']], names(param.env))]]
		table.values <- do.call(paste(pred.type, '.trajectories.table', sep=''), 
							c(list(pred), param.table))
		table.values <- round(table.values[!apply(is.na(table.values), 1, all),],
								add.param.names[['table.decimal']])
		table.values <- cbind(rownames(table.values), table.values)
		colnames(table.values)[1] <- 'year'
		win <- gwindow(do.call(paste('get.', pred.type, '.table.title', sep=''), 
						list(country.pars$name, pred)),
					parent=h$action$mw, height=max(min(22.2*(dim(table.values)[1]+1),600), 100))
		g <- ggroup(container=win, horizontal=FALSE, expand=TRUE)
		gt <- gtable(table.values, container=g, expand=TRUE)
		gbutton('Print to R Console', container=g, handler=function(h,...){
										print(do.call(paste(pred.type, '.trajectories.table', sep=''), 
												c(list(pred), param.table)))})
	}
}

get.e0.table.title <- function(country, pred) 
	return (paste(country, '-', bayesLife:::get.sex.label(pred$mcmc.set$meta)))
	
e0.show.map.group <- function(g, main.win, parent.env) {
	e <- new.env()
	e$sim.dir <- parent.env$sim.dir
	set.f <- gframe("<span color='blue'>Map settings</span>", markup=TRUE, 
									horizontal=FALSE, container=g)
	set.g1 <- ggroup(horizontal=TRUE, container=set.f)
	glabel('Percentile:', container=set.g1)
	e$percentiles <- list('median'=0.5, 'lower 80'=0.1, 'upper 80'=0.9, 'lower 90'=0.05, 'upper 90'=0.95,
						'lower 95'=0.025, 'upper 95'=0.975, 'lower 60'=0.2, 'upper 60'=0.8,
						'lower 50'=0.25, 'upper 50'=0.75, 'lower 40'=0.3, 'upper 40'=0.7, 
						'lower 20'=0.4, 'upper 20'=0.6
						)
	e$map.percentile <- gdroplist(names(e$percentiles), container=set.g1)
	addSpace(set.g1, 5)
	glabel('Measure:', container=set.g1)
	e$map.measure <- gdroplist(c('e0', bayesLife:::e0.parameter.names.cs.extended()), container=set.g1)
	addSpace(set.g1, 5)
	e$map.same.scale <- gcheckbox('Same scale for all maps', checked=TRUE, container=set.g1)
	
	set.g3 <- ggroup(horizontal=TRUE, container=set.f)
	glabel('Bounds:    ', container=set.g3)
	e$map.bounds <- gdroplist(c(80, 90, 95, 60, 50, 40, 20), container=set.g3)
	glabel('%', container=set.g3)
	addSpace(set.g3, 15)
	e$sex <- gdroplist(c('Female', 'Male'), container=set.g3, selected=1)	
	
	set.g2 <- ggroup(horizontal=TRUE, container=set.f)
	glabel('Use R package:', container=set.g2)
	e$map.package <- gradio(c('rworldmap', 'googleVis'), horizontal = TRUE, 
						handler=function(h, ...) {
							enabled(e$map.bounds) <- svalue(h$obj) == 'googleVis';
							enabled(e$map.same.scale) <- svalue(h$obj) == 'rworldmap'}, 
						container=set.g2)
	enabled(e$map.bounds) <- svalue(e$map.package) == 'googleVis'
	enabled(e$map.same.scale) <- svalue(e$map.package) == 'rworldmap'
	addSpring(g)
	bg <- ggroup(horizontal=TRUE, container=g)
	create.help.button(topic='e0.map', package='bayesLife', parent.group=bg,
						parent.window=main.win)
	addSpring(bg)
	gbutton('Generate Script', container=bg, handler=e0.showMap, 
								action=list(mw=main.win, env=e, script=TRUE))
	addSpace(bg, 5)
	GraphB.map <- gaction(label=' Show Map ', handler=e0.showMap, 
						action=list(mw=main.win, env=e, script=FALSE))
	gbutton(action=GraphB.map, container=bg)
	
}

e0.showMap <- function(h, ...) {
	e <- h$action$env
	if(!has.required.arguments(list(sim.dir='Simulation directory'), env=e)) return()
	percentile <- svalue(e$map.percentile)
	quantile <- e$percentiles[[percentile]]
	param.env <-list(sim.dir=svalue(e$sim.dir), quantile=quantile)
	param.names1 <- list(text='sim.dir')
	param.pred <- get.parameters(param.names1, env=param.env, quote=h$action$script, retrieve.from.widgets=FALSE)
	same.scale <- svalue(e$map.same.scale)
	par.name <- svalue(e$map.measure)
	bounds <- svalue(e$map.bounds)
	package <- svalue(e$map.package)
	sex <- svalue(e$sex)
	param.pred$joint.male <- sex == 'Male'
	map.function <- if(package == 'rworldmap') 'e0.map' else 'e0.map.gvis'
	if(h$action$script) {
		cmd <- paste('pred <- get.e0.prediction(', paste(paste(names(param.pred), param.pred, sep='='), collapse=', '), 
						')\n', sep='')
		if (par.name == 'e0') {
			if(package == 'rworldmap') {
				cmd <- paste(cmd, "param.map <- get.e0.map.parameters(pred, same.scale=", same.scale,
					", quantile=", quantile, ")\n", sep="")
				cmd <- paste(cmd, 'do.call("', map.function, '", param.map)', sep='')
			} else {
				cmd <- paste(cmd, map.function, '(pred, quantile=', quantile, ', pi=', bounds, ')', sep='')
			}
		} else {
			cmd <- paste(cmd, map.function, '(pred, quantile=', quantile, ', par.name="', par.name, '"', sep='')
			cmd <- paste(cmd, if (package == 'googleVis') paste(', pi=', bounds, sep='') else '', sep='')
			cmd <- paste(cmd, ')', sep='')
		}
		script.text <- gwindow('bayesLife commands', parent=h$action$mw)
		gtext(cmd, container=script.text)
	} else {
		pred <- do.call('get.e0.prediction', param.pred)
		if (par.name == 'e0' && package == 'rworldmap') {
			param.map <- get.e0.map.parameters(pred, same.scale=same.scale, quantile=quantile)
		} else {
			param.map <- list(pred=pred, quantile=quantile)
			if (par.name != 'e0')
				param.map[['par.name']]<- par.name
		}
		if(package == 'rworldmap') param.map[['device']] <- 'dev.new'
		if (package == 'googleVis') param.map[['pi']] <- bounds
		g <- create.graphics.map.window(parent=h$action$mw, pred=pred, params=param.map, percentile=percentile, 
										is.gvis= package == 'googleVis', title="World Map", type='e0', main.part='e0')
	}
}

e0.show.dl.group <- function(g, main.win, parent.env) {
	e <- new.env()
	e$sim.dir <- parent.env$sim.dir
	e$pred.type <- 'e0'
	defaults.dl <- formals(e0.DLcurve.plot)
	defaults.dl.all <- formals(e0.DLcurve.plot.all)
	country.f <- gframe("<span color='blue'>Country settings</span>", markup=TRUE, 
							horizontal=FALSE, container=g)
	e$dlc.country <- create.country.widget(country.f, defaults.dl.all, main.win, prediction=FALSE, 
											parent.env=e)
	dl.f <- gframe("<span color='blue'>DL curve settings</span>", markup=TRUE, 
							horizontal=FALSE, container=g)
	dlfg1 <- ggroup(horizontal=TRUE, container=dl.f)
	glabel('CI (%):', container=dlfg1)
	e$pi <- gedit('80, 95', width=7, container=dlfg1)
	glabel('Burnin:', container=dlfg1)
	e$burnin <- gedit(defaults.dl$burnin, width=5, container=dlfg1)
	glabel('e0 min, max:', container=dlfg1)
	e$e0.lim <- gedit(defaults.dl$e0.lim, width=7, container=dlfg1)
	glabel('# curves:', container=dlfg1)
	e$nr.curves <- gedit(defaults.dl$nr.curves, width=6, container=dlfg1)
	
	dlfg2 <- ggroup(horizontal=TRUE, container=dl.f)
	e$predictive.distr <- gcheckbox('Predictive distribution', 
							checked=defaults.dl$predictive.distr, container=dlfg2)
	
	graph.f <- gframe("<span color='blue'>Advanced graph parameters</span>", markup=TRUE, 
						horizontal=FALSE, container=g)
	e$graph.pars <- create.graph.pars.widgets(graph.f, main.win=main.win)
	addSpring(g)
	button.g <- ggroup(horizontal=TRUE, container=g)
	create.help.button(topic='e0.DLcurve.plot', package='bayesLife', parent.group=button.g,
						parent.window=main.win)
	addSpring(button.g)
	gbutton('Generate Script', container=button.g, handler=e0.showDLcurve, 
								action=list(mw=main.win, env=e, script=TRUE))
	addSpace(button.g, 5)
	GraphB.dlc <- gaction(label='Graph', icon='lines', handler=e0.showDLcurve, 
						action=list(mw=main.win, env=e, script=FALSE))
	gbutton(action=GraphB.dlc, container=button.g)

}

e0.showDLcurve <- function(h, ...) {
	e <- h$action$env
	if(!has.required.arguments(list(sim.dir='Simulation directory'), env=e)) return()
	country.pars <- get.country.code.from.widget(e$dlc.country$country.w, e$dlc.country)
	if(is.null(country.pars)) return(NULL)
	param.names.all <- list(text='sim.dir', numvector=c('pi', 'e0.lim'),
							numeric=c('nr.curves', 'burnin'),
							logical='predictive.distr')
	param.env <- get.parameters(param.names.all, env=e, quote=h$action$script)
	param.env.rest <- list(country=country.pars$code, output.dir=country.pars$output.dir,
							output.type=country.pars$output.type, verbose=TRUE)
	param.env <- c(param.env, 
					get.parameters(list(text=c('output.dir', 'output.type'), 
										logical='verbose', numeric='country'), 
									param.env.rest, quote=TRUE,
									retrieve.from.widgets=FALSE))

	param.mcmc <- param.env['sim.dir']
	if(h$action$script) {
		cmd <- paste('m <- get.e0.mcmc(', paste(paste(names(param.mcmc), param.mcmc, sep='='), collapse=', '), 
						')\n', sep='')
	} else {
		m <- do.call('get.e0.mcmc', param.mcmc)
		cmd <- ''
	}
	pars.value <- svalue(e$graph.pars)
	param.plot1c <- list()
	for (par in c('pi', 'nr.curves', 'e0.lim', 'country', 'burnin', 'predictive.distr')) 
		if(is.element(par, names(param.env))) param.plot1c <- c(param.plot1c, param.env[par])

	if(!is.null(country.pars$code)) { # one country
		cmd <- paste(cmd, 'e0.DLcurve.plot(mcmc.list=m, ',
						paste(paste(names(param.plot1c), param.plot1c, sep='='), collapse=', '), ', ',
						pars.value, ')', sep='')
		if (h$action$script) {
			script.text <- gwindow('bayesLife commands', parent=h$action$mw)
			gtext(cmd, container=script.text)
		} else {
			create.graphics.window(parent=h$action$mw, title=paste("Double Logistic Curves for", country.pars$name))
			eval(parse(text=cmd))
		}
	} else { # all countries
		param.plot.allc <- param.env[c(names(param.plot1c), 'output.dir', 'output.type',  'verbose')]
		cmd <- paste(cmd, 'e0.DLcurve.plot.all(mcmc.list=m, ', 
						paste(paste(names(param.plot.allc), param.plot.allc, sep='='), collapse=', '), ', ',
					pars.value, ')', sep='')
		if (h$action$script) {
			script.text <- gwindow('bayesLife commands', parent=h$action$mw)
			gtext(cmd, container=script.text)
		} else {
			eval(parse(text=cmd))
		}
	}
}

e0.show.traces.group <- function(g, main.win, parent.env) {
	e <- new.env()
	e$sim.dir <- parent.env$sim.dir
	e$pred.type <- 'e0'
	country.f <- gframe("<span color='blue'>Country settings</span>", markup=TRUE, 
							horizontal=TRUE, container=g)
	e$cs.chb <- gcheckbox("Country specific", checked=FALSE, container=country.f,
							handler=function(h,...) {
								if (svalue(h$obj)) {
									enabled(e$par.cs.dl)<-!svalue(e$pars.chb)
									enabled(e$par.dl)<-FALSE
								} else {
									enabled(e$par.dl)<-!svalue(e$pars.chb)
									enabled(e$par.cs.dl)<-FALSE
									}
								enabled(e$country$country.w) <- svalue(h$obj)
								enabled(e$country$country.select.b) <- svalue(h$obj)							}
							)
	addSpace(country.f, 5)
	
	e$country <- create.country.widget(country.f,  main.win=main.win, show.all=FALSE, prediction=FALSE, 
											parent.env=e)
	par.f <- gframe("<span color='blue'>Parameter settings</span>", markup=TRUE, 
							horizontal=FALSE, container=g)
	par.g1 <- ggroup(horizontal=TRUE, container=par.f)
	glabel('Parameters:', container=par.g1)
	e$pars.chb <- gcheckbox("all", container=par.g1, checked=TRUE, 
							handler=function(h,...) 
								if(svalue(e$cs.chb)) {
									enabled(e$par.cs.dl)<-!svalue(h$obj)
									enabled(e$par.dl)<-FALSE
								} else {
									enabled(e$par.dl)<-!svalue(h$obj)
									enabled(e$par.cs.dl)<-FALSE
							})
	addSpace(par.g1, 15)
	e$par.dl <- gdroplist(e0.parameter.names(), container=par.g1)
	enabled(e$par.dl) <- FALSE
	e$par.cs.dl <- gdroplist(e0.parameter.names.cs(), container=par.g1)
	enabled(e$par.cs.dl) <- FALSE
	addSpace(par.f, 10)
	glabel('# points:', container=par.g1)
	e$nr.points <- gedit(100, width=5, container=par.g1, coerce.with=as.numeric)
	
	par.g2 <- ggroup(horizontal=TRUE, container=par.f)
	glabel("Burnin:", container=par.g2)
	e$burnin <- gedit(0, width=5, container=par.g2, coerce.with=as.numeric)
	glabel("Thin:", container=par.g2)
	e$thin <- gedit(1, width=5, container=par.g2, coerce.with=as.numeric)
	
	enabled(e$country$country.w) <- svalue(e$cs.chb)
	enabled(e$country$country.select.b) <- svalue(e$cs.chb)
	addSpring(g)
	button.g <- ggroup(horizontal=TRUE, container=g)
	create.help.button(topic='e0.partraces.plot', package='bayesLife', parent.group=button.g,
						parent.window=main.win)	
	addSpring(button.g)
	SummaryB.traces <- gaction(label='Show summary', handler=e0.showParTraces, 
						action=list(mw=main.win, env=e, print.summary=TRUE))
	gbutton(action=SummaryB.traces, container=button.g)
	GraphB.traces <- gaction(label='Graph', icon='lines', handler=e0.showParTraces, 
						action=list(mw=main.win, env=e, print.summary=FALSE))
	gbutton(action=GraphB.traces, container=button.g)
}

e0.showParTraces <- function(h, ...) {
	e <- h$action$env
	cs <- svalue(e$cs.chb)
	dir <- svalue(e$sim.dir)
	burnin <- svalue(e$burnin)
	thin <- svalue(e$thin)
	print.summary <- h$action$print.summary
	if (cs) {
		country.pars <- get.country.code.from.widget(e$country$country.w, e$country)
		if(is.null(country.pars)) return(NULL)
	}	
	all.pars <- svalue(e$pars.chb)
	nr.points <- svalue(e$nr.points)
	if(print.summary) {
		warn <- getOption('warn')
		options(warn=-1) # disable warning messages
		mcmc.set <- get.e0.mcmc(dir)
		options(warn=warn)
		con <- textConnection("mc.summary", "w", local=TRUE)
		mc.exist <- TRUE
		sink(con)
		if (is.null(mcmc.set)) {
			cat('No simulation available in this directory.')
			mc.exist <- FALSE
		}
	} else create.graphics.window(parent=h$action$mw, title="Parameter traces", dpi=150, height=4*150)
	
	if (cs) { # country-specific parameters
		if (!all.pars) {
			pars <- svalue(e$par.cs.dl)
			if(print.summary) {if (mc.exist) print(summary(mcmc.set, country=country.pars$code, par.names.cs=pars, par.names=NULL, 
											burnin=burnin, thin=thin))
			} else e0.partraces.cs.plot(sim.dir=dir, country=country.pars$code, par.names=pars, nr.points=nr.points, 
											burnin=burnin, thin=thin)
		} else {
			if(print.summary){if (mc.exist) print(summary(mcmc.set, country=country.pars$code, par.names=NULL, 
											burnin=burnin, thin=thin))
			} else e0.partraces.cs.plot(sim.dir=dir, country=country.pars$code, nr.points=nr.points, 
											burnin=burnin, thin=thin)
		}
	} else { # World-parameters
		if (!all.pars) { # selected pars
			pars <- svalue(e$par.dl)
			if(print.summary) {if (mc.exist) print(summary(mcmc.set, par.names.cs=NULL, par.names=pars, 
											burnin=burnin, thin=thin))
			} else e0.partraces.plot(sim.dir=dir, par.names=pars, nr.points=nr.points, 
											burnin=burnin, thin=thin)
		} else { # all pars
			if(print.summary) {if (mc.exist) print(summary(mcmc.set, par.names.cs=NULL, 
											burnin=burnin, thin=thin))
			} else e0.partraces.plot(sim.dir=dir, nr.points=nr.points, 
											burnin=burnin, thin=thin)
		}
	}
	if(print.summary) {
		sink()
		close(con)
		sum.win <- gwindow('MCMC summary', parent=h$action$mw, width=500, height=400)
		gtext(mc.summary, container=sum.win)
	}
}


