popResults.group <- function(g, main.win, parent) {
	e <- new.env()
								
	e$sim.dir <- parent$sim.dir
	
	graph.defaults <- formals(png)

	nb <- gnotebook(container=g, expand=TRUE)
	
	traj.g <- ggroup(label="<span color='#0B6138'>Population trajectories</span>", 
							markup=TRUE, horizontal=FALSE, container=nb)
	traj.env <- pop.show.trajectories.group(traj.g, main.win, e)
	pyr.g <- ggroup(label="<span color='#0B6138'>Population pyramid</span>", 
							markup=TRUE, horizontal=FALSE, container=nb)
	pyr.env <- pop.show.pyramid.group(pyr.g, main.win, e)
#	map.g <- ggroup(label="<span color='#0B6138'>Pop world maps</span>", 
#							markup=TRUE, horizontal=FALSE, container=nb)
#	map.env <- pop.show.map.group(map.g, main.win, e)

	svalue(nb) <- 1
}

pop.show.trajectories.group <- function(g, main.win, parent.env) {
	e <- new.env()
	e$sim.dir <- parent.env$sim.dir
	e$pred.type <- 'pop'
	defaults.pred <- formals(pop.predict)
	defaults.traj <- formals(pop.trajectories.plot)
	defaults.traj.all <- formals(pop.trajectories.plot.all)
		
	country.f <- gframe("<span color='blue'>Country settings</span>", markup=TRUE, 
									horizontal=FALSE, container=g)
	e$show.traj.country <- create.country.widget(country.f, defaults.traj.all, 
									main.win, prediction=TRUE, parent.env=e)
		
	sex.time.g <- ggroup(horizontal=TRUE, container=g)
	age.settings.f <- gframe("<span color='blue'>Sex &amp; age settings</span>", markup=TRUE, 
								horizontal=TRUE, container=sex.time.g)
	glabel('Sex:', container=age.settings.f)
	e$sex <- gdroplist(c('both', 'female', 'male'), container=age.settings.f, selected=1)
	addSpace(age.settings.f, 5)
	age.gb <- gbutton(" Age ", container=age.settings.f,
				handler=selectAgeMenuPop,
				action=list(mw=main.win, env=e, multiple=TRUE))
	e$age <- 'all'
	addSpace(age.settings.f, 5)
	e$sum.over.ages <- gcheckbox("Sum over ages", container=age.settings.f, checked=TRUE,
							handler=function(h,...){
								enabled(e$TableB.show.traj) <- svalue(h$obj)})
	
	addSpring(sex.time.g)
	time.f <- gframe("<span color='blue'>Time range</span>", markup=TRUE, 
									horizontal=TRUE, container=sex.time.g)
	glabel('From year:', container=time.f)
	e$start.year <- gedit(defaults.pred$start.year, width=4, container=time.f)
	glabel('To year:', container=time.f)
	e$end.year <- gedit(defaults.pred$end.year, width=4, container=time.f)
	
	traj.g <- ggroup(horizontal=TRUE, container=g, expand=TRUE)
	traj.settings.f <- gframe("<span color='blue'>Trajectories settings</span>", markup=TRUE, 
								horizontal=TRUE, container=traj.g, expand=TRUE)
	glabel('CI (%):', container=traj.settings.f)
	e$pi <- gedit('80, 95', width=7, container=traj.settings.f)
	
	addSpace(traj.settings.f, 10)
	e$half.child.variant <- gcheckbox('+/- 0.5 child', checked=defaults.traj$half.child.variant, 
								container=traj.settings.f)
	addSpace(traj.settings.f, 10)
	glabel('# trajectories:', container=traj.settings.f)
	e$nr.traj <- gedit(20, width=5, container=traj.settings.f)
	

	
	graph.f <- gframe("<span color='blue'>Advanced graph parameters</span>", markup=TRUE, 
									horizontal=FALSE, container=g)
	e$graph.pars <- create.graph.pars.widgets(graph.f, main.win=main.win)
	addSpring(g)
	button.g <- ggroup(horizontal=TRUE, container=g)
	create.help.button(topic='pop.trajectories.plot', package='bayesPop', parent.group=button.g,
						parent.window=main.win)
	addSpring(button.g)
	
	gbutton('Generate Script', container=button.g, handler=show.e0.traj, 
								action=list(mw=main.win, env=e, type='plot', script=TRUE,
											pred.type='pop', package='bayesPop'))
	addSpace(button.g, 5)
	TableB.show.traj.act <- gaction(label='Table', icon='dataframe', handler=show.e0.traj, 
						action=list(mw=main.win, env=e, type='table', script=FALSE,
											pred.type='pop', package='bayesPop'))
	GraphB.show.traj.act <- gaction(label='Graph', icon='lines', handler=show.e0.traj, 
						action=list(mw=main.win, env=e, type='plot', script=FALSE,
											pred.type='pop', package='bayesPop'))
	e$TableB.show.traj <- gbutton(action=TableB.show.traj.act, container=button.g)
	gbutton(action=GraphB.show.traj.act, container=button.g)
	enabled(e$TableB.show.traj) <- svalue(e$sum.over.ages)
	return(e)

}

get.additional.pop.param <- function(e, script, type, ...) {
	if (e$age==0) e$age<- 'all'
	param.names <- list(text=c('sex'),logical=c('sum.over.ages', 'half.child.variant'))
	quote <- if(type=='table') script else TRUE
	param <- c(get.parameters(param.names, env=e, quote=quote), 
			get.parameters(list(numtext='age'), env=e, quote=quote, retrieve.from.widgets=FALSE))
	return(list(add=param, plot=c('pi', 'xlim', 'nr.traj', 'sex', 'age', 'sum.over.ages', 'half.child.variant'), 
					 table=c('pi', 'country', 'sex', 'age', 'half.child.variant'),
					 pred=c(),
					 table.decimal=0))
}

get.pop.table.title <- function(country, pred) 
	return (country)

pop.show.pyramid.group <- function(g, main.win, parent.env) {
	e <- new.env()
	e$sim.dir <- parent.env$sim.dir
	e$pred.type <- 'pop'
	defaults.pred <- formals(pop.predict)
	defaults.pyr <- formals(pop.pyramid)
	defaults.pyr.all <- formals(pop.pyramid.all)
	defaults.traj.pyr <- formals(pop.trajectories.pyramid)
		
	country.f <- gframe("<span color='blue'>Country settings</span>", markup=TRUE, 
									horizontal=FALSE, container=g)
	e$show.pyr.country <- create.country.widget(country.f, defaults.pyr.all, 
									main.win, prediction=TRUE, parent.env=e, disable.table.button=FALSE)
		
	type.settings.f <- gframe("<span color='blue'>Type &amp; Time settings</span>", markup=TRUE, 
								horizontal=FALSE, container=g)
	type.g1 <- ggroup(horizontal=TRUE, container=type.settings.f)
	e$is.traj.pyr <- gcheckbox("Trajectory pyramid", container=type.g1, checked=FALSE,
						handler=function(h,...) enabled(e$nr.traj) <- svalue(h$obj))
	addSpace(type.g1, 10)
	e$proportion <- gcheckbox("Show x-axis as proportion", container=type.g1, checked=defaults.pyr$proportion)
	
	type.g2 <- ggroup(horizontal=TRUE, container=type.settings.f)
	year.gb <- gbutton(" Years ", container=type.g2,
				handler=selectYearsMenuPop,
				action=list(mw=main.win, env=e, multiple=TRUE))
	glabel(":", container=type.g2)
	#e$year <- glabel(paste(defaults.pred$present.year, "        "), container=type.settings.f)
	e$year <- gedit(defaults.pred$present.year, width=10, container=type.g2)	
	addSpace(type.g2, 10)
	glabel('Highest age category:', container=type.g2)
	e$age <- gedit(defaults.pyr$age[length(defaults.pyr$age)], width=3, container=type.g2)

	#type.g2 <- ggroup(horizontal=TRUE, container=type.settings.f)
	#e$proportion <- gcheckbox("Show as proportion", container=type.g2, checked=TRUE)
						
	traj.settings.f <- gframe("<span color='blue'>Uncertainty settings</span>", markup=TRUE, 
								horizontal=TRUE, container=g)
	glabel('CI (%):', container=traj.settings.f)
	e$pi <- gedit('80, 95', width=7, container=traj.settings.f)

	addSpace(traj.settings.f, 10)
	glabel('# trajectories:', container=traj.settings.f)
	e$nr.traj <- gedit(20, width=5, container=traj.settings.f)
	enabled(e$nr.traj) <- svalue(e$is.traj.pyr)
	addSpring(g)
	button.g <- ggroup(horizontal=TRUE, container=g)
	create.help.button(topic='pop.pyramid', package='bayesPop', parent.group=button.g,
						parent.window=main.win)
	addSpring(button.g)
	
	gbutton('Generate Script', container=button.g, handler=show.pop.pyramid, 
								action=list(mw=main.win, env=e, script=TRUE))
	addSpace(button.g, 5)
	GraphB.show.pyr.act <- gaction(label='Graph', icon='lines', handler=show.pop.pyramid, 
						action=list(mw=main.win, env=e, script=FALSE))
	gbutton(action=GraphB.show.pyr.act, container=button.g)
	return(e)
}

show.pop.pyramid <- function(h, ...) {
	e <- h$action$env
	if(!has.required.arguments(list(sim.dir='Simulation directory'), env=e)) return()
	country.pars <- get.country.code.from.widget(e$show.pyr.country$country.w, e$show.pyr.country, 
							force.country.spec=FALSE)
	if(is.null(country.pars)) return(NULL)
	traj.pyramid <- svalue(e$is.traj.pyr)
	param.names.all <- list(text='sim.dir', numvector=c('pi', 'year'), 
							numeric='age', logical='proportion')
	if(traj.pyramid) param.names.all$numeric <- c(param.names.all$numeric, 'nr.traj')

	param.env <- get.parameters(param.names.all, env=e, quote=h$action$script)
	param.env.rest <- list(country=country.pars$code, output.dir=country.pars$output.dir,
							output.type=country.pars$output.type, verbose=TRUE)
	param.env <- c(param.env, 
					get.parameters(list(text=c('output.dir', 'output.type'), 
										logical='verbose', numeric='country'), 
									param.env.rest, quote=TRUE,
									retrieve.from.widgets=FALSE))
	param.env$age <- 1:param.env$age
	param.pred <- param.env['sim.dir']
	
	pred <- do.call('get.pop.prediction', param.pred)
	if(h$action$script) {
		cmd <- paste('pred <- get.pop.prediction(', 
					paste(paste(names(param.pred), param.pred, sep='='), collapse=', '), 
						')\n', sep='')
	} else {	
		cmd <- ''
	}
	param.plot1c <- param.env[c('pi', 'age', 'year', 'proportion')]
	if(traj.pyramid) {
		param.plot1c <- c(param.plot1c, param.env['nr.traj'])
		pyr.command <- 'pop.trajectories.pyramid'
	} else {
		pyr.command <- 'pop.pyramid'
	}
		
	if(is.element('country', names(param.env))) param.plot1c <- c(param.plot1c, param.env['country'])
	if(!is.null(param.env$country)) { # one country
		years <- param.env$year
		if (h$action$script) {
			script.text <- gwindow('bayesPop commands', parent=h$action$mw)
			for(year in years) {
				param.plot1c$year <- year
				cmd <- paste(cmd, pyr.command, '(pred,', 
					paste(paste(names(param.plot1c), param.plot1c, sep='='), collapse=', '), ',)\n')
			}
			gtext(cmd, container=script.text)
		} else {
			base.cmd <- cmd
			for(year in years) {
				param.plot1c$year <- year
				cmd <- paste(base.cmd, pyr.command, '(pred,', 
					paste(paste(names(param.plot1c), param.plot1c, sep='='), collapse=', '), ',)\n')
				create.graphics.window(parent=h$action$mw, title=paste("Pyramid for", country.pars$name), 
										ps=8, width=700, height=500)
				eval(parse(text=cmd))
			}
		}
	} else { # all countries
		param.plot.allc <- param.env[c(names(param.plot1c), 'output.dir', 'output.type',  'verbose')]
		cmd <- paste(cmd, pyr.command, '.all(pred, ',
					paste(paste(names(param.plot.allc), param.plot.allc, sep='='), collapse=', '), sep='')
		cmd <- paste(cmd, ')', sep='')
		if (h$action$script) {
			script.text <- gwindow('bayesPop commands', parent=h$action$mw)
			gtext(cmd, container=script.text)
		} else {
			eval(parse(text=cmd))
		}
	}
}

selectAgeMenuPop <- function(h, ...) {
	age.selected <- function(h1, ...) {
		age <- svalue(h$action$env$age.gt)
		h$action$env$age <- if (is.element(0, age)) 'all' else age 
		visible(h$action$env$age.sel.win) <- FALSE
	}
	if (!is.null(h$action$env$age.sel.win)) 
		visible(h$action$env$age.sel.win) <- TRUE
	else {
		ages <- c('All', bayesPop:::get.age.labels(seq(0, by=5, length=27)))
		h$action$env$age.table <- data.frame(Index=seq(0,length=length(ages)), Age=ages)
		h$action$env$age.sel.win <- win <- gwindow('Select ages', 
							parent=h$action$mw, height=450, width=200,
							handler=function(h, ...) {
								h$action$env$age.sel.win<-NULL;
								h$action$env$age.ok.handler <- NULL
							},
							action=list(env=h$action$env))
		t.group <- ggroup(horizontal=FALSE, container=win)
		h$action$env$age.gt <- gtable(h$action$env$age.table, container=t.group, 
					expand=TRUE, multiple=h$action$multiple, handler=age.selected)
		b.group <- ggroup(horizontal=TRUE, container=t.group)
		gbutton('Cancel', container=b.group, handler=function(h, ...) 
					visible(win) <- FALSE)
		addSpring(b.group)
		h$action$env$age.okbutton <- gbutton('OK', container=b.group)
	}
	if(!is.null(h$action$env$age.ok.handler)) 
		removehandler(h$action$env$age.okbutton, h$action$env$age.ok.handler)
	h$action$env$age.ok.handler <- addhandlerclicked(
						h$action$env$age.okbutton, handler=age.selected)

}
selectYearsMenuPop <- function(h, ...) {
	year.selected <- function(h1, ...) {
		years <- svalue(h$action$env$year.gt)
		svalue(h$action$env$year) <- paste(years, collapse=',')
		visible(h$action$env$year.sel.win) <- FALSE
	}
	if (!is.null(h$action$env$year.sel.win)) 
		visible(h$action$env$year.sel.win) <- TRUE
	else {
		if(!has.required.arguments(list(sim.dir='Simulation directory'), env=h$action$env)) return()
		param <-list(sim.dir=svalue(h$action$env$sim.dir))
		pred <- do.call('get.pop.prediction', param)
		if(is.null(pred)) {
			gmessage('Simulation directory contains no valid population projection.', 
						title='Input Error', icon='error')
        	return(NULL)
		}
		h$action$env$year.table <- data.frame(Mid.year=pred$proj.years)
		h$action$env$year.sel.win <- win <- gwindow('Select years', 
							parent=h$action$mw, height=450, width=100,
							handler=function(h, ...) {
								h$action$env$year.sel.win<-NULL;
								h$action$env$year.ok.handler <- NULL
							},
							action=list(env=h$action$env))
		t.group <- ggroup(horizontal=FALSE, container=win)
		h$action$env$year.gt <- gtable(h$action$env$year.table, container=t.group, 
					expand=TRUE, multiple=h$action$multiple, handler=year.selected)
		b.group <- ggroup(horizontal=TRUE, container=t.group)
		gbutton('Cancel', container=b.group, handler=function(h, ...) 
					visible(win) <- FALSE)
		addSpring(b.group)
		h$action$env$year.okbutton <- gbutton('OK', container=b.group)
	}
	if(!is.null(h$action$env$year.ok.handler)) 
		removehandler(h$action$env$year.okbutton, h$action$env$year.ok.handler)
	h$action$env$year.ok.handler <- addhandlerclicked(
						h$action$env$year.okbutton, handler=year.selected)


}
