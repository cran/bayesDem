selectDir <- function(h, ...) {
	text.widget <- h$action$text.widget
	gf <- gfile(inititalfilename=svalue(text.widget), 
			type='selectdir',
			handler=function(h2,...){svalue(text.widget)<-h2$file;
				dispose(gf)})
	
}

create.help.button <- function(topic, package, parent.group, parent.window) {
	helpaction <- gaction(label='Help', icon='help', 
		handler=function(h, ...) {
			oldhtmloption <- options(htmlhelp=FALSE);
			oldchmoption <- options(chmhelp=FALSE);
			ghelp(topic=topic, package=package, 
					cont=gwindow(parent=parent.window, width=700, height=700));
			options(htmlhelp=oldhtmloption);
			options(chmhelp=oldchmoption)
		})
	gbutton(action=helpaction, cont=parent.group)
}

create.info.button <- function(dir.widget.name, parent.group, parent.window, env) {
	infoaction <- gaction(label='Info', icon='info', handler=show.summary,
					action=list(mw=parent.window, env=env, dir.widget.name=dir.widget.name))
	gbutton(action=infoaction, cont=parent.group)
}

create.graphics.window <- function(parent, title='', dpi=80) {
	e <- new.env()
	win <- gwindow(title, parent=parent, horizontal=FALSE)
	g <- ggroup(cont=win, horizontal=FALSE, expand=TRUE)
	g1 <- ggroup(cont=g, horizontal=TRUE)
	glabel("Output type:", cont=g1)
	#types <- formals(savePlot)$type
	e$type <- gdroplist(c("pdf", "postscript", "png", "jpeg", "tiff", "bmp"), cont=g1)
	gb <- gbutton('Save', cont=g1)
	g2 <- ggroup(cont=g, horizontal=TRUE, expand=TRUE)
	ggraphics(cont=g2, ps=10, dpi=dpi)
	addHandlerClicked(gb, handler=saveGraph, action=list(mw=win, env=e, dpi=dpi, dev=dev.cur()))
	Sys.sleep(1)
	return(g)
}



saveGraph <- function(h, ...){
	e <- h$action$env
	type <- svalue(e$type)
	postfix <- list(png='png', jpeg='jpg', pdf='pdf', tiff='tiff', bmp='bmp', postscript='ps')
	filter <- list()
	filter[[paste(type, 'files')]] <- list(patterns=paste('*', postfix[[type]], sep='.'))
	gfile(type='save', quote=FALSE,
					filter=c(filter, list("All files" = list(patterns = c("*")))),
					handler=function(h1,...){
						filename <- h1$file
						pattern <- paste('[.]', postfix[[type]], '$|[.]', type, '$', sep='')
						if(length(grep(pattern, filename))==0)
							filename <- paste(filename,  postfix[[type]], sep='.')
						size <- list()
						for(measure in c('width', 'height')) {
							size[[measure]]  <- NULL
							if(is.null(e[[measure]])) {
								if(type != 'pdf' & type != 'postscript') {
									size[[measure]] <- h$action$dpi*6
								}
							} else {
								if(e[[measure]] != 'default') {
									if(!is.null(e[[measure]][[type]]))
										size[[measure]] <- e[[measure]][[type]]
									else
										size[[measure]] <- e[[measure]]
								}
							}
						}
						dev.set(h$action$dev)
						do.call('dev.print', c(list(file=filename, device=eval(parse(text=type))),
												size))
					}
			)
	
}

show.summary <- function(h, ...) {
	e <- h$action$env
	dir <- svalue(e[[h$action$dir.widget.name]])
	warn <- getOption('warn')
	options(warn=-1) # disable warning messages
	mcmc.set <- get.tfr.mcmc(dir)
	# get prediction
	pred <- get.tfr.prediction(sim.dir=dir)
	options(warn=warn)
	
	con <- textConnection("info", "w", local=TRUE)
	sink(con)
	if (is.null(mcmc.set)) {
		cat('No simulation available in this directory.')
	} else { 
		cat('Simulation results\n')
		cat('********************\n')
		print(summary(mcmc.set, meta.only=TRUE))
		cat('===============================\n')
	}
	if (!is.null(pred)) {		
		cat('\nProjections for end year:', pred$end.year)
		cat('\n------------------')
		print(summary(pred))
	}
	sink()
	close(con)
	info.win <- gwindow('Directory Info', parent=h$action$mw, width=500, height=400)
	gtext(info, cont=info.win)

}

get.parameters <- function(par.names, env, quote=FALSE, retrieve.from.widgets=TRUE) {
	# Get values from a bunch of widgets, separated into groups by the value types
	# par.names is a list of vectors with elements: 'numeric', 'numvector', 'text', 'numtext' etc.
	# Each vector contains the names of the corresponding widgets in the environment 'env'. 
	params <- list()
	op <- options("useFancyQuotes")
	options(useFancyQuotes = FALSE)
	for (par in unlist(par.names)) {
		params[[par]] <- if(retrieve.from.widgets) svalue(env[[par]]) else env[[par]]
		if(is.null(params[[par]])) next
		if (nchar(params[[par]])==0) {params[[par]] <- NULL; next}
		if(any(par == par.names$numeric)) params[[par]] <- as.numeric(params[[par]])
		if(any(par == par.names$numvector)) params[[par]] <- as.numeric(strsplit(params[[par]], ',')[[1]])
		if (quote & any(par == par.names$text)) params[[par]] <- sQuote(params[[par]])
		if(any(par == par.names$numtext)) {
			warn <- getOption('warn')
			options(warn=-1)
			numpar <- as.numeric(params[[par]])
			options(warn=warn)
			if (!is.na(numpar)) {params[[par]] <- numpar}
			else {if (quote) params[[par]] <- sQuote(params[[par]])}
		}
	}
	options(op)
	return(params)
}

has.required.arguments <- function(par.names, env) {
	for (par in names(par.names)) {
		value <- svalue(env[[par]])
		if (nchar(value)==0) {
			gmessage(paste('Argument', par.names[[par]], 'is required.'))
			return(FALSE)
		}
	}
	return(TRUE)
}

makeDFView <- function(df, container, f=NULL, ...) {
	model <- rGtkDataFrame(df)
	view <- gtkTreeView(model)
	## Michael Lawrence's trick
	mapply(view$insertColumnWithAttributes,-1, colnames(model), 
		lapply(1:ncol(model), function(i) gtkCellRendererText()), text = seq_len(ncol(model)) - 1)

	sw <- gtkScrolledWindow()
	sw$add(view)
	
	coerceVar <- function(x, value) UseMethod("coerceVar")
	coerceVar.default <- function(x, value) value
	coerceVar.numeric <- function(x, value) as.numeric(value)
	coerceVar.integer <- function(x, value) as.integer(value)
	coerceVar.logical <- function(x, value) as.logical(value)
	coerceVar.factor <- function(x, value) {
		ind <- pmatch(value, levels(x))
		if(is.na(ind))
			ind
		else
			levels(x)[ind]
	}

	sapply(1:ncol(model), function(j) {
		col <- view$getColumn(j-1)
		#print(col)
		cr <- col$getCellRenderers()[[1]]
		cr['editable'] <- TRUE
		gSignalConnect(cr, "edited", 
			f=function(cr, path, newtext, user.data) {
				if(!is.null(f)) f(...) else NULL
				curRow <- as.numeric(path) + 1
				curCol <- user.data$column
				model <- user.data$model
				## coerce newtext from character to desired type
				## otherwise this coerces to character
				model[curRow, curCol] <- coerceVar(model[,curCol], newtext)
			}, data=list(model=model, column=j))
	})

	add(container, sw, expand=TRUE)
	list(model=model, view=view)
}
