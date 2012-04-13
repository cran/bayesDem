bayesDem.go <- function(wpp.year.tfr=tfr.wpp.year.default, wpp.year.e0=e0.wpp.year.default,
						wpp.year.pop=pop.wpp.year.default) {
		
	quit.bayesdem <- function(h, ...) {
		dispose(main.win)
	}
	options(guiToolkit=guiToolkit.default)
	# 
	wait.window <- gwindow('Bayesian Demographer Initialization', width=400, height=100,
						parent=c(500, 300))
	glabel('Starting Bayesian Demographer ...', container=wait.window)
	
	# main window
	main.win <<- gwindow(paste('Bayesian Demographer  v.', 
			packageVersion("bayesDem")), visible=FALSE, parent=c(400,150))

	main.g <- ggroup(horizontal=FALSE, container=main.win)
	
	# notebook with tabs
	main.notebook <- gnotebook(container=main.g, expand=TRUE)

	# TFR prediction tab
	tfr.pred <- ggroup(label="<span weight='bold' color='blue'>Projection of Total Fertility Rate</span>", 
		markup=TRUE, horizontal=FALSE, container=main.notebook)

	tfrPredTab(tfr.pred, main.win, wpp.year=wpp.year.tfr)

	# Life expectancy
	e0w <- ggroup(label="<span weight='bold' color='blue'>Projection of Life Expectancy</span>", 
		markup=TRUE, horizontal=FALSE, container=main.notebook)
	e0PredTab(e0w, main.win, wpp.year=wpp.year.e0)
	
	# Population Prediction
	pop.w <- ggroup(label="<span weight='bold' color='blue'>Population Projection</span>", 
		markup=TRUE, horizontal=FALSE, container=main.notebook)
	popPredTab(pop.w, main.win, wpp.year=wpp.year.pop)

	svalue(main.notebook)<- 1
	
	# Quit Button
	button.group <- ggroup(container=main.g, expand=TRUE)
	gbutton('Quit', handler=quit.bayesdem, container=button.group)
	addSpring(button.group)
	label <- glabel('BayesPop group\nUniversity of Washington', container=button.group)
	font(label) <- c(style='normal', family='serif', size='xx-small')
	dispose(wait.window)
	visible(main.win) <- TRUE

}

