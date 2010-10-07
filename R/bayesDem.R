bayesDem.go <- function(wpp.year=wpp.year.default) {
		
	quit.bayesdem <- function(h, ...) {
		dispose(main.win)
	}
	options(guiToolkit=guiToolkit.default)
	# 
	wait.window <- gwindow('Bayesian Demographer Initialization', width=400, height=100,
						parent=c(500, 300))
	glabel('Starting Bayesian Demographer ...', cont=wait.window)
	
	# main window
	main.win <<- gwindow(paste('Bayesian Demographer  v.', 
			installed.packages()["bayesDem", "Version"]), visible=FALSE, parent=c(400,150))

	main.g <- ggroup(horizontal=FALSE, cont=main.win)
	
	# notebook with tabs
	main.notebook <- gnotebook(cont=main.g, expand=TRUE)

	# TFR prediction tab
	tfr.pred <- ggroup(label="<span weight='bold' color='blue'>Projection of Total Fertility Rate</span>", 
		markup=TRUE, horizontal=FALSE, cont=main.notebook)

	tfrPredTab(tfr.pred, main.win, wpp.year=wpp.year)

	# Not implemented tabs
	# TFR life expectancy
	e0w <- ggroup(label="<span weight='bold' color='blue'>Projection of Life Expectancy</span>", 
		markup=TRUE, horizontal=FALSE, cont=main.notebook)
	e0PredTab(e0w, main.win)

	svalue(main.notebook)<- 1
	
	# Quit Button
	button.group <- ggroup(cont=main.g, expand=FALSE)
	gbutton('Quit', handler=quit.bayesdem, cont=button.group)
	dispose(wait.window)
	visible(main.win) <- TRUE

}

