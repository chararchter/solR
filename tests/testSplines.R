function = drawSplines(datMeteo){
###########################
#splines
###########################

intrval1 = c(as_datetime("2019-01-19 06:00:00 UTC"), as_datetime("2019-01-19 07:00:00 UTC") + hours(110))

# how many entries are there in intrval1? needed to decide nknots, which determines smoothing
intrRow = nrow(datMeteo %>%  filter(between(timestamp, min(intrval1), max(intrval1))))

spl_Meteo1 = spline(datMeteo$timestamp, datMeteo$solarIrradiance)
# spl_Meteo1 = smooth.spline(datMeteo$timestamp, datMeteo$solarIrradiance,spar = 1e-7, tol = 1e-2, all.knots = TRUE)
spl_Meteo2 = smooth.spline(datMeteo$timestamp, datMeteo$solarIrradiance,spar = 1e-7, tol = 1e-3, all.knots = TRUE)
spl_Meteo3 = smooth.spline(datMeteo$timestamp, datMeteo$solarIrradiance,spar = 1e-7, tol = 1e-3, nknots = floor(0.45*intrRow))
spl_Meteo4 = smooth.spline(datMeteo$timestamp, datMeteo$solarIrradiance,spar = 1e-7, tol = 1e-3, nknots = floor(0.35*intrRow))
# spl_Meteo4 = smooth.spline(datMeteo$timestamp, datMeteo$solarIrradiance,spar = 1e-7, tol = 1e-3, nknots = intrRow-10)

spl_Meteo1 = data.frame(spl_Meteo1)
spl_Meteo1$x <- lubridate::as_datetime(spl_Meteo1$x)
spl_Meteo1 = dplyr::arrange(spl_Meteo1, x)

spl_Meteo2 = data.frame(x = spl_Meteo2$x, y = spl_Meteo2$y)
spl_Meteo2$x <- lubridate::as_datetime(spl_Meteo2$x)
spl_Meteo2 = dplyr::arrange(spl_Meteo2, x)

spl_Meteo3 = data.frame(x = spl_Meteo3$x, y = spl_Meteo3$y)
spl_Meteo3$x <- lubridate::as_datetime(spl_Meteo3$x)
spl_Meteo3 = dplyr::arrange(spl_Meteo3, x)

spl_Meteo4 = data.frame(x = spl_Meteo4$x, y = spl_Meteo4$y)
spl_Meteo4$x <- lubridate::as_datetime(spl_Meteo4$x)
spl_Meteo4 = dplyr::arrange(spl_Meteo4, x)

plot0 <- datMeteo %>%
    ggplot() +
    geom_point(aes(x = datMeteo$timestamp, y = datMeteo$solarIrradiance)) +
    ylab("Data") + coord_cartesian(xlim = intrval1) +
    theme_minimal() +
    theme(axis.title.x = element_blank())

plot1 <- spl_Meteo1 %>%
    ggplot() +
    geom_line(aes(x = x, y = y), size = 0.5, alpha = 0.75) +
    ylab("Spline") + coord_cartesian(xlim = intrval1) +
    theme_minimal() +
    theme(axis.title.x = element_blank())

plot2 <- spl_Meteo2 %>%
    ggplot() +
    geom_line(aes(x = x, y = y), size = 0.5, alpha = 0.75) +
    ylab("Smoothing spline all knots") + coord_cartesian(xlim = intrval1) +
    theme_minimal() +
    theme(axis.title.x = element_blank())

plot3 <- spl_Meteo3 %>%
    ggplot() +
    geom_line(aes(x = x, y = y), size = 0.5, alpha = 0.75) +
    ylab("Smoothing spline nknots = 45% all knots") + coord_cartesian(xlim = intrval1) +
    theme_minimal() +
    theme(axis.title.x = element_blank())

plot4 <- spl_Meteo4 %>%
    ggplot() +
    geom_line(aes(x = x, y = y), size = 0.5, alpha = 0.75) +
    ylab("Smoothing spline nknots = 35% all knot") + coord_cartesian(xlim = intrval1) +
    theme_minimal() +
    theme(axis.title.x = element_blank())

##########################
# cubic spline doesnt work. datetime seems at fault. fix it.
# pp = cubicspline (datMeteo$timestamp , datMeteo$solarIrradiance )
# ppfun = function ( xspl ) ppval (pp , xspl )
# # xspline = seq (0 ,150 ,0.5)
# xspl = seq.POSIXt(from=as_datetime(min(intrval1)), to=as_datetime(max(intrval1)), by="hour")
# cubicspline (datMeteo$timestamp , datMeteo$solarIrradiance , xi = xspl )
# yspl = cubicspline (datMeteo$timestamp , datMeteo$solarIrradiance , xspl )

# plot5 <- ggplot() +
#     geom_line(aes(x = xspl, y = yspl), size = 0.5, alpha = 0.75) +
#     ylab("Cubic spline") + coord_cartesian(xlim = intrval1) +
#     theme_minimal() +
#     theme(axis.title.x = element_blank())
##########################

pdf('testSplines.pdf',
    width=8,
    height=12)
grid.arrange(
    arrangeGrob(plot0,plot2,plot3,plot4,nrow=4,heights=c(.25,.25,.25,.25))
)
dev.off()

}