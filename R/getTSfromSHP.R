getTSfromSHP <- function(obj, lat = NA, lon = NA, ADM = 2) {
    if (ADM == 2) {
        eumap = readOGR(system.file("NUTS", "NUTS_REG_01M_2013_REGIONS", package = "enear"))
    } else {
        eumap = readOGR(system.file("NUTS", "NUTS_REG_01M_2013_ADM1", package = "enear"))
    }

    if (!is.list(obj)) {
        if (is.na(lat) || is.na(lon)) {
            stop("You need to specify lat and lon vector in case of not-ECOMS objects")
        }
        pts = expand.grid(lat = lat, lon = lon)
        pts_index = expand.grid(lat = seq(1, length(lat)), lon = seq(1, length(lon)))
    } else {
        pts = expand.grid(lat = obj$xyCoords$y, lon = obj$xyCoords$x)
        pts_index = expand.grid(lat = seq(1, length(obj$xyCoords$y)), lon = seq(1, length(obj$xyCoords$x)))
        lat = obj$xyCoords$y
        lon = obj$xyCoords$x
        obj = obj$Data
    }

    coordinates(pts) = c("lon", "lat")
    proj4string(pts) = proj4string(eumap)

    over_target = over(pts, as(eumap, "SpatialPolygons"))
    pts$region = eumap$NUTS_ID[over_target]

    pts_index$region = droplevels(eumap$NUTS_ID[over_target])
    pts_index = pts_index[!is.na(over_target), ]

    SEL_REGIONS = levels(pts_index$region)
    data = list()
    for (REG in SEL_REGIONS) {
        sel_pts = pts_index[pts_index$region == REG, c(1, 2)]
        lsel = vector("list", nrow(sel_pts))
        for (i in 1:nrow(sel_pts)) {
            if (length(dim(obj)) == 3) {
                lsel[[i]] = obj[, sel_pts$lat[i], sel_pts$lon[i]]
            } else {
                lsel[[i]] = t(obj[, , sel_pts$lat[i], sel_pts$lon[i]])
            }
        }
        lsel = do.call("cbind", lsel)
        if (length(dim(obj)) == 3) {
            d = rowMeans(lsel, na.rm = T)
        } else {
            nmem = dim(obj)[1]
            d = matrix(NA, nrow = nrow(lsel), ncol = nmem)
            for (k in 1:nmem) {
                d[, k] = rowMeans(matrix(lsel[, seq(k, ncol(lsel), nmem)], nr = nrow(lsel)), na.rm = T)
            }
        }
        data[[REG]] = d
    }
    return(data)
}
