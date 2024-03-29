% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ebv_read_bb.R
\name{ebv_read_bb}
\alias{ebv_read_bb}
\title{Read subset (bounding box) of one datacube of an EBV netCDF}
\usage{
ebv_read_bb(
  filepath,
  datacubepath,
  entity = NULL,
  timestep = 1,
  bb,
  outputpath = NULL,
  epsg = 4326,
  overwrite = FALSE,
  ignore_RAM = FALSE,
  verbose = TRUE
)
}
\arguments{
\item{filepath}{Character. Path to the netCDF file.}

\item{datacubepath}{Character. Path to the datacube (use
\code{\link[=ebv_datacubepaths]{ebv_datacubepaths()}}).}

\item{entity}{Character or Integer. Default is NULL. If the structure is 3D,
the entity argument is set to NULL. Else, a character string or single
integer value must indicate the entity of the 4D structure of the EBV
netCDFs.}

\item{timestep}{Integer. Choose one or several timesteps.}

\item{bb}{Integer Vector. Definition of subset by bounding box: c(xmin,
xmax, ymin, ymax).}

\item{outputpath}{Character. Default: NULL, returns the data as a SpatRaster
object in memory. Optional: set path to write subset as GeoTiff on disk.}

\item{epsg}{Integer. Default: 4326 (WGS84). Change accordingly if your
bounding box coordinates are based on a different coordinate reference
system.}

\item{overwrite}{Logical. Default: FALSE. Set to TRUE to overwrite the
outputfile defined by 'outputpath'.}

\item{ignore_RAM}{Logical. Default: FALSE. Checks if there is enough space in
your memory to read the data. Can be switched off (set to TRUE).}

\item{verbose}{Logical. Default: TRUE. Turn off additional prints by setting
it to FALSE.}
}
\value{
Returns a SpatRaster object if no outputpath is given. Otherwise the
subset is written onto the disk and the outputpath is returned.
}
\description{
Read a subset of one or more layers from one datacube of the
NetCDF file. Subset definition by a bounding box.
}
\note{
In case the epsg of the Bounding Box and the netCDF differ, the data is
returned based on the epsg of the netCDF Dataset.
}
\examples{
#set path to EBV netCDF
file <- system.file(file.path("extdata","martins_comcom_subset.nc"), package="ebvcube")
#get all datacubepaths of EBV netCDF
datacubes <- ebv_datacubepaths(file, verbose=FALSE)

#set outputpath
out <- file.path(system.file(package='ebvcube'),"extdata","subset_bb.tif")
#define two different bounding boxes based on different EPSG codes
bb_wgs84 <- c(-26, 64, 30, 38)
bb_utm32 <- c(-2383703, 5532302, 3643854, 4564646)


\dontrun{
#read bb (based on EPSG 4326) - return SpatRaster
cSAR_subset <- ebv_read_bb(filepath = file, datacubepath = datacubes[1,1],
                             entity = 1, timestep = 1:3, bb = bb_wgs84)

#read bb (based on EPSG 4326) - write to GeoTiff
path <- ebv_read_bb(filepath = file, datacubepath = datacubes[1,1],
                    entity = 1, timestep = 1, bb = bb_wgs84,
                    outputpath = out, overwrite = TRUE)

#read bb (based on ESRI 54009) - write to GeoTiff
path  <- ebv_read_bb(filepath = file, datacubepath = datacubes[1,1],
                     entity = 1, timestep = 1:2, bb = bb_utm32,
                     epsg = 32632, outputpath = out, overwrite = TRUE)
}
}
\seealso{
\code{\link[=ebv_read_shp]{ebv_read_shp()}} for subsetting via shapefile.
}
