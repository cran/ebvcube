% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ebv_create_taxonomy.R
\name{ebv_create_taxonomy}
\alias{ebv_create_taxonomy}
\title{Create an EBV netCDF with taxonomy}
\usage{
ebv_create_taxonomy(
  jsonpath,
  outputpath,
  taxonomy,
  taxonomy_key = FALSE,
  epsg = 4326,
  extent = c(-180, 180, -90, 90),
  resolution = c(1, 1),
  timesteps = NULL,
  fillvalue,
  prec = "double",
  sep = ",",
  force_4D = TRUE,
  overwrite = FALSE,
  verbose = TRUE
)
}
\arguments{
\item{jsonpath}{Character. Path to the json file downloaded from the EBV Data
Portal. Login to the page and click on 'Uploads' and 'New Upload' to start
the process.}

\item{outputpath}{Character. Set path where the netCDF file should be created.}

\item{taxonomy}{Character. Path to the csv table holding the taxonomy.
Default: comma-separated delimiter, else change the \code{sep} argument
accordingly. The csv needs to have the following structure: The header
displays the names of the different taxonomy levels ordered from the highest
level to the lowest, e.g. "order", "family", "genus", "scientificName". We
strongly encourage the usage of the \href{https://dwc.tdwg.org/terms/#taxon}{Darwin
Core terminology} for the taxonomy levels. The last column (if \code{taxonomy_key}=FALSE)
is equivalent to the \code{entity} argument in the \code{\link[=ebv_create]{ebv_create()}}
function. Each row of the csv corresponds to a unique entity. In case the
\code{taxonomy_key} argument (see below) is set to TRUE, this table gets an
additional last column which holds the taxonomy key per entity - in this
case the second last column contains the entity names, e.g. the following
column order: "order", "family", "genus", "scientificName", "taxonomy_key".
The last column (here named "taxonomy_key") should have the exact name of
the taxonomy key from the authority of the taxonomic backbone. It will be
added as an additional attribute to the netCDF. For example, if your
taxonomy is based on the \href{https://www.gbif.org/dataset/d7dddbf4-2cf0-4f39-9b2a-bb099caae36c}{GBIF
Backbone Taxonomy} the column name could be "usageKey". For an
example CSV including the \code{taxonomy_key} download the 'Entities as CSV'
from the \href{https://portal.geobon.org/ebv-detail?id=82}{Occurrence
Metrics for the Birds Directive Annex I Species in EU27 dataset} of the
portal. For an example without \code{taxonomy_key}, download
\href{https://portal.geobon.org/ebv-detail?id=84}{Species habitat
suitability of European terrestrial vertebrates for contemporary climate
and land use}.
To create your netCDF follow the same structure. The column names may be
different depending on the taxonomy used.}

\item{taxonomy_key}{Logical. Default: FALSE. Set to TRUE if the last column in
your taxonomy csv file defines the taxonomy_key for each entity.}

\item{epsg}{Integer. Default: 4326 (WGS84). Defines the coordinate reference
system via the corresponding epsg code.}

\item{extent}{Numeric. Default: c(-180,180,-90,90). Defines the extent of the
data: c(xmin, xmax, ymin, ymax).}

\item{resolution}{Numerical. Vector of two numerical values defining the
longitudinal and latitudinal resolution of the pixel: c(lon,lat).}

\item{timesteps}{Character. Vector of the timesteps in the dataset. Default:
NULL - in this case the time will be calculated from the start-, endpoint
and temporal resolution given in the metadata file (json). Else, the dates
must be given in in ISO format 'YYYY-MM-DD' or shortened 'YYYY' in case of
yearly timesteps.}

\item{fillvalue}{Numeric. Value of the missing data (NoData value) in the
array. Has to be a single numeric value or NA.}

\item{prec}{Character. Default: 'double'. Precision of the data set. Valid
options: 'short' 'integer' 'float' 'double' 'char' 'byte'.}

\item{sep}{Character. Default: ','. If the delimiter of the csv specifying the
entity-names differs from the default, indicate here.}

\item{force_4D}{Logical. Default is TRUE. If the argument is TRUE, there will
be 4D cubes (lon, lat, time, entity) per metric. If this argument is changed
to FALSE, there will be 3D cubes (lon, lat, time) per entity (per metric).
So the latter yields a higher amount of cubes and does not bundle all
information per metric. In the future the standard will be restricted to the
4D version. Recommendation: go with the 4D cubes!}

\item{overwrite}{Logical. Default: FALSE. Set to TRUE to overwrite the output
file defined by 'outputpath'}

\item{verbose}{Logical. Default: TRUE. Turn off additional prints by setting
it to FALSE.}
}
\value{
Creates the netCDF file at the 'outputpath' location including the
taxonomy information.
}
\description{
Create the core structure of the EBV netCDF based on the json
from the \href{https://portal.geobon.org}{EBV Data Portal}. Additionally,
you can add the hierarchy of the taxonomy. This is not provided in the
\code{\link[=ebv_create]{ebv_create()}} function. Use the \code{\link[=ebv_create]{ebv_create()}} function
if your dataset holds no taxonomic information. Data will be added
afterwards using \code{\link[=ebv_add_data]{ebv_add_data()}}.
}
\note{
To check out the results take a look at your netCDF file with
\href{https://www.giss.nasa.gov/tools/panoply/}{Panoply} provided by the
NASA.

You can check the taxonomy info with \code{\link[=ebv_properties]{ebv_properties()}} in the
slot 'general' under the name 'taxonomy' and 'taxonomy_key'.
}
\examples{
#set path to JSON file
json <- system.file(file.path("extdata/testdata","5.json"), package="ebvcube")
#set output path of the new EBV netCDF
out <-  tempfile(fileext='.nc')
#set path to the csv holding the taxonomy names
taxonomy <- file.path(system.file(package='ebvcube'),"extdata/testdata","id5_entities.csv")

#create new EBV netCDF with taxonomy
\dontrun{
ebv_create_taxonomy(jsonpath = json, outputpath = out, taxonomy = taxonomy,
           fillvalue = -127, resolution = c(0.25, 0.25), verbose = FALSE)
#remove file
file.remove(out)
}
}
