# https://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when#comment20826625_12429344
# 2012 hadley says "globalVariables is a hideous hack and I will never use it"
# 2014 hadley updates his own answer with globalVariables as one of "two solutions"
globalVariables(c('year', 'value', 'Country.Name', 'Country.Code', 'Indicator.Name', 'Indicator.Code'))

#' WDI: World Development Indicators (World Bank)
#' 
#' Downloads the requested data by using the World Bank's API, parses the
#' resulting XML file, and formats it in long country-year format. 
#' 
#' @param country Vector of countries (ISO-2 character codes, e.g. "BR", "US",
#'     "CA") for which the data is needed. Using the string "all" instead of
#'     individual iso codes pulls data for every available country.
#' @param indicator Character vector of indicators codes. See the WDIsearch()
#' function. If you supply a named vector, the indicators will be automatically
#' renamed: `c('women_private_sector' = 'BI.PWK.PRVS.FE.ZS')`
#' @param start Start date, usually a year in integer format. Must be 1960 or
#' greater.
#' @param end End date, usually a year in integer format. Must be greater than
#' the `start` argument.
#' @param extra TRUE returns extra variables such as region, iso3c code, and
#'     incomeLevel.
#' @param cache NULL (optional) a list created by WDIcache() to be used with the extra=TRUE argument.
#' @param latest Integer indicating the number of most recent non-NA values to get. Default is NULL. If specified, it overrides the start and end dates.
#' @param language ISO-2 code in lower case indicating in which language the characters should be provided. List of languages available with `WDI::languages_supported()`. Default is English.
#'     
#'     
#' @details It is possible to only specify the `indicator` and the `country` arguments, in which case `WDI()` will return data from 1960 to the last year available on World Bank's website. 
#' 
#' It is also possible to get only the most recent non-NA values, with `latest`.
#' 
#' @return Data frame with country-year observations. You can extract a
#' data.frame with indicator names and descriptive labels by inspecting the
#' `label` attribute of the resulting data.frame: `attr(dat, 'label')`
#' @author Vincent Arel-Bundock \email{vincent.arel-bundock@umontreal.ca}
#' @importFrom RJSONIO fromJSON
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' WDI(country="all", indicator=c("AG.AGR.TRAC.NO","TM.TAX.TCOM.BC.ZS"),
#'     start=1990, end=2000)
#' WDI(country=c("US","BR"), indicator="NY.GNS.ICTR.GN.ZS", start=1999, end=2000,
#'     extra=TRUE, cache=NULL)
#'
#' # Rename indicators on the fly
#' WDI(country = 'CA', indicator = c('women_private_sector' = 'BI.PWK.PRVS.FE.ZS',
#'                                   'women_public_sector' = 'BI.PWK.PUBS.FE.ZS'))
#'                                   
#' # Get the 5 latest non-NA values
#' WDI(country=c("US","BR"), indicator="NY.GNS.ICTR.GN.ZS", latest = 5)
#' }
#'
WDI <- function(country = "all", 
                indicator = "NY.GDP.PCAP.KD",
                start = 1960, 
                end = 2020, 
                extra = FALSE, 
                cache = NULL,
                latest = NULL,
                language = "en"){


    # Sanity: country
    if (!is.character(country)) {
        stop('The `country` argument must be a character vector')
    }

    country <- gsub('[^a-zA-Z0-9]', '', country)
    country_good <- unique(c(WDI::WDI_data$country[,'iso3c'], 
                             WDI::WDI_data$country[,'iso2c']))
    country_good <- c('all', country_good)
    country_bad <- base::setdiff(country, country_good)
    country <- base::intersect(country, country_good)

    if (length(country) == 0) {
        stop('None of the countries requested are valid. Please use ISO-2, ISO-3, or World Bank regional codes.')
    }
    if (length(country_bad) > 0) {
        warning('Please use ISO-2, ISO-3, or World Bank regional codes. Some of the country codes that you requested are invalid: ',  
                paste(country_bad, collapse = ', '))
    }

    # Sanity: start & end
    is_integer <- function(x) is.numeric(x) && (x %% 1 == 0)
    
    # If latest is specified then the good order of dates doesn't matter
    if (!is.null(start) && !is.null(end) && is.null(latest)) {
      if(!(start <= end)){
        stop('`end` must be equal to or greater than `start`.')
      }
      if (is_integer(start) && (start < 1960)) {
        stop('`start` must be equal to or greater than 1960')
      }
    }
    
    # Sanity: needs dates or number of most recent values (but not both)
    if (is.null(start) && is.null(end) && is.null(latest)) {
      stop("Need to specify dates or number of latest values.")
    }
    
    
    # "Language" option (placed here and not in wdi.query because
    # otherwise tryCatch doesn't show the expected error message)
    # get the two first letters (iso code)
    supported_fully <- substr(languages_supported()$fully, 
                              start = 1, 
                              stop = 2)
    supported_locally <- substr(languages_supported()$locally, 
                                start = 1, 
                                stop = 2)
    if (is.null(language)) {
      language <- "en"
    } else {
      if (language %in% supported_locally) {
        warning("This language is only supported partially.")
      } else if (!(language %in% supported_locally ||  
                   language %in% supported_fully)) {
        stop(paste0("This language is not supported. Run ",
                    "WDI::languages_supported() to have a list of ",
                    "fully and partially supported languages."))
      }
    }
    

    # Download
    dat <- list()
    failed <- NULL

    for (i in indicator) {
        tmp <- tryCatch(wdi.dl(i, country, start, end, latest, language), error = function(e) e)
        if (is.null(tmp) || !inherits(tmp$data, 'data.frame') || (nrow(tmp$data) == 0)) {
            failed <- c(failed, i)
        } else {
            dat[[i]] <- tmp
        }
    }

    # Sanity: downloaded data
    if (length(failed) > 0) {

        msg <- sprintf(
'The following indicators could not be downloaded: %s.

Please make sure that you are running the latest version of the `WDI` package, and that the arguments you are using in the `WDI()` function are valid.

Sometimes, downloads will suddenly stop working, even if nothing has changed in the R code of the WDI package. ("The same WDI package version worked yesterday!") In those cases, the problem is almost certainly related to the World Bank servers or to your internet connection.

You can check if the World Bank web API is currently serving the indicator(s) of interest by typing a URL of this form in your web browser:

%s',
paste(failed, collapse = ", "),
wdi.query(indicator = failed[1])[1])

        if (length(failed) == length(indicator)) {
            stop(msg)
        } else if (length(failed) > 0) {
            warning(msg)
        }
    }

    # Extract labels
    lab = lapply(dat, function(x) data.frame('indicator' = x$indicator,
                                             'label' = x$label,
                                             stringsAsFactors = FALSE))
    lab = do.call('rbind', lab)

    # Extract data
    dat = lapply(dat, function(x) x$data)
    dat = Reduce(function(x,y) merge(x,y,all=TRUE), dat)
    row.names(dat) <- NULL

    # Extras
    if(!is.null(cache)){
        country_data = cache$country
    }else{
        country_data = WDI::WDI_data$country
    }
    if(extra==TRUE){
	    dat = merge(dat, country_data, all.x=TRUE)
    }
    countries = country[country != 'all' & !(country %in% dat$iso2c)]
    if(length(countries) > 0){
    }

    # Assign label attributes
    for (i in 1:nrow(lab)) {
        if (lab$indicator[i] %in% colnames(dat)) {
            attr(dat[[lab$indicator[i]]], 'label') = lab$label[[i]]
        }
    }

	# Rename columns based on indicator vector names
	if (!is.null(names(indicator))) {
		for (i in seq_along(indicator)) {
			idx = match(indicator[i], colnames(dat))
			if (!is.na(idx)) {
				colnames(dat)[idx] = names(indicator)[i]
			}
		}
	}

	# Output
  return(dat)
}

#' Download all the WDI indicators at once.
#' 
#' @return Data frame 
#' @author Vincent Arel-Bundock \email{vincent.arel-bundock@umontreal.ca}
#' @param timeout integer maximum number of seconds to wait for download
#' @return a list of 6 data frames: Data, Country, Series, Country-Series,
#' Series-Time, FootNote
#' @export
WDIbulk = function(timeout = 600) {

    # store default option
    oo <- options(timeout = timeout)

    if (!'tidyr' %in% utils::installed.packages()[, 1]) {
        stop('To use the `WDIbulk` function, you must install the `tidyr` package.')
    }

    # download
    temp_dir = tempdir()
    temp_file = tempfile(tmpdir = temp_dir)
    url = 'https://databank.worldbank.org/data/download/WDI_csv.zip'
    utils::download.file(url, temp_file)

    # read
    unzipped <- utils::unzip(zipfile = temp_file,
                             exdir = temp_dir)

    out = lapply(unzipped, function(x){
        utils::read.csv(x, stringsAsFactors = FALSE)
    })

    # flush
    unlink(temp_file)

    # names
    names(out) = c("Data", "Country", "Series",
                   "Country-Series", "Series-Time",
                   "FootNote")

    # clean "Data" entry
    out$Data$X = NULL

    out$Data = tidyr::pivot_longer(
        data = out$Data,
        cols = tidyr::starts_with("X"),
        names_to = "year",
        names_prefix = "X",
        values_to = "value"
    )

    # clean year column
    out$Data$year = as.integer(out$Data$year)

    # restore default option
    on.exit(options(oo))

    # output
    return(out)
}

#' Internal function to build API call
#'
#' @export
#' @keywords internal
wdi.query = function(indicator = "NY.GDP.PCAP.CD", 
                     country = 'all', 
                     start = 1960, 
                     end = 2020,
                     latest = NULL,
                     language = "en") {

    country <- paste(country, collapse = ';')
    
    # "Latest" option
    if (!is.null(latest)) {
      latest <- paste0("&mrnev=", latest)
    }
    
    # If latest is specified, dates are overridden
    if (!is.null(start) && !is.null(end)) {
      if (is.null(latest)) {
        years <- paste0("&date=", start, ":", end)
      } else {
        years <- NULL
      }
    } else {
      years <- NULL
    }

    # WDI only allows 32500 per_page (this seems undocumented)
    out = paste0("https://api.worldbank.org/v2/",
                 language, 
                 "/country/", 
                 country, "/indicator/", indicator,
                 "?format=json",
                 years,
                 "&per_page=32500",
                 "&page=", 1:10,
                 latest)
    return(out)
}

#' Internal function to download data
#'
#' @export
#' @keywords internal
wdi.dl = function(indicator, country, start, end, latest = NULL, language = "en"){
    get_page <- function(daturl) {
        # download
        dat_raw = RJSONIO::fromJSON(daturl, nullValue=NA)[[2]]
        # extract data 
        dat = lapply(dat_raw, function(j) cbind(j$country[[1]], j$country[[2]], j$value, j$date))
        dat = data.frame(do.call('rbind', dat), stringsAsFactors = FALSE)
        colnames(dat) = c('iso2c', 'country', as.character(indicator), 'year')
        dat$label <- dat_raw[[1]]$indicator['value']
        # output
        return(dat)
    }

    pages <- wdi.query(indicator, country, start, end, latest, language)

    dat <- list()
    done <- FALSE # done when pages no longer return useable info
    for (i in seq_along(pages)) {
        if (!done) {
            tmp <- tryCatch(get_page(pages[i]), error = function(e) NULL)
            if (inherits(tmp, 'data.frame') && (nrow(tmp) > 0)) {
                dat[[i]] <- tmp
            } else {
                done <- TRUE
            }
        }
    }
    dat <- do.call('rbind', dat)

    # numeric types
    dat[[indicator]] <- as.numeric(dat[[indicator]])

    # date is character for monthly/quarterly data, numeric otherwise
    if (!any(grepl('M|Q', dat$year))) {
        dat$year <- as.integer(dat$year)
    }

    # Bad data in WDI JSON files require me to impose this constraint
    if (!is.null(start) && !is.null(end)) {
      dat = dat[!is.na(dat$year) & dat$year <= end & dat$year >= start,] 
    }

    # output
    out = list('data' = dat[, 1:4],
               'indicator' = indicator,
               'label' = dat$label[1])

    return(out)
}

#' Update the list of available WDI indicators
#'
#' Download an updated list of available WDI indicators from the World Bank website. Returns a list for use in the \code{WDIsearch} function. 
#' 
#' @return Series of indicators, sources and descriptions in two lists list  
#' @note Downloading all series information from the World Bank website can take time.
#' The \code{WDI} package ships with a local data object with information on all the series
#' available on 2012-06-18. You can update this database by retrieving a new list using \code{WDIcache}, and  then
#' feeding the resulting object to \code{WDIsearch} via the \code{cache} argument. 
#' @export
WDIcache = function(){
    # Series
    series_url = 'https://api.worldbank.org/v2/indicator?per_page=25000&format=json'
    series_dat    = RJSONIO::fromJSON(series_url, nullValue=NA)[[2]]
    series_dat = lapply(series_dat, function(k) cbind(
                        'indicator'=k$id, 'name'=k$name, 'description'=k$sourceNote, 
                        'sourceDatabase'=k$source[2], 'sourceOrganization'=k$sourceOrganization)) 
    series_dat = do.call('rbind', series_dat)          
    # Countries
    country_url = 'https://api.worldbank.org/v2/countries/all?per_page=25000&format=json'
    country_dat = RJSONIO::fromJSON(country_url, nullValue=NA)[[2]]
    country_dat = lapply(country_dat, function(k) cbind(
                         'iso3c'=k$id, 'iso2c'=k$iso2Code, 'country'=k$name, 'region'=k$region[['value']],
                         'capital'=k$capitalCity, 'longitude'=k$longitude, 'latitude'=k$latitude, 
                         'income'=k$incomeLevel[['value']], 'lending'=k$lendingType[['value']])) 
    country_dat = do.call('rbind', country_dat)
    row.names(country_dat) = row.names(series_dat) = NULL
    out = list('series'=series_dat, 'country'=country_dat)
    out$series = iconv(out$series, to = 'utf8')
    out$country = iconv(out$country, to = 'utf8')
    # some regions have extra whitespace in wb data
    out$country[, 'region'] = base::trimws(out$country[, 'region'])
    return(out)
}

#' Search names and descriptions of available WDI series
#' 
#' Data frame with series code, name, description, and source for the WDI series
#' which match the given criteria
#' 
#' @param string Character string. Search for this string using \code{grep} with
#'     \code{ignore.case=TRUE}.
#' @param field Character string. Search this field. Admissible fields:
#'     'indicator', 'name', 'description', 'sourceDatabase', 'sourceOrganization'
#' @param short TRUE: Returns only the indicator's code and name. FALSE: Returns
#'     the indicator's code, name, description, and source.
#' @param cache Data list generated by the \code{WDIcache} function. If omitted,
#'     \code{WDIsearch} will search a local list of series.  
#' @return Data frame with code, name, source, and description of all series which
#'     match the criteria.  
#' @export
#' @examples
#' \dontrun{
#' WDIsearch(string='gdp', field='name', cache=NULL)
#' WDIsearch(string='AG.AGR.TRAC.NO', field='indicator', cache=NULL)
#' }
WDIsearch <- function(string="gdp", field="name", short=TRUE, cache=NULL){
    if(!is.null(cache)){ 
        series = cache$series    
    }else{
        series = WDI::WDI_data$series
    }
    matches = grep(string, series[,field], ignore.case=TRUE)
    if(short){
        out = series[matches, c('indicator', 'name')]
    }else{
        out = series[matches,]
    }
    return(out)
}


#' List of supported languages
#' 
#' This prints two lists of languages, the fully supported ones and the locally supported ones:
#' * the languages in the category "fully" will return translated names and other info for all countries.
#' * the languages in the category "partially" will return translated names and other info only for the country they represent. 
#' 
#' For example, choosing "vi" (for Vietnamese) will translate "Vietnam" in the dataset but other country names won't be translated and will be empty.
#'
#'
#' @return A list of fully and partially supported languages.
#' @export
languages_supported <- function() {
  
    fully <- c("en (English)", "es (Spanish)", "fr (French)", 
               "ar (Arabic)", "zh (Chinese)")
    locally <- c("bg (Bulgarian)", "de (German)", "hi (Hindi)", 
                 "id (Indonesian)", "ja (Japanese)", "km (Khmer)", 
                 "ko (Korean)", "mk (Macedonian)", "mn (Mongolian)",
                 "pl (Polish)", "pt (Portuguese)", "ro (Romanian)",
                 "ru (Russian)", "sq (Albanian)", "th (Thai)", "tr (Turkish)",
                 "uk (Ukrainian)", "vi (Vietnamese)")
    
    list(
      fully = fully,
      locally = locally
    )
    
}
