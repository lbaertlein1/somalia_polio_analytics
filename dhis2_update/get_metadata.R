# ==========================================
# DHIS2 metadata pull with adaptive strategy
# - checks collection size first
# - uses single pull for small endpoints
# - uses pagination for large endpoints
# - saves JSON for all pulls
# - saves CSV only for flat/tabular outputs
# ==========================================

library(httr2)
library(jsonlite)
library(purrr)
library(dplyr)

base_url <- "https://www.somalia-polio-ims.org"
username <- "admin"
password <- "district"

out_dir <- "dhis2_metadata_pull"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

page_size <- 250
paginate_threshold <- 500

dhis2_get <- function(endpoint, query = list()) {
  req <- request(paste0(base_url, endpoint)) |>
    req_auth_basic(username = username, password = password) |>
    req_url_query(!!!query) |>
    req_error(is_error = function(resp) FALSE)
  
  resp <- req_perform(req)
  
  if (resp_status(resp) >= 400) {
    stop(
      sprintf(
        "Request failed [%s] for %s\n%s",
        resp_status(resp),
        endpoint,
        resp_body_string(resp)
      )
    )
  }
  
  resp_body_json(resp, simplifyVector = TRUE)
}

save_json <- function(x, name) {
  write_json(
    x,
    path = file.path(out_dir, paste0(name, ".json")),
    pretty = TRUE,
    auto_unbox = TRUE,
    na = "null"
  )
}

flatten_for_csv <- function(x) {
  if (is.null(x)) return(NULL)
  if (!is.data.frame(x)) x <- as.data.frame(x, stringsAsFactors = FALSE)
  
  out <- x
  
  for (j in seq_along(out)) {
    if (is.list(out[[j]]) || is.matrix(out[[j]])) {
      out[[j]] <- vapply(
        out[[j]],
        function(val) {
          if (length(val) == 1 && !is.list(val) && !is.data.frame(val)) {
            as.character(val)
          } else {
            jsonlite::toJSON(val, auto_unbox = TRUE, null = "null")
          }
        },
        character(1)
      )
    }
  }
  
  out
}

save_csv_if_flat_enough <- function(x, name) {
  if (is.null(x)) return(invisible(NULL))
  
  flat <- flatten_for_csv(x)
  utils::write.csv(
    flat,
    file = file.path(out_dir, paste0(name, ".csv")),
    row.names = FALSE,
    na = ""
  )
}

get_collection_count <- function(endpoint, root, extra_query = list()) {
  res <- dhis2_get(
    endpoint,
    query = c(
      list(
        page = 1,
        pageSize = 1,
        fields = "id"
      ),
      extra_query
    )
  )
  
  if (!is.null(res$pager$total)) return(res$pager$total)
  if (!is.null(res[[root]])) return(length(res[[root]]))
  NA_integer_
}

is_flat_df <- function(x) {
  if (is.null(x)) return(FALSE)
  if (!is.data.frame(x)) x <- as.data.frame(x, stringsAsFactors = FALSE)
  !any(vapply(x, function(col) is.list(col) || is.matrix(col), logical(1)))
}

pull_collection_single <- function(endpoint, root, fields, name, extra_query = list()) {
  message("Pulling ", name, " with single request")
  
  res <- dhis2_get(
    endpoint,
    query = c(
      list(
        paging = "false",
        fields = fields
      ),
      extra_query
    )
  )
  
  save_json(res, name)
  
  if (!is.null(res[[root]]) && is_flat_df(res[[root]])) {
    utils::write.csv(
      as.data.frame(res[[root]], stringsAsFactors = FALSE),
      file = file.path(out_dir, paste0(name, ".csv")),
      row.names = FALSE,
      na = ""
    )
  }
  
  invisible(res)
}

pull_collection_paginated <- function(
    endpoint,
    root,
    fields,
    name,
    extra_query = list(),
    page_size = 250
) {
  message("Pulling ", name, " with pagination")
  
  page <- 1
  all_rows <- list()
  
  repeat {
    message("  page ", page)
    
    res <- dhis2_get(
      endpoint,
      query = c(
        list(
          page = page,
          pageSize = page_size,
          fields = fields
        ),
        extra_query
      )
    )
    
    write_json(
      res,
      file.path(out_dir, paste0(name, "_page_", page, ".json")),
      pretty = TRUE,
      auto_unbox = TRUE
    )
    
    if (!is.null(res[[root]])) {
      all_rows[[page]] <- res[[root]]
    }
    
    if (is.null(res$pager) || page >= res$pager$pageCount) break
    page <- page + 1
  }
  
  combined <- list()
  combined[[root]] <- unlist(all_rows, recursive = FALSE)
  
  write_json(
    combined,
    file.path(out_dir, paste0(name, ".json")),
    pretty = TRUE,
    auto_unbox = TRUE
  )
  
  invisible(combined)
}

pull_collection_auto <- function(endpoint, root, fields, name, extra_query = list(),
                                 paginate_threshold = 500, page_size = 250) {
  n <- get_collection_count(endpoint, root, extra_query = extra_query)
  message("Endpoint ", name, " estimated rows: ", n)
  
  if (is.na(n) || n > paginate_threshold) {
    pull_collection_paginated(
      endpoint = endpoint,
      root = root,
      fields = fields,
      name = name,
      extra_query = extra_query,
      page_size = page_size
    )
  } else {
    pull_collection_single(
      endpoint = endpoint,
      root = root,
      fields = fields,
      name = name,
      extra_query = extra_query
    )
  }
}

# --------------------------------
# system / user context
# --------------------------------

system_info <- dhis2_get(
  "/api/system/info",
  query = list(
    fields = "contextPath,dateFormat,serverDate,serverTimeZoneId,version,revision,systemId"
  )
)
save_json(system_info, "system_info")

me_info <- dhis2_get(
  "/api/me",
  query = list(
    fields = "id,username,name,userRoles[id,name],organisationUnits[id,name,level,path],dataViewOrganisationUnits[id,name,level,path]"
  )
)
save_json(me_info, "me")

# --------------------------------
# endpoint registry
# --------------------------------

metadata_jobs <- list(
  list(
    endpoint = "/api/organisationUnits",
    root = "organisationUnits",
    fields = "id,code,name,shortName,displayName,level,path,openingDate,closedDate,parent[id,code,name,level]"
  ),
  list(
    endpoint = "/api/organisationUnitLevels",
    root = "organisationUnitLevels",
    fields = "id,level,name,offlineLevels"
  ),
  list(
    endpoint = "/api/organisationUnitGroups",
    root = "organisationUnitGroups",
    fields = "id,code,name,shortName,organisationUnitGroupSet[id,name]"
  ),
  list(
    endpoint = "/api/organisationUnitGroupSets",
    root = "organisationUnitGroupSets",
    fields = "id,code,name,shortName,compulsory,organisationUnitGroups[id,name]"
  ),
  list(
    endpoint = "/api/programs",
    root = "programs",
    fields = paste(
      c(
        "id",
        "code",
        "name",
        "shortName",
        "programType",
        "trackedEntityType[id,name]",
        "categoryCombo[id,name]",
        "version",
        "organisationUnits[id,name,level]",
        "programStages[id,name,repeatable,executionDateLabel]",
        "programTrackedEntityAttributes[id,name,mandatory,displayInList,trackedEntityAttribute[id,name,code,valueType,optionSet[id,name]]]",
        "programIndicators[id,name,code]",
        "programRules[id,name,code]"
      ),
      collapse = ","
    )
  ),
  list(
    endpoint = "/api/programStages",
    root = "programStages",
    fields = paste(
      c(
        "id",
        "code",
        "name",
        "repeatable",
        "executionDateLabel",
        "program[id,name]",
        "programStageDataElements[id,compulsory,allowProvidedElsewhere,displayInReports,dataElement[id,name,code,valueType,optionSet[id,name],categoryCombo[id,name]]]"
      ),
      collapse = ","
    )
  ),
  list(
    endpoint = "/api/programIndicators",
    root = "programIndicators",
    fields = "id,code,name,shortName,aggregationType,expression,filter,analyticsType,program[id,name]"
  ),
  list(
    endpoint = "/api/programRules",
    root = "programRules",
    fields = "id,code,name,program[id,name],programStage[id,name],condition,programRuleActions[id,programRuleActionType,data,text,dataElement[id,name],trackedEntityAttribute[id,name],programStageSection[id,name]]"
  ),
  list(
    endpoint = "/api/trackedEntityAttributes",
    root = "trackedEntityAttributes",
    fields = "id,code,name,shortName,valueType,unique,optionSet[id,name],aggregationType"
  ),
  list(
    endpoint = "/api/trackedEntityTypes",
    root = "trackedEntityTypes",
    fields = "id,code,name,shortName"
  ),
  list(
    endpoint = "/api/dataSets",
    root = "dataSets",
    fields = "id,code,name,shortName,periodType,expiryDays,timelyDays,openFuturePeriods,renderAsTabs,organisationUnits[id,name,level],dataSetElements[dataElement[id,name,code,valueType,optionSet[id,name],categoryCombo[id,name]]],sections[id,name],categoryCombo[id,name]"
  ),
  list(
    endpoint = "/api/sections",
    root = "sections",
    fields = "id,code,name,shortName,dataSet[id,name],greyedFields[dataElement[id,name],categoryOptionCombo[id,name]]"
  ),
  list(
    endpoint = "/api/dataElements",
    root = "dataElements",
    fields = "id,code,name,shortName,formName,domainType,valueType,aggregationType,zeroIsSignificant,optionSet[id,name],categoryCombo[id,name],legendSets[id,name]"
  ),
  list(
    endpoint = "/api/dataElementGroups",
    root = "dataElementGroups",
    fields = "id,code,name,shortName,dataElements[id,name]"
  ),
  list(
    endpoint = "/api/dataElementGroupSets",
    root = "dataElementGroupSets",
    fields = "id,code,name,shortName,compulsory,dataElementGroups[id,name]"
  ),
  list(
    endpoint = "/api/optionSets",
    root = "optionSets",
    fields = "id,code,name,shortName,valueType,options[id,code,name,sortOrder]"
  ),
  list(
    endpoint = "/api/categoryCombos",
    root = "categoryCombos",
    fields = "id,code,name,skipTotal,categories[id,name]"
  ),
  list(
    endpoint = "/api/categories",
    root = "categories",
    fields = "id,code,name,dataDimension,categoryOptions[id,code,name]"
  ),
  list(
    endpoint = "/api/categoryOptions",
    root = "categoryOptions",
    fields = "id,code,name,shortName"
  ),
  list(
    endpoint = "/api/categoryOptionCombos",
    root = "categoryOptionCombos",
    fields = "id,code,name,categoryCombo[id,name],categoryOptions[id,name]"
  ),
  list(
    endpoint = "/api/indicators",
    root = "indicators",
    fields = "id,code,name,shortName,indicatorType[id,name],numerator,denominator,annualized,decimals,aggregationType,legendSets[id,name]"
  ),
  list(
    endpoint = "/api/indicatorTypes",
    root = "indicatorTypes",
    fields = "id,name,factor,number,displayName"
  ),
  list(
    endpoint = "/api/validationRules",
    root = "validationRules",
    fields = "id,code,name,leftSide[expression],rightSide[expression],operator,importance"
  ),
  list(
    endpoint = "/api/constants",
    root = "constants",
    fields = "id,code,name,value"
  ),
  list(
    endpoint = "/api/sqlViews",
    root = "sqlViews",
    fields = "id,name,description,type,cacheStrategy,sqlQuery"
  ),
  list(
    endpoint = "/api/legendSets",
    root = "legendSets",
    fields = "id,code,name,legends[id,name,startValue,endValue,color]"
  ),
  list(
    endpoint = "/api/predictors",
    root = "predictors",
    fields = "id,code,name,output[id,name],generator,periodType,organisationUnitLevels[level]"
  ),
  list(
    endpoint = "/api/jobConfigurations",
    root = "jobConfigurations",
    fields = "id,name,jobType,enabled,cronExpression"
  )
)

# --------------------------------
# run all jobs
# --------------------------------

for (job in metadata_jobs) {
  nm <- gsub("^/api/", "", job$endpoint)
  nm <- gsub("/", "_", nm)
  
  pull_collection_auto(
    endpoint = job$endpoint,
    root = job$root,
    fields = job$fields,
    name = nm,
    paginate_threshold = paginate_threshold,
    page_size = page_size
  )
}

# --------------------------------
# full metadata export
# --------------------------------

metadata_full <- dhis2_get(
  "/api/metadata",
  query = list(
    fields = ":owner"
  )
)
save_json(metadata_full, "metadata_full_owner")

message("Done. Files saved in: ", normalizePath(out_dir))