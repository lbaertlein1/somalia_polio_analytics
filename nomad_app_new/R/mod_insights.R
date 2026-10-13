# R/mod_insights.R
# AI-generated insights using Claude Haiku.
# All insights are generated once on data load (or year filter change),
# stored in a reactiveValues object, and rendered directly into the UI.

# ── UI helper ─────────────────────────────────────────────────────────────────
# Wraps a uiOutput in a styled insight box.
insight_ui <- function(ns, output_id, class = "insight") {
  uiOutput(ns(output_id))
}

# ── Render helper ─────────────────────────────────────────────────────────────
# Renders an insight from the insights reactiveValues into a uiOutput.
render_insight <- function(output, output_id, insights_rv, key,
                           fallback = NULL, class = "insight") {
  output[[output_id]] <- renderUI({
    text <- insights_rv[[key]]
    if (is.null(text)) {
      if (!is.null(fallback)) return(div(class = class, HTML(fallback)))
      return(div(class = "insight-loading",
                 div(class = "spinner"),
                 span(style = "color:var(--muted);font-size:12px;margin-left:8px",
                      "Generating insight\u2026")
      ))
    }
    if (startsWith(text, "ERROR:")) {
      return(div(class = class,
                 HTML(paste0("<em style='color:var(--muted)'>", substr(text, 8, 300), "</em>"))
      ))
    }
    div(class = class, HTML(text))
  })
}

# ── Generate all insights ─────────────────────────────────────────────────────
# Call once from the main server. Returns a reactiveValues with all insight keys.
# Keys: movement_key, movement_zd, cross_border,
#       zd_summary, coverage_summary,
#       access_hf, access_challenges,
#       report_actions

generate_all_insights <- function(data, yr, session) {
  rv <- reactiveValues(
    movement_key   = NULL,
    cross_border   = NULL,
    zd_summary     = NULL,
    coverage_gap   = NULL,
    access_summary = NULL,
    report_actions = NULL
  )
  
  observe({
    req(yr())
    y <- yr()
    
    # Clear on year change so spinners show while regenerating
    rv$movement_key   <- NULL
    rv$cross_border   <- NULL
    rv$zd_summary     <- NULL
    rv$coverage_gap   <- NULL
    rv$access_summary <- NULL
    rv$report_actions <- NULL
    
    # Build all prompts from current year slice
    fl  <- data$flows[[y]]
    ind <- data$indegree[[y]]
    zd  <- data$zd_by_dist[[y]]
    pf  <- data$perf_table[[y]]
    cb  <- data$cross_border[[y]]
    kpi <- data$kpi[[y]]
    hf  <- data$hf_access[[y]]
    ch  <- data$challenges[[y]]
    
    # Helpers
    fmt <- function(n) if (is.null(n) || is.na(n)) "N/A" else format(round(n), big.mark = ",")
    
    fl_clean <- if (!is.null(fl)) fl[!is.na(fl$prev_c) & !is.na(fl$next_c), ] else data.frame()
    total    <- if (nrow(fl_clean) > 0) sum(fl_clean$count) else 0
    intra    <- if (nrow(fl_clean) > 0) sum(fl_clean$count[fl_clean$prev_c == fl_clean$next_c], na.rm = TRUE) else 0
    intra_pct <- if (total > 0) round(100 * intra / total) else 0
    
    cross_flows <- if (nrow(fl_clean) > 0) {
      cx <- fl_clean[fl_clean$prev_c != fl_clean$next_c & fl_clean$count >= 3, ]
      cx <- cx[order(-cx$count), ][seq_len(min(5, nrow(cx))), ]
      paste(apply(cx, 1, function(r) paste0(r["prev_c"], "->", r["next_c"], " (", r["count"], ")")), collapse = ", ")
    } else "none"
    
    top_ind <- if (!is.null(ind) && nrow(ind) > 0)
      paste(apply(head(ind, 5), 1, function(r) paste0(r["district"], ":", r["count"])), collapse = ", ")
    else "N/A"
    
    top_zd <- if (!is.null(zd) && nrow(zd) > 0) {
      zd2 <- zd[!is.na(zd$district) & zd$district != "Luuq", ]
      if (nrow(zd2) > 0) {
        zd2 <- zd2[order(-zd2$rate011), ][seq_len(min(5, nrow(zd2))), ]
        paste(apply(zd2, 1, function(r) paste0(r["district"], " ", r["rate011"], "%")), collapse = ", ")
      } else "N/A"
    } else "N/A"
    
    dolow_zd <- if (!is.null(zd)) { r <- zd$rate011[!is.na(zd$district) & zd$district == "Dolow"]; if (length(r) > 0) paste0(r[1], "%") else "N/A" } else "N/A"
    elwak_zd <- if (!is.null(zd)) { r <- zd$rate011[!is.na(zd$district) & zd$district == "Elwak"]; if (length(r) > 0) paste0(r[1], "%") else "N/A" } else "N/A"
    
    gap_pct <- if (!is.null(pf)) {
      tzd <- sum(pf$zd_id, na.rm = TRUE); tv <- sum(pf$vaccinated, na.rm = TRUE)
      if (tzd > 0) paste0(round(100 * (1 - tv / tzd)), "%") else "N/A"
    } else "N/A"
    
    far_hf <- if (!is.null(hf)) {
      r <- hf$pct[hf$bucket == "120+ min"]
      if (length(r) > 0) paste0(r[1], "%") else "N/A"
    } else "N/A"
    
    top_challenges <- if (!is.null(ch) && nrow(ch) > 0)
      paste(head(ch$challenge, 3), collapse = ", ")
    else "N/A"
    
    sys_prompt <- paste(
      "You are a public health analyst for the Somalia Nomadic Polio Eradication Programme.",
      "Write concise, direct insights for field programme staff.",
      "2-4 sentences max. Lead with the most critical finding.",
      "Use specific numbers. No bullet points. No preamble. No markdown headers.",
      "Bold key terms using <strong> HTML tags."
    )
    
    prompts <- list(
      movement_key = paste0(
        "Year: ", y, ". Nomadic camp movement data: ",
        intra_pct, "% intra-district movement. ",
        "Cross-district flows: ", cross_flows, ". ",
        "Top destination districts by indegree: ", top_ind, ". ",
        "Highest ZD districts: ", top_zd, ". ",
        "Write a key finding insight about movement patterns and zero-dose implications."
      ),
      cross_border = paste0(
        "Year: ", y, ". Cross-border data: ",
        "Dolow (Ethiopia border): ", fmt(cb$dolow), " camps, ZD rate 0-11mo: ", dolow_zd, ". ",
        "Elwak (Kenya border): ", fmt(cb$elwak), " camps, ZD rate 0-11mo: ", elwak_zd, ". ",
        "Overall outreach gap: ", gap_pct, " of ZD children unreached. ",
        "Write a cross-border polio risk insight with specific transit vaccination recommendations."
      ),
      zd_summary = paste0(
        "Year: ", y, ". Zero-dose rates by district: ", top_zd, ". ",
        "Total ZD children identified: ", fmt(kpi$zd_total), ". ",
        "Children vaccinated through outreach: ", fmt(kpi$vaccinated), ". ",
        "Write a 2-sentence insight about the zero-dose burden and which districts need urgent action."
      ),
      coverage_gap = paste0(
        "Year: ", y, ". Outreach coverage gap: ", gap_pct, " of identified ZD children not reached. ",
        "Total ZD identified: ", fmt(kpi$zd_total), ", vaccinated: ", fmt(kpi$vaccinated), ". ",
        if (!is.null(pf)) paste0("Lowest coverage districts: ",
                                 paste(apply(head(pf[order(pf$cov_pct), ][pf[order(pf$cov_pct), ]$zd_id > 50, ], 3), 1,
                                             function(r) paste0(r["district"], " ", r["cov_pct"], "%")), collapse = ", "), ". ")
        else "",
        "Write a 2-sentence insight about coverage gaps and priority districts."
      ),
      access_summary = paste0(
        "Year: ", y, ". Access data: ",
        far_hf, " of camps are 120+ minutes from nearest health facility. ",
        "Top reported challenges: ", top_challenges, ". ",
        "97% of nomadic populations travel by livestock track or foot. ",
        "Write a 2-sentence structural access insight explaining why fixed-post immunization fails here."
      ),
      report_actions = paste0(
        "Year: ", y, ". Programme summary: ",
        fmt(kpi$camps), " camps enumerated, ",
        fmt(kpi$zd_total), " zero-dose children identified, ",
        fmt(kpi$vaccinated), " vaccinated through outreach (",
        gap_pct, " gap). ",
        "Highest ZD districts: ", top_zd, ". ",
        "Cross-border camps: Dolow ", fmt(cb$dolow), " (", dolow_zd, " ZD), ",
        "Elwak ", fmt(cb$elwak), " (", elwak_zd, " ZD). ",
        "Write 3-4 prioritised action items for field programme leadership. ",
        "Format each as a short bold heading followed by one sentence. Use 🔴 for critical, 🟡 for important, 🟢 for positive findings."
      )
    )
    
    # Fire API calls asynchronously using promises if available, else sequential
    for (key in names(prompts)) {
      local({
        k <- key
        p <- prompts[[k]]
        tryCatch({
          result <- call_claude_insight(p, sys_prompt)
          rv[[k]] <- insight_to_html(result)
        }, error = function(e) {
          rv[[k]] <- paste0("ERROR:", e$message)
        })
      })
    }
  })
  
  rv
}

# ── Claude API call ───────────────────────────────────────────────────────────
call_claude_insight <- function(prompt, system_prompt = NULL) {
  api_key <- Sys.getenv("ANTHROPIC_API_KEY")
  if (api_key == "") stop("ANTHROPIC_API_KEY not set in .Renviron")
  
  if (is.null(system_prompt)) {
    system_prompt <- paste(
      "You are a public health analyst for the Somalia Nomadic Polio Eradication Programme.",
      "Write concise, direct insights. 2-4 sentences. Lead with the most critical finding.",
      "Use specific numbers. No bullet points. Bold key terms with <strong> HTML tags."
    )
  }
  
  resp <- httr::POST(
    url  = "https://api.anthropic.com/v1/messages",
    httr::add_headers(
      "Content-Type"      = "application/json",
      "anthropic-version" = "2023-06-01",
      "x-api-key"         = api_key
    ),
    body = jsonlite::toJSON(list(
      model      = "claude-haiku-4-5-20251001",
      max_tokens = 400L,
      system     = system_prompt,
      messages   = list(list(role = "user", content = prompt))
    ), auto_unbox = TRUE),
    encode = "json"
  )
  
  if (httr::http_error(resp)) stop("API returned ", httr::status_code(resp))
  
  parsed <- jsonlite::fromJSON(
    httr::content(resp, as = "text", encoding = "UTF-8"),
    simplifyVector = FALSE
  )
  for (b in parsed$content) {
    if (!is.null(b$type) && b$type == "text") return(b$text)
  }
  stop("No text in API response")
}

# ── Markdown → HTML ───────────────────────────────────────────────────────────
insight_to_html <- function(text) {
  text <- gsub("\\*\\*(.+?)\\*\\*", "<strong>\\1</strong>", text)
  text <- gsub("\n\n+", "</p><p>", text)
  text <- gsub("\n", " ", text)
  paste0("<p>", text, "</p>")
}