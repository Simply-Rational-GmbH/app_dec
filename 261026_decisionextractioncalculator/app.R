suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(bslib))
suppressPackageStartupMessages(library(ggplot2))

# Farben aus dem Simply-Rational-CI (siehe _brand.yml)
sr <- list(ink = "#020202", cream = "#FFF5EB", purple = "#661AAE",
           teal = "#1AA7B4", orange = "#EA5A0C", red = "#C00D46")
farbe_entscheider <- sr$teal

fmt <- function(x, digits = 0) {
  format(round(x, digits), big.mark = ".", decimal.mark = ",",
         scientific = FALSE, trim = TRUE)
}
eur <- function(x) paste(fmt(x), "€")
pct <- function(x) ifelse(is.finite(x), paste(fmt(x * 100, 1), "%"), "–")
num <- function(x, default = 0) if (is.null(x) || is.na(x)) default else x

theme_sr <- function() {
  theme_minimal(base_size = 14) +
    theme(
      plot.background  = element_rect(fill = sr$cream, colour = NA),
      panel.background = element_rect(fill = sr$cream, colour = NA),
      panel.grid.minor = element_blank(),
      legend.position  = "top",
      legend.box       = "vertical",
      legend.title     = element_blank(),
      plot.title       = element_text(face = "bold")
    )
}

# ---- Datenmodell ------------------------------------------------------------
# Eine Entscheidung besteht aus Name, Anzahl Entscheider:innen und N Optionen.
# Jede Option hat Fälle pro Jahr, Umsatz pro Jahr (negativ = Risiko/Verlust)
# und ein Marktpotenzial (zusätzlicher Umsatz, der am Markt noch möglich ist).

neue_option <- function(id, name = "Option", faelle = 0, umsatz = 0, potenzial = 0) {
  list(id = id, name = name, faelle = faelle, umsatz = umsatz, potenzial = potenzial)
}

neue_entscheidung <- function(id, opt_ids, name = "Neue Entscheidung", n = 1,
                              optionen = NULL) {
  if (is.null(optionen)) {
    optionen <- list(neue_option(opt_ids[1], "Ja"), neue_option(opt_ids[2], "Nein"))
  }
  list(id = id, name = name, n = n, optionen = optionen)
}

start_zustand <- list(
  neue_entscheidung(1, NULL, "Antrag versichern?", n = 5, optionen = list(
    neue_option(1, "Ja",   faelle = 1000, umsatz = 500000, potenzial = 5000000),
    neue_option(2, "Nein", faelle = 300,  umsatz = 0, potenzial = 0)
  ))
)

# Simply Rational Szenario: ab Jahr y treffen alle Entscheider:innen x % mehr Entscheidungen.
# Mehrere Events eines Szenarios multiplizieren sich. Ausgewählte Szenarien werden
# nebeneinander verglichen; "Eigenes Szenario" hat frei editierbare Events.
neues_event <- function(id, x = 10, y = 1) list(id = id, x = x, y = y)
start_events <- list()

sr_szenarien <- list(
  poc   = list(name = "Entscheidungslogik PoC",        x = 5,               y = 1,
               farbe = sr$orange),
  logik = list(name = "Entscheidungslogik",            x = c(5, 10),        y = c(1, 2),
               farbe = sr$purple),
  klass = list(name = "Fallklassifizierung",           x = 15,              y = 1,
               farbe = sr$red),
  ade   = list(name = "Augmented Decision Excellence", x = c(5, 10, 20),    y = c(1, 2, 3),
               farbe = "#174CBF"),
  eigen = list(name = "Eigenes Szenario",              x = numeric(0),      y = numeric(0),
               farbe = "#82800A")
)
start_szenarien <- c("poc", "logik", "klass", "ade")

# Effizienzfaktor je Jahr t: Produkt aller Events mit Startjahr <= t
effizienz <- function(x, y, t) {
  vapply(t, function(tt) prod(1 + pmax(-100, x[y <= tt]) / 100), numeric(1))
}

szenario_label <- function(id) {
  s <- sr_szenarien[[id]]
  details <- if (length(s$x)) {
    paste(sprintf("Jahr %d: +%s %%", s$y, s$x), collapse = ", ")
  } else "Events unten frei festlegen"
  tagList(strong(s$name), span(class = "sr-note", paste0(" · ", details)))
}

start_add_n <- 1
start_jahre <- 10

# ---- URL-Parameter ----------------------------------------------------------
# Alle Eingaben lassen sich über die URL setzen (Positionen beginnen bei 1):
#   d1=Name  d1_n=Entscheider:innen
#   d1_o1=Optionsname  d1_o1_cases=Fälle/Jahr  d1_o1_revenue=Umsatz/Jahr  d1_o1_market=Marktpotenzial
#   sc=poc,logik,klass,ade,eigen  ausgewählte Simply Rational Szenarien (sc=0 → keins)
#   e1_pct=Steigerung in %  e1_year=ab Jahr  (Events des eigenen Szenarios)
#   add=zusätzliche Entscheider:innen pro Jahr  years=Zeithorizont
#   d=0 → keine Entscheidungen
# Fehlen alle d-Parameter, gilt das Beispiel oben.

url_zahl <- function(q, key, default, min = -Inf, max = Inf) {
  x <- suppressWarnings(as.numeric(q[[key]]))
  if (length(x) == 1 && is.finite(x)) min(max(x, min), max) else default
}

url_indizes <- function(q, prefix) {
  pattern <- paste0("^", prefix, "([0-9]+)(_.*)?$")
  treffer <- grep(pattern, names(q), value = TRUE)
  sort(unique(as.integer(sub(pattern, "\\1", treffer))))
}

url_zustand <- function(q, neue_id) {
  if (identical(q$d, "0")) return(list())
  idx <- url_indizes(q, "d")
  if (length(idx) == 0) return(start_zustand)
  lapply(seq_along(idx), function(i) {
    k <- paste0("d", idx[i])
    optionen <- lapply(url_indizes(q, paste0(k, "_o")), function(j) {
      p <- paste0(k, "_o", j)
      neue_option(neue_id(), num(q[[p]], paste("Option", j)),
                  faelle    = url_zahl(q, paste0(p, "_cases"), 0, min = 0),
                  umsatz    = url_zahl(q, paste0(p, "_revenue"), 0),
                  potenzial = url_zahl(q, paste0(p, "_market"), 0))
    })
    neue_entscheidung(neue_id(), NULL, num(q[[k]], paste("Entscheidung", i)),
                      n = url_zahl(q, paste0(k, "_n"), 1, min = 1),
                      optionen = optionen)
  })
}

url_szenarien <- function(q) {
  if (is.null(q$sc)) {
    return(c(start_szenarien, if (length(url_indizes(q, "e"))) "eigen"))
  }
  intersect(names(sr_szenarien), trimws(strsplit(q$sc, ",")[[1]]))
}

url_events <- function(q, neue_id) {
  idx <- url_indizes(q, "e")
  if (length(idx) == 0) return(start_events)
  lapply(idx, function(i) {
    neues_event(neue_id(),
                x = url_zahl(q, paste0("e", i, "_pct"), 10, min = -100),
                y = round(url_zahl(q, paste0("e", i, "_year"), 1, min = 1, max = 20)))
  })
}

als_url <- function(z, ev, szenarien, add_n, jahre) {
  v <- list()
  if (length(z) == 0) v$d <- 0
  for (i in seq_along(z)) {
    k <- paste0("d", i)
    v[[k]] <- z[[i]]$name
    v[[paste0(k, "_n")]] <- z[[i]]$n
    for (j in seq_along(z[[i]]$optionen)) {
      o <- z[[i]]$optionen[[j]]
      p <- paste0(k, "_o", j)
      v[[p]] <- o$name
      if (o$faelle != 0)    v[[paste0(p, "_cases")]]   <- o$faelle
      if (o$umsatz != 0)    v[[paste0(p, "_revenue")]] <- o$umsatz
      if (o$potenzial != 0) v[[paste0(p, "_market")]]  <- o$potenzial
    }
  }
  v$sc <- if (length(szenarien)) paste(szenarien, collapse = ",") else 0
  for (i in seq_along(ev)) {
    v[[paste0("e", i, "_pct")]]  <- ev[[i]]$x
    v[[paste0("e", i, "_year")]] <- ev[[i]]$y
  }
  v$add   <- add_n
  v$years <- jahre
  wert <- function(x) {
    x <- if (is.numeric(x)) format(x, scientific = FALSE, digits = 15, trim = TRUE) else enc2utf8(x)
    gsub("%2C", ",", utils::URLencode(x, reserved = TRUE), fixed = TRUE)  # Kommas lesbar lassen
  }
  paste0("?", paste(names(v), vapply(v, wert, character(1)), sep = "=", collapse = "&"))
}

# ---- UI-Bausteine -----------------------------------------------------------
cmd <- function(type, id, input = "cmd") {
  sprintf("Shiny.setInputValue('%s', {type: '%s', id: %d, t: Date.now()}, {priority: 'event'})",
          input, type, id)
}

event_ui <- function(e) {
  div(class = "sr-event",
    numericInput(paste0("e", e$id, "_x"), "Steigerung (%)", e$x, min = -100, step = 5),
    numericInput(paste0("e", e$id, "_y"), "ab Jahr", e$y, min = 1, max = 20, step = 1),
    tags$button(type = "button", class = "btn btn-sm sr-icon-btn", title = "Event entfernen",
                `aria-label` = "Event entfernen", onclick = cmd("del", e$id, "evt"), "×")
  )
}

options_ui <- function(o) {
  div(class = "sr-option",
    textInput(paste0("o", o$id, "_name"), "Option", o$name),
    numericInput(paste0("o", o$id, "_faelle"), "Fälle / Jahr", o$faelle, min = 0, step = 100),
    numericInput(paste0("o", o$id, "_umsatz"), "Umsatz / Jahr (€)", o$umsatz, step = 1000),
    numericInput(paste0("o", o$id, "_potenzial"), "Marktpotenzial (€)", o$potenzial, step = 1000),
    tags$button(type = "button", class = "btn btn-sm sr-icon-btn", title = "Option entfernen",
                `aria-label` = "Option entfernen", onclick = cmd("del_opt", o$id), "×")
  )
}

entscheidung_ui <- function(d, i) {
  card(class = "sr-decision",
    card_header(
      div(class = "sr-decision-head",
        span(class = "sr-badge", i),
        textInput(paste0("d", d$id, "_name"), NULL, d$name, width = "100%"),
        numericInput(paste0("d", d$id, "_n"), "Entscheider:innen", d$n, min = 1, step = 1)
      )
    ),
    lapply(d$optionen, options_ui),
    div(class = "sr-decision-actions",
      tags$button(type = "button", class = "btn btn-sm btn-outline-primary",
                  onclick = cmd("add_opt", d$id), "+ Option"),
      tags$button(type = "button", class = "btn btn-sm btn-outline-primary",
                  onclick = cmd("copy", d$id), "Entscheidung kopieren"),
      tags$button(type = "button", class = "btn btn-sm btn-link sr-danger",
                  onclick = cmd("del", d$id), "Entfernen")
    )
  )
}

# ---- Seitenrahmen im Stil von simplyrational.de -----------------------------
sr_header <- tags$header(class = "sr-header",
  div(class = "sr-header-inner",
    tags$a(class = "sr-header-logo", href = "https://www.simplyrational.de/",
           tags$img(src = "simply-rational-logo.webp", alt = "Simply Rational")),
    span(class = "sr-header-title", "Entscheidungs-Potenzialrechner"),
    tags$a(class = "sr-header-cta", href = "mailto:GuteEntscheidung@simplyrational.de",
           "Gute Entscheidung")
  )
)

sr_hero <- div(class = "sr-hero",
  div(class = "sr-hero-stem", `aria-hidden` = "true"),
  h1("Was sind Ihre Entscheidungen wert?"),
  p("Erfassen Sie wiederkehrende Entscheidungen mit Fällen, Umsatz und Marktpotenzial",
    "und sehen Sie, wie zusätzliche Entscheider:innen und Simply Rational Szenarien das",
    "Potenzial über die Zeit ausschöpfen.")
)

sr_footer <- tags$footer(class = "sr-footer",
  div(class = "sr-footer-arc", `aria-hidden` = "true", div()),
  div(class = "sr-footer-bar",
    div(class = "sr-footer-inner",
      div(class = "sr-footer-main",
        div(class = "sr-footer-contact",
          p(class = "sr-footer-company", "SIMPLY RATIONAL GmbH"),
          p("Alte Brauerei 14 | 10965 Berlin | Germany"),
          p(class = "sr-tight", tags$a(href = "mailto:GuteEntscheidung@simplyrational.de",
                                       "GuteEntscheidung@simplyrational.de")),
          div(p("Philipp"), p(tags$a(href = "tel:+491622690262", "+49 162 269 026 2"))),
          div(p("Susanne"), p(tags$a(href = "tel:+4917640056676", "+49 176 400 566 76")))
        ),
        tags$a(class = "sr-footer-logo", href = "https://www.simplyrational.de/",
               `aria-label` = "Simply Rational"),
        tags$nav(class = "sr-footer-legal", `aria-label` = "Rechtliches",
          tags$a(href = "https://www.simplyrational.de/impressum", "Impressum"),
          tags$a(href = "https://www.simplyrational.de/datenschutz", "Datenschutzerklärung")
        )
      ),
      div(class = "sr-footer-copy", p(paste("©", format(Sys.Date(), "%Y"), "SIMPLY RATIONAL GmbH")))
    )
  )
)

param_hilfe <- tags$details(class = "sr-params", style = "margin-top: 1rem;",
  tags$summary("Welche URL-Parameter gibt es?"),
  p(class = "sr-note", "Positionen beginnen bei 1. Fehlende Werte werden mit 0 bzw. Standardwerten gefüllt."),
  div(style = "overflow-x: auto;",
    tags$table(class = "sr-table",
      tags$thead(tags$tr(tags$th("Parameter"), tags$th("Bedeutung"))),
      tags$tbody(
        tags$tr(tags$td(code("d1")), tags$td("Name der 1. Entscheidung")),
        tags$tr(tags$td(code("d1_n")), tags$td("Anzahl Entscheider:innen")),
        tags$tr(tags$td(code("d1_o1")), tags$td("Name der 1. Option")),
        tags$tr(tags$td(code("d1_o1_cases")), tags$td("Fälle pro Jahr")),
        tags$tr(tags$td(code("d1_o1_revenue")), tags$td("Umsatz pro Jahr in € (negativ = Risiko/Verlust)")),
        tags$tr(tags$td(code("d1_o1_market")), tags$td("Marktpotenzial in €")),
        tags$tr(tags$td(code("sc")), tags$td(
          "Simply Rational Szenarien, kommagetrennt: ",
          code("poc"), " Entscheidungslogik PoC, ", code("logik"), " Entscheidungslogik, ",
          code("klass"), " Fallklassifizierung, ", code("ade"), " Augmented Decision Excellence, ",
          code("eigen"), " Eigenes Szenario; ", code("sc=0"), " = keins")),
        tags$tr(tags$td(code("e1_pct")), tags$td("Eigenes Szenario: Steigerung in %")),
        tags$tr(tags$td(code("e1_year")), tags$td("Eigenes Szenario: ab Jahr")),
        tags$tr(tags$td(code("add")), tags$td("Zusätzliche Entscheider:innen pro Jahr")),
        tags$tr(tags$td(code("years")), tags$td("Zeithorizont in Jahren (1–20)")),
        tags$tr(tags$td(code("d=0")), tags$td("Keine Entscheidungen"))
      )
    )
  )
)

share_js <- "
Shiny.addCustomMessageHandler('sr-share', function(q) {
  document.getElementById('sr-share-url').value = location.origin + location.pathname + q;
});
function srCopy(btn) {
  var f = document.getElementById('sr-share-url');
  var done = function() { btn.textContent = 'Kopiert ✓'; setTimeout(function() { btn.textContent = 'Link kopieren'; }, 1500); };
  if (navigator.clipboard && window.isSecureContext) {
    navigator.clipboard.writeText(f.value).then(done, function() { f.select(); document.execCommand('copy'); done(); });
  } else { f.select(); document.execCommand('copy'); done(); }
}
"

ui <- function(req) {
  q <- parseQueryString(req$QUERY_STRING)
  page(
    title = "Entscheidungs-Potenzialrechner | Simply Rational",
    theme = bs_theme(bg = sr$cream, fg = sr$ink, primary = sr$purple,
                     base_font = "Bricolage Grotesque", heading_font = "Noto Serif JP"),
    lang = "de",
    tags$head(
      tags$link(rel = "icon", href = "simply-rational-logo.webp"),
      tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
      tags$link(rel = "stylesheet",
                href = "https://fonts.googleapis.com/css2?family=Bricolage+Grotesque:wght@400;500;600;700;800&family=Noto+Serif+JP:wght@800&display=swap"),
      tags$link(rel = "stylesheet", href = "sr.css"),
      tags$script(HTML(share_js))
    ),
    sr_header,
    tags$main(class = "sr-main",
      sr_hero,

      # 1. Eingabe -------------------------------------------------------------
      h2(class = "sr-section-title", "Entscheidungen"),
      p(class = "sr-note",
        "Je Option: Fälle und Umsatz pro Jahr sowie das zusätzliche Marktpotenzial.",
        "Negative Umsätze bilden Risiken bzw. Verluste ab (z. B. durch eine Ablehnung)."),
      uiOutput("editor"),
      div(tags$button(type = "button", class = "btn btn-primary",
                      onclick = cmd("add", 0), "+ Neue Entscheidung")),

      # 2. Zusammenfassung -----------------------------------------------------
      h2(class = "sr-section-title", "Übersicht"),
      layout_column_wrap(
        width = "200px",
        value_box("Fälle / Jahr", textOutput("vb_faelle")),
        value_box("Umsatz / Jahr", textOutput("vb_umsatz")),
        value_box("Marktpotenzial", textOutput("vb_potenzial")),
        value_box("Ø Entscheidungen je Entscheider:in / Jahr", textOutput("vb_mean"))
      ),
      card(
        card_header(span(class = "sr-title", "Je Entscheidung und Option")),
        uiOutput("uebersicht")
      ),

      # 3. Analyse -------------------------------------------------------------
      h2(class = "sr-section-title", "Wachstum durch Marktpotenzial"),
      card(
        layout_column_wrap(
          width = "200px",
          numericInput("add_n", "Zusätzliche Entscheider:innen pro Jahr",
                       url_zahl(q, "add", start_add_n, min = 0), min = 0, step = 1),
          sliderInput("jahre", "Zeithorizont (Jahre)", min = 1, max = 20,
                      value = round(url_zahl(q, "years", start_jahre, min = 1, max = 20)), step = 1)
        ),
        div(class = "sr-subtitle", "Simply Rational Szenario"),
        p(class = "sr-note",
          "Ab dem angegebenen Jahr trifft jede:r Entscheider:in x % mehr Entscheidungen.",
          "Die Events eines Szenarios wirken multiplikativ. Jedes ausgewählte Szenario wird allein",
          "(durchgezogen) und kombiniert mit den zusätzlichen Entscheider:innen (gestrichelt) gezeigt."),
        checkboxGroupInput("szenarien", NULL,
                           choiceNames = lapply(names(sr_szenarien), szenario_label),
                           choiceValues = names(sr_szenarien),
                           selected = url_szenarien(q), width = "100%"),
        conditionalPanel("input.szenarien && input.szenarien.indexOf('eigen') > -1",
          div(class = "sr-custom",
            uiOutput("events"),
            div(tags$button(type = "button", class = "btn btn-sm btn-outline-primary",
                            onclick = cmd("add", 0, "evt"), "+ Event"))
          )
        ),
        p(class = "sr-note", style = "margin-top: 1rem;",
          "Zusätzliche Entscheider:innen und Simply Rational Szenarien erhöhen die Fallzahl. Der Umsatz",
          "je Fall bleibt gleich; zusätzlicher Umsatz kann das Marktpotenzial ausschöpfen,",
          "aber nicht überschreiten."),
        navset_underline(
          nav_panel("Umsatz", plotOutput("plot_umsatz", height = "380px")),
          nav_panel("Ausschöpfung", plotOutput("plot_ausschoepfung", height = "380px")),
          nav_panel("Entscheidungen je Entscheider:in", plotOutput("plot_mean", height = "380px"))
        ),
        uiOutput("szenario_tabelle")
      ),

      # 4. Teilen --------------------------------------------------------------
      h2(class = "sr-section-title", "Szenario teilen"),
      card(
        p(class = "sr-note",
          "Alle Eingaben stehen in der Adresszeile. Mit diesem Link öffnet sich der Rechner",
          "genau mit Ihren Werten."),
        div(class = "sr-share",
          tags$input(id = "sr-share-url", class = "form-control", type = "text", readonly = NA,
                     `aria-label` = "Link zu diesem Szenario", onclick = "this.select()"),
          tags$button(type = "button", class = "btn btn-primary", onclick = "srCopy(this)",
                      "Link kopieren")
        ),
        param_hilfe
      )
    ),
    sr_footer
  )
}

server <- function(input, output, session) {

  next_id <- local({ i <- 100; function() { i <<- i + 1; i } })
  q <- parseQueryString(isolate(session$clientData$url_search))

  zustand  <- reactiveVal(url_zustand(q, next_id))
  struktur <- reactiveVal(0)          # zählt Strukturänderungen -> Editor neu zeichnen

  # Aktuelle Eingabewerte in den Zustand übernehmen (vor jeder Strukturänderung)
  sync <- function() {
    lapply(isolate(zustand()), function(d) {
      d$name <- num(input[[paste0("d", d$id, "_name")]], d$name)
      d$n    <- num(input[[paste0("d", d$id, "_n")]], d$n)
      d$optionen <- lapply(d$optionen, function(o) {
        p <- paste0("o", o$id, "_")
        o$name      <- num(input[[paste0(p, "name")]], o$name)
        o$faelle    <- num(input[[paste0(p, "faelle")]], o$faelle)
        o$umsatz    <- num(input[[paste0(p, "umsatz")]], o$umsatz)
        o$potenzial <- num(input[[paste0(p, "potenzial")]], o$potenzial)
        o
      })
      d
    })
  }

  observeEvent(input$cmd, {
    z <- sync()
    id <- input$cmd$id
    pos <- which(vapply(z, `[[`, numeric(1), "id") == id)
    switch(input$cmd$type,
      add = {
        z[[length(z) + 1]] <- neue_entscheidung(next_id(), c(next_id(), next_id()),
                                                name = paste("Entscheidung", length(z) + 1))
      },
      copy = {
        kopie <- z[[pos]]
        kopie$id <- next_id()
        kopie$name <- paste(kopie$name, "(Kopie)")
        kopie$optionen <- lapply(kopie$optionen, function(o) { o$id <- next_id(); o })
        z <- append(z, list(kopie), after = pos)
      },
      del = {
        z[[pos]] <- NULL
      },
      add_opt = {
        z[[pos]]$optionen[[length(z[[pos]]$optionen) + 1]] <-
          neue_option(next_id(), paste("Option", length(z[[pos]]$optionen) + 1))
      },
      del_opt = {
        z <- lapply(z, function(d) {
          d$optionen <- Filter(function(o) o$id != id, d$optionen)
          d
        })
      }
    )
    zustand(z)
    struktur(struktur() + 1)
  })

  # ---- Events des eigenen Szenarios -----------------------------------------
  events        <- reactiveVal(url_events(q, next_id))
  events_struct <- reactiveVal(0)

  sync_events <- function() {
    lapply(isolate(events()), function(e) {
      e$x <- num(input[[paste0("e", e$id, "_x")]], e$x)
      e$y <- num(input[[paste0("e", e$id, "_y")]], e$y)
      e
    })
  }

  observeEvent(input$evt, {
    ev <- sync_events()
    if (input$evt$type == "add") {
      letztes <- if (length(ev)) max(vapply(ev, `[[`, numeric(1), "y")) else 0
      ev[[length(ev) + 1]] <- neues_event(next_id(), x = 10, y = letztes + 1)
    } else {
      ev <- Filter(function(e) e$id != input$evt$id, ev)
    }
    events(ev)
    events_struct(events_struct() + 1)
  })

  output$events <- renderUI({
    events_struct()
    ev <- isolate(events())
    if (length(ev) == 0) return(p(class = "sr-note", "Noch keine Events im eigenen Szenario."))
    tagList(lapply(ev, event_ui))
  })

  # ---- URL mit allen Eingaben aktuell halten ---------------------------------
  url_query <- debounce(reactive({
    struktur()
    events_struct()
    als_url(sync(), sync_events(), input$szenarien,
            num(input$add_n, start_add_n), num(input$jahre, start_jahre))
  }), 400)

  observe({
    query <- url_query()
    updateQueryString(query, mode = "replace")
    session$sendCustomMessage("sr-share", query)
  })

  # Effizienzfaktor des eigenen Szenarios je Jahr
  eigenes_szenario <- reactive({
    events_struct()
    ev <- events()
    t <- 0:input$jahre
    x <- vapply(ev, function(e) num(input[[paste0("e", e$id, "_x")]], e$x), numeric(1))
    y <- vapply(ev, function(e) num(input[[paste0("e", e$id, "_y")]], e$y), numeric(1))
    effizienz(x, y, t)
  })

  # Linien in den Grafiken: Name -> Farbe
  farben <- reactive({
    ids <- intersect(names(sr_szenarien), input$szenarien)
    c("Mehr Entscheider:innen" = farbe_entscheider,
      setNames(vapply(ids, function(id) sr_szenarien[[id]]$farbe, character(1)),
               vapply(ids, function(id) sr_szenarien[[id]]$name, character(1))))
  })

  output$editor <- renderUI({
    struktur()
    z <- isolate(zustand())
    if (length(z) == 0) return(p(class = "sr-note", "Noch keine Entscheidung angelegt."))
    tagList(Map(entscheidung_ui, z, seq_along(z)))
  })

  # Live-Daten aus den Eingabefeldern (eine Zeile je Option)
  daten <- reactive({
    struktur()
    z <- zustand()
    zeilen <- lapply(seq_along(z), function(i) {
      d <- z[[i]]
      if (length(d$optionen) == 0) return(NULL)
      do.call(rbind, lapply(d$optionen, function(o) {
        p <- paste0("o", o$id, "_")
        data.frame(
          d_id        = d$id,
          entscheidung = num(input[[paste0("d", d$id, "_name")]], d$name),
          n           = max(1, num(input[[paste0("d", d$id, "_n")]], d$n)),
          option      = num(input[[paste0(p, "name")]], o$name),
          faelle      = max(0, num(input[[paste0(p, "faelle")]], o$faelle)),
          umsatz      = num(input[[paste0(p, "umsatz")]], o$umsatz),
          potenzial   = num(input[[paste0(p, "potenzial")]], o$potenzial)
        )
      }))
    })
    do.call(rbind, zeilen)
  })

  # Je Entscheidung: Fälle und Entscheider:innen
  je_entscheidung <- reactive({
    df <- daten()
    req(!is.null(df), nrow(df) > 0)
    agg <- aggregate(cbind(faelle, umsatz, potenzial) ~ d_id + entscheidung + n, df, sum)
    agg[match(unique(df$d_id), agg$d_id), ]
  })

  output$vb_faelle    <- renderText(fmt(sum(daten()$faelle)))
  output$vb_umsatz    <- renderText(eur(sum(daten()$umsatz)))
  output$vb_potenzial <- renderText(eur(sum(daten()$potenzial)))
  output$vb_mean      <- renderText({
    e <- je_entscheidung()
    fmt(sum(e$faelle) / sum(e$n), 1)
  })

  output$uebersicht <- renderUI({
    df <- daten()
    if (is.null(df) || nrow(df) == 0) return(p(class = "sr-note", "Keine Daten."))
    e <- je_entscheidung()
    zelle_eur <- function(x) tags$td(class = if (x < 0) "sr-neg", eur(x))
    je_fall   <- function(u, f) if (f > 0) eur(u / f) else "–"

    zeilen <- list()
    for (i in seq_len(nrow(e))) {
      sub <- df[df$d_id == e$d_id[i], ]
      for (j in seq_len(nrow(sub))) {
        zeilen[[length(zeilen) + 1]] <- tags$tr(
          tags$td(if (j == 1) sub$entscheidung[j]), tags$td(sub$option[j]),
          tags$td(fmt(sub$faelle[j])),
          tags$td(pct(sub$faelle[j] / e$faelle[i])),
          zelle_eur(sub$umsatz[j]), tags$td(je_fall(sub$umsatz[j], sub$faelle[j])),
          zelle_eur(sub$potenzial[j]), tags$td(""), tags$td("")
        )
      }
      zeilen[[length(zeilen) + 1]] <- tags$tr(class = "sr-sub",
        tags$td(""), tags$td("Summe"),
        tags$td(fmt(e$faelle[i])), tags$td(""),
        zelle_eur(e$umsatz[i]), tags$td(je_fall(e$umsatz[i], e$faelle[i])),
        zelle_eur(e$potenzial[i]),
        tags$td(fmt(e$n[i])), tags$td(fmt(e$faelle[i] / e$n[i], 1))
      )
    }
    zeilen[[length(zeilen) + 1]] <- tags$tr(class = "sr-total",
      tags$td("Gesamt"), tags$td(""),
      tags$td(fmt(sum(e$faelle))), tags$td(""),
      zelle_eur(sum(e$umsatz)), tags$td(je_fall(sum(e$umsatz), sum(e$faelle))),
      zelle_eur(sum(e$potenzial)),
      tags$td(fmt(sum(e$n))), tags$td(fmt(sum(e$faelle) / sum(e$n), 1))
    )

    div(style = "overflow-x: auto;",
      tags$table(class = "sr-table",
        tags$thead(tags$tr(
          tags$th("Entscheidung"), tags$th("Option"), tags$th("Fälle / Jahr"),
          tags$th("Anteil"), tags$th("Umsatz / Jahr"), tags$th("Umsatz je Fall"),
          tags$th("Marktpotenzial"), tags$th("Entscheider:innen"),
          tags$th("Ø Entsch. je Entscheider:in")
        )),
        tags$tbody(zeilen)
      )
    )
  })

  # ---- Wachstumsszenarien ---------------------------------------------------
  # Faktor f skaliert die Fallzahl; der Zusatzumsatz U*(f-1) wird je Option
  # auf das Intervall zwischen 0 und dem Marktpotenzial begrenzt.
  szenario <- reactive({
    df <- daten()
    req(!is.null(df), nrow(df) > 0)
    e  <- je_entscheidung()
    t <- 0:input$jahre
    # zusätzliche Entscheider:innen verteilen sich anteilig auf alle Entscheidungen
    n_fak <- (sum(e$n) + max(0, num(input$add_n)) * t) / sum(e$n)
    eins  <- rep(1, length(t))

    # je Linie: Faktoren, Farbe (basis) und Linientyp (variante)
    faktoren <- list("Mehr Entscheider:innen" =
                       list(n = n_fak, eff = eins, basis = "Mehr Entscheider:innen", variante = "allein"))
    for (id in intersect(names(sr_szenarien), input$szenarien)) {
      s <- sr_szenarien[[id]]
      eff <- if (id == "eigen") eigenes_szenario() else effizienz(s$x, s$y, t)
      faktoren[[s$name]] <- list(n = eins, eff = eff, basis = s$name, variante = "allein")
      faktoren[[paste(s$name, "+ Entscheider:innen")]] <-
        list(n = n_fak, eff = eff, basis = s$name, variante = "kombiniert")
    }

    do.call(rbind, Map(function(f, name) {
      fak <- f$n * f$eff
      werte <- t(vapply(fak, function(k) {
        zusatz <- pmin(pmax(df$umsatz * (k - 1), pmin(0, df$potenzial)), pmax(0, df$potenzial))
        anteil <- ifelse(df$umsatz != 0, zusatz / df$umsatz, 0)
        c(umsatz = sum(df$umsatz + zusatz),
          zusatz = sum(zusatz),
          faelle = sum(df$faelle * (1 + anteil)))
      }, numeric(3)))
      data.frame(szenario = name, basis = f$basis, variante = f$variante, jahr = t, werte,
                 entscheider = sum(e$n) * f$n)
    }, faktoren, names(faktoren)))
  })

  potenzial_gesamt <- reactive(sum(daten()$potenzial))

  varianten <- c(allein = "allein", kombiniert = "+ zusätzliche Entscheider:innen")
  linien_faktoren <- function(s) {
    s$basis    <- factor(s$basis, levels = names(farben()))
    s$variante <- factor(s$variante, levels = names(varianten))
    s
  }
  linien <- function() {
    list(
      geom_line(linewidth = 1.1),
      geom_point(size = 1.8),
      scale_colour_manual(values = farben()),
      scale_linetype_manual(values = c(allein = "solid", kombiniert = "22"), labels = varianten),
      guides(colour   = guide_legend(nrow = 2, byrow = TRUE, order = 1),
             linetype = guide_legend(order = 2, override.aes = list(colour = sr$ink, shape = NA)))
    )
  }

  output$plot_umsatz <- renderPlot({
    s <- szenario()
    s <- linien_faktoren(s)
    ist <- sum(daten()$umsatz)
    ggplot(s, aes(jahr, umsatz, colour = basis, linetype = variante, group = szenario)) +
      geom_hline(yintercept = ist, colour = "grey55") +
      geom_hline(yintercept = ist + potenzial_gesamt(), colour = sr$ink, linetype = "dashed") +
      annotate("text", x = 0, y = ist + potenzial_gesamt(), label = "Umsatz + Marktpotenzial",
               hjust = 0, vjust = -0.6, size = 3.6) +
      annotate("text", x = 0, y = ist, label = "Ist", hjust = 0, vjust = 1.6, size = 3.6,
               colour = "grey40") +
      linien() +
      scale_x_continuous(breaks = scales::breaks_pretty()) +
      scale_y_continuous(labels = function(x) fmt(x / 1e3)) +
      labs(title = "Umsatz pro Jahr je Szenario", x = "Jahr", y = "Tsd. €") +
      theme_sr()
  }, bg = sr$cream)

  output$plot_ausschoepfung <- renderPlot({
    s <- szenario()
    s <- linien_faktoren(s)
    validate(need(potenzial_gesamt() != 0, "Bitte ein Marktpotenzial eingeben."))
    ggplot(s, aes(jahr, zusatz / potenzial_gesamt(), colour = basis, linetype = variante, group = szenario)) +
      geom_hline(yintercept = 1, colour = sr$ink, linetype = "dashed") +
      linien() +
      scale_y_continuous(labels = function(x) paste(fmt(x * 100), "%"), limits = c(0, NA)) +
      labs(title = "Ausgeschöpftes Marktpotenzial", x = "Jahr", y = NULL) +
      theme_sr()
  }, bg = sr$cream)

  output$plot_mean <- renderPlot({
    s <- szenario()
    s <- linien_faktoren(s)
    ggplot(s, aes(jahr, faelle / entscheider, colour = basis, linetype = variante, group = szenario)) +
      linien() +
      scale_y_continuous(labels = fmt) +
      labs(title = "Ø Entscheidungen je Entscheider:in und Jahr",
           subtitle = "Sinkt der Wert, ist das Marktpotenzial ausgeschöpft (freie Kapazität).",
           x = "Jahr", y = NULL) +
      theme_sr()
  }, bg = sr$cream)

  output$szenario_tabelle <- renderUI({
    s <- szenario()
    pot <- potenzial_gesamt()
    zeilen <- lapply(unique(s$szenario), function(name) {
      x <- s[s$szenario == name, ]
      ende <- x[nrow(x), ]
      voll <- if (pot != 0) x$jahr[which(x$zusatz / pot >= 0.999)] else integer(0)
      tags$tr(
        tags$td(span(style = paste0("color:", farben()[[x$basis[1]]], "; font-weight:",
                                    if (x$variante[1] == "allein") 700 else 500, ";"), name)),
        tags$td(if (length(voll)) paste("Jahr", voll[1]) else "nicht im Zeitraum"),
        tags$td(eur(ende$umsatz)), tags$td(eur(ende$zusatz)),
        tags$td(pct(if (pot != 0) ende$zusatz / pot else NA)),
        tags$td(fmt(ende$entscheider, 1)),
        tags$td(fmt(ende$faelle / ende$entscheider, 1))
      )
    })
    div(style = "overflow-x: auto; margin-top: 1rem;",
      tags$table(class = "sr-table",
        tags$thead(tags$tr(
          tags$th("Szenario"), tags$th("Potenzial ausgeschöpft"),
          tags$th(paste("Umsatz Jahr", input$jahre)), tags$th("Zusatzumsatz"),
          tags$th("Ausschöpfung"), tags$th("Entscheider:innen"),
          tags$th("Ø Entsch. je Entscheider:in")
        )),
        tags$tbody(zeilen)
      )
    )
  })
}

shinyApp(ui, server)
