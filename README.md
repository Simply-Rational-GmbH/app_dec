# Entscheidungs-Potenzialrechner

Shiny-App von Simply Rational: Entscheidungen mit Fällen, Umsatz und Marktpotenzial erfassen und
sehen, wie zusätzliche Entscheider:innen und Simply Rational Szenarien das Potenzial über die Zeit
ausschöpfen.
Gestaltung nach [simplyrational.de](https://www.simplyrational.de) und dem
[Data Science Blog](https://simply-rational-gmbh.github.io/datascienceblog/).

## Dateien

| Datei | Zweck |
|---|---|
| `app.R` | Die App (UI + Server) |
| `www/sr.css`, `www/simply-rational-logo.webp` | Header, Hero, Footer und Karten im Simply-Rational-Stil |
| `manifest.json` | R-Version und Paketliste für Posit Connect Cloud |
| `_brand.yml` | Farben und Schriften des CI (Referenz) |
| `design/` | Quarto-Konfiguration und CSS des Blogs (nur Referenz, wird nicht deployt) |

## Lokal starten

```bash
Rscript -e "shiny::runApp('.', port = 3838)"
```

## Auf Posit Connect Cloud veröffentlichen

1. Repository zu GitHub pushen.
2. In [connect.posit.cloud](https://connect.posit.cloud) **Publish → Shiny** wählen, Repository und
   Branch `main` auswählen, `app.R` als Primary File.
3. Bei jedem Push auf `main` kann Connect Cloud automatisch neu veröffentlichen
   („Republish on push“ aktivieren).

Nach Änderungen an den verwendeten Paketen `manifest.json` neu erzeugen:

```bash
Rscript -e "rsconnect::writeManifest(appFiles = c('app.R', 'www/sr.css', 'www/simply-rational-logo.webp'), appPrimaryDoc = 'app.R')"
```

## URL-Parameter

Alle Eingaben lassen sich über die URL setzen, und die App schreibt jede Änderung zurück in die
Adresszeile. Jeder Zustand ist damit ein teilbarer Link („Szenario teilen“ in der App).
Positionen beginnen bei 1.

| Parameter | Bedeutung |
|---|---|
| `d1` | Name der 1. Entscheidung |
| `d1_n` | Anzahl Entscheider:innen |
| `d1_o1` | Name der 1. Option dieser Entscheidung |
| `d1_o1_cases` | Fälle pro Jahr |
| `d1_o1_revenue` | Umsatz pro Jahr in € (negativ = Risiko/Verlust) |
| `d1_o1_market` | Marktpotenzial in € |
| `sc` | Simply Rational Szenarien, kommagetrennt (siehe unten); `sc=0` = keins |
| `e1_pct` | Eigenes Szenario: Steigerung in % |
| `e1_year` | Eigenes Szenario: ab Jahr (1–20) |
| `add` | Zusätzliche Entscheider:innen pro Jahr |
| `years` | Zeithorizont in Jahren (1–20) |
| `d=0` | Keine Entscheidungen |

Simply Rational Szenarien (Steigerung der Entscheidungen je Entscheider:in, Events wirken multiplikativ):

| `sc`-Wert | Szenario | Events |
|---|---|---|
| `poc` | Entscheidungslogik PoC | Jahr 1: +5 % |
| `logik` | Entscheidungslogik | Jahr 1: +5 %, Jahr 2: +10 % |
| `klass` | Fallklassifizierung | Jahr 1: +15 % |
| `ade` | Augmented Decision Excellence | Jahr 1: +5 %, Jahr 2: +10 %, Jahr 3: +20 % |
| `eigen` | Eigenes Szenario | frei über `e1_pct`, `e1_year`, … |

Jedes ausgewählte Szenario erscheint in den Grafiken zweimal: allein (durchgezogen) und kombiniert
mit den zusätzlichen Entscheider:innen (gestrichelt), dazu die Linie „Mehr Entscheider:innen“ allein. Fehlt `sc`, sind die vier
festen Szenarien ausgewählt (plus `eigen`, falls `e…`-Parameter gesetzt sind).

Fehlen alle `d…`-Parameter, startet die App mit dem Beispiel. Fehlende Zahlen einer
Option sind 0. Texte URL-kodieren (`%20` für Leerzeichen, `%C3%BC` für ü usw.).

Beispiel:

```
?d1=Antrag%20versichern%3F&d1_n=5&d1_o1=Ja&d1_o1_cases=1000&d1_o1_revenue=500000&d1_o1_market=5000000&d1_o2=Nein&d1_o2_cases=300&sc=logik,ade,eigen&e1_pct=15&e1_year=2&add=1&years=10
```

Auf Connect Cloud müssen die Parameter an die direkte URL der App
(`https://<id>.share.connect.posit.cloud/`) angehängt werden, nicht an die Seite mit dem
Connect-Cloud-Rahmen. Der Link aus „Szenario teilen“ hat bereits diese Form.
