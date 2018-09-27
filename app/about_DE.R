tags$html(
  tags$head(
    tags$title('Daten\xfcbersicht')
  ),
  tags$body(
    h3("Dateninformationen"),
    p("Diese Anwendung bietet eine benutzerfreundliche Schnittstelle zum Herunterladen der meteorologischen Daten des Portals Open Data S\xfcdtirol."),
    p("Die Anwendung Open Data S\xfcdtirol API und die dazugeh\xf6rige Dokumentation sind auf der Webseite der Autonomen Provinz Bozen verf\xfcgbar."),
    p(a( href='http://daten.buergernetz.bz.it/de/dataset/misure-meteo-e-idrografiche',
         target='_blank','Open Data S\xfcdtirol - Meteo data web service' )),
    p("Nachdem Sie die Stationen und Sensoren ausgew\xe4hlt haben, filtern Sie die Tabelle und / oder zeichnen Sie ein Polygon auf der Karte und w\xe4hlen Sie den gew\xfcnschten Zeitraum aus (die verf\xfcgbaren Daten beziehen sich auf die letzten 3 Jahre)."),
    p("Sobald der Daten-Download abgeschlossen ist, wird ein Symbol \x22speichern\x22 angezeigt und Sie k\xf6nnen die Datentabelle im csv- oder json-Format speichern. 
      Sie k\xf6nnen zwischen verschiedenen zeitlichen Aggregationen w\xe4hlen. \x22grezzo\x22 beh\xe4lt die urspr\xfcngliche Aufl\xf6sung der Datenerfassung bei. 
      (Die \xfcbliche Downloaddauer betr\xe4gt 5 oder 10 Minuten). F\xfcr die anderen Optionen der zeitlichen Aggregation (Stundenmittelwerte, Tagesmittelwerte, etc.) erhalten Sie andere Statistiken)."), 
    p("F\xfcr alle Messgr\xf6\xdfen wird die Gesamtzahl der fehlenden Daten f\xfcr den ausgew\xe4hlten Zeitraum angezeigt."),
    p("F\xfcr alle Messgr\xf6\xdfen, mit Ausnahme von Niederschlag und Windrichtung, wird der arithmetische Durchschnitt f\xfcr die spezifische zeitliche Aggregation berechnet. "),
    p("Niederschlag und Lufttemperatur ergeben sich aus der Gesamtsumme, bezogen auf die spezifische zeitliche Aggregation."), 
    p("Lufttemperatur und relative Luftfeuchtigkeit beinhalten auch die Minimal- und Maximalwerte, die sich auf die spezifische zeitliche Aggregation beziehen."),
    p("Die Windrichtung wird mit den folgenden Zahlencodes neu klassifiziert und der am h\xe4ufigsten Vorkommende wird den Zahlencodes zugewiesen:"), 
    p("1 = N; 2 = NO; 3 = O; 4 = SO; 5 = S; 6 = SW; 7 = W; 8 = NW."), 
    p("Sie k\xf6nnen zwischen verschiedenen Tabellenformaten w\xe4hlen, \x22breit\x22 oder \x22lange\x22.
      Das breite Tabellenformat enth\xe4lt f\xfcr alle Messgr\xf6\xdfen eine entsprechende Spalte. Das lange Tabellenformat beinhaltet eine Spalte, die die Gr\xf6\xdfe \x22Sensor\x22 identifiziert und eine Spalte \x22Value\x22 die den Messgr\xf6\xdfen entspricht."),
    br(),
    
    h3("Haftungsausschluss"),
    p("Die Qualit\xe4t der mit dieser Anwendung heruntergeladenen meteorologischen Daten ist nicht garantiert, da sie weder vom Datenanbieter, noch von den Entwicklern dieser WebApp validiert wurden. 
      Diese WebApp hat ausschlie\xdflich den Zweck, den Zugriff auf die meteorologischen und Hydrologischen Daten des Portals Open Data S\xfcdtirol zu erleichtern."),
    
    br(),
    
    h3("Starten Sie die Anwendung unter: Von Github heruntergeladen"),
    p("Der Quellcode ist unter folgendem Link verf\xfcgbar:"),
    p(a( href='https://github.com/GiulioGenova/ODBZ/',
         target='_blank','Giulio Genova - ODBZ' )),
    p("Um die Anwendung zu starten, f\xfchren Sie die folgende Zeichenkette aus:"),
    p("=========================================================================="),
    p("if (!require(\x22shiny\x22)) install.packages(\x22shiny\x22)"),
    p("shiny::runGitHub('GiulioGenova/ODBZ',subdir=\x22app\x22,launch.browser = TRUE)"),
    p("=========================================================================="),
    p("Dadurch werden automatisch alle notwendigen Pakete auf Ihrem Computer installiert (nur bei der ersten Ausf\xfchrung erforderlich) und die Webanwendung wird gestartet."),
    br(),
    
    h3("Autor"),
    p(a( href='https://github.com/GiulioGenova/',
         target='_blank','Giulio Genova' )),
    p("eurac - Institut f\xfcr Alpine Umwelt"),
    p("mail: giulio.genova@eurac.edu"),
    p("2018-09-27"),
    br(),
    h3("Danksagungen"),
    p("Wir m\xf6chten uns bei Mattia Rossi bedanken, der das Paket R\x22MonalisR\x22 entwickelt  und eine fundamentale Rolle im Backend dieser Anwendung eingenommen hat. Das Paket finden Sie unter folgendem Link:"),
    
    p(a( href='https://gitlab.inf.unibz.it/earth_observation_public/MonalisR', 
         target='_blank','MonalisR'))
    
    )
  )