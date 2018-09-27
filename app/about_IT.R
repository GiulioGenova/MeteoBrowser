tags$html(
  tags$head(
    tags$title('Panoramica dei dati')
  ),
  tags$body(
    h3("Informazioni sui dati"),
    p("
      Questa applicazione fornisce un'interfaccia user-friendly per scaricare i dati meteorologici del potale Open Data Alto Adige"),
    p("L'API Open Data Alto Adige e la relativa documentazione sono disponibili sul sito web della Provincia di Bolzano."),
    p(a( href='http://daten.buergernetz.bz.it/it/dataset/misure-meteo-e-idrografiche',
         target='_blank','Open Data Alto Adige - Meteo data web service' )),
    p("Una volta selezionate le stazioni e i sensori, filtrando la tabella e / o disegnando un poligono sulla mappa, e scelto l'arco temporale d'interesse (i dati disponibili si riferiscono agli ultimi tre anni)."), 
    p("Quando lo scarico dati \x232 completato, un'icona \x22salva\x22 vine mostrata e si può scegliere di salvare la tabella di dati in formato csv o json. 
      Si puo scegliere tra diverse aggregazioni temporali. \x22grezzo\x22 mantiene l'originale risoluzione d'acquisizione dei dati
      (tipicamente 5 o 10 minuti). Per le altre opzioni di aggregazione temporale (medie orarie, giornaliere, ecc.) si avranno diverse statistiche per le diverse variabili."), 
    p("Per tutte le variabili misurate si avr\x224 il numero totale di dati mancanti per il periodo selezionato."),
    p("per tutte le variabili misurate, fatta eccezione per la precipitazione e direzione del vento \x232 compiuta la media aritmetica per la specifica aggregazione temporale. "),
    p("Precipitazione e temperatura dell'aria avranno anche la somma totale riferita alla specifica aggregazione temporale."), 
    p("Temperatura dell'aria e umidit\x224 relativa avranno anche i valori minimi e massimi riferiti alla specifica aggregazione temporale."),
    p("La direzione del vento \x232 riclassificata con i seguenti codici numerici e il píu frequente sar\x224 assegnato:"), 
    p("1 = N; 2 = NE; 3 = E; 4 = SE; 5 = S; 6 = SO; 7 = O; 8 = NO."), 
    p("Si può scegliere tra diversi formati di tabella,\x22larga\x22 or \x22lunga\x22.
      Nel formato di tabella largo si ottengono una colonna per ogni variabile misurata. Nel formato di tabella lungo si ottengono  una colonna che identifica la misura \x22Sensor\x22 e una colonna 
      \x22Value\x22 che corrisponde alle misure."),
    br(),
    
    h3("Dichiarazione di esclusione di responsabilit\x224"),
    p("Dei dati metereologici scaricati con questa applicazione non ne sono garantiti la qualit\x224 in quanto non validati dal fornitore dei dati, tanto meno dagli svliuppatori di questa WebApp. 
      La seguente WebApp ha il solo intento di facilitare l'accesso ai dati metereologici e idrologici del portale Open Data Alto Adige"),
    
    br(),
    
    h3("Lancia l'applicazione in locale: Scaricata da Github"),
    p("Il codice sorgente \x232 ottenibile nel seguente link:"),
    p(a( href='https://github.com/GiulioGenova/ODBZ/',
         target='_blank','Giulio Genova - ODBZ' )),
    p("Per lanciare l'applicazione in locale, eseguire la seguente stringa di codice:"),
    p("=========================================================================="),
    p("if (!require(\x22shiny\x22)) install.packages(\x22shiny\x22)"),
    p("shiny::runGitHub('GiulioGenova/ODBZ',subdir=\x22app\x22,launch.browser = TRUE)"),
    p("=========================================================================="),
    p("Questo installer\x224 automaticamente tutti i pacchetti necessari sul vostro computer (necessario solo la prima volta che viene eseguito) e lanciera l'applicazione web."),
    br(),
    
    h3("Autore"),
    p(a( href='https://github.com/GiulioGenova/',
         target='_blank','Giulio Genova' )),
    p("Eurac - Istituto per l'Ambiente Alpino"),
    p("e-mail: giulio.genova@eurac.edu"),
    p("2018-09-26"),
    br(),
    h3("Ringraziamenti"),
    p("Rignaziamo Mattia Rossi il quale ha sviluppato il pacchetto R\x22MonalisR\x22 che svolge un ruolo fondamentale nel backend di questa pplicazione. Il pacchetto puó essere trovato al seguente link:"),
    
    p(a( href='https://gitlab.inf.unibz.it/earth_observation_public/MonalisR', 
         target='_blank','MonalisR'))
    
    )
    )