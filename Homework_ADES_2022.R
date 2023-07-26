#Carico il data-frame wvs e verifico se l'operazione è avvenuta correttamente
#Se tutto avviene corretamente, nel dataset di R deve essere presente solo l'oggetto wvs
rm(list=ls())
load('C:\\Users\\Thomas De Massari\\Documents\\Homework ADES\\WorldValueSurvey.rdata'); ls() 

#Visualizzo la sitensi delle variabili e del dataset a video
names(wvs); str(wvs)

library(rmf) #carico la libreria rmf

#ESERCIZIO 1
#L'analisi dovrà essere condotta sulla variabile v119, che contiene le risposte, su una scala di Likert, alla domanda "la competizione è positiva?". Alla risposta "si" è attribuito il valore 1, mentre alla risposta "no" è attribuito il valore 10, contemplano le sfumature intermedie da 2 a 9. Se l'intervistato non sa rispondere o non vuole, viene attribuito rispettivamente il valore -1 e -2.

v119 = wvs$v119
#Distribuzione per frequenze
#Distribuzione per frequenze assolute
FreAss = table(v119)
#Distribuzione per frequenze relative
FreRel = prop.table(table(v119))
#Distribuzione per frequenza percentuale
FrePer = prop.table(table(v119))*100
#Creo una dataset contenente la variabile in questione e le sue distribuzioni di frequenza, calcolate sopra
cbind(FreAss,FreRel,FrePer)

#Media con R e con RMF
(miR = mean(v119, na.rm=TRUE)) #è necessario specificare di non considerare i dati mancanti
(miRMF = media(v119)) #in automatico non  considera i dati mancanti

#Mediana con R e con RMF (discorso analogo alla media)
median(v119, na.rm=TRUE)
mediana(v119)

#Calcolo della varianza
#1. Calcolo della devianza, esludendo i dati mancanti
devianza = sum((v119-miR)^2,na.rm=TRUE)
#2. Calcolo della varianza con conseguente stampa a video del risultato
N = sum(FreAss) #dimensione della popolazione, al netto dei dati mancati
(varianza = devianza/N)
#Con R
var(v119,na.rm=TRUE)*((N-1)/N)

#Calcolo della deviazione standard 
sqrt(varianza)
#con R
sd(v119,na.rm=TRUE)*((N-1)/N)

#Si può concludere che gli il 50% degli intervistati hanno espresso un giudizio minore o uguale a 4, mostrando quindi una leggera proprensione per la competizione. 


#ESERCIZIO 2
#L'analisi dovrà essere condotta sulla variabile v119 e sulla variabile area, che raccoglie le zone d'Italia di provenienza degli intervistati (nord-ovest, nord-est, centro, sud e isole)
CompetitionIsGood = wvs$v119
Area = wvs$area

#gli output prodotti sono i medesimi, solo che tapply è più sintetica
by(CompetitionIsGood, Area, mean, na.rm=TRUE)
tapply(CompetitionIsGood,Area,mean,na.rm=TRUE)

#I dati in questione mostrano che nel nord-est d'Italia c'è una leggera propensione per la competizione rispetto alle altre aree d'Italia, seppur non netta in quanto il valore medio è approssimabile a 4. Le altre regioni invece sono più conformi alla media nazionale.


#ESERCIZIO 3
CompetitionIsGood = wvs$v119
Area = wvs$area
boxplot(CompetitionIsGood~Area,horizontal=TRUE,xlab="Competition is good",ylab="Area geografica")

#Il box plot rappresenta in modo esplicito la situazione descritta sopra: il nord-est si distacca dall'andamento mediano nazionale, mentre il centro, il sud e le isole hanno un comportamento simile. 


#ESERCIZIO 4
#L'analisi dovrà essere condotta sulle variabili v120 e v207, che contengono rispettivamente le risposte alle domande "lavorare duramente nel lungo periodo conduce di solito al successo?" e "non è mai giustificato suicidarsi". Con 1 viene rappresentata la risposta affermativa e con 10 la risposta negativa. Al solito, si associa -1 e -2 alle risposte "non so" e "non voglio rispondere".
c3 = wvs$v120
c4 = wvs$v207

x = c3[complete.cases(c3,c4)]
cov(c3,c4,use="complete.obs")*(length(x)-1)/length(x)
cor(c3,c4,use="complete.obs")
hardworking=c3
plot(hardworking,c4,ylab="suicide")

#Si può concludere che non c'è nessuna correlazione tra la convinzione o meno che il duro lavoro porti al successo e la giustificazione del suicidio. A sostegno di ciò, oltre al coefficiente di correlazione lineare prossimo allo zero, la rappresentazione grafica mostra come le varie osservazioni sono posizionate in modo caotico sul piano cartesiano.


#ESERCIZIO 5
dado = c(1:20) #dado con 20 facce
lanci = 6 #numero di lanci
c7 = 18 #valore della colonna 7

#lancio il dado 100000 volte
nrep = 100000
ris = numeric(nrep)
for (i in 1:nrep)
{
  x = max(sample(dado,size=lanci,replace=TRUE))
  ris[i] = x
}
#output della prova
frq(ris)
#maggiore o uguale al valore della colonna 7
ok = ris>=c7
out = ris[ok]
frq(out)
#Probabilità stimata con il metodo Monte Carlo (# successi/# prove) che esca un numero maggiore della colonna 7
sum(ok)/nrep


#ESERCIZIO 6
pins = 0.10
crediti = 256
c10 = 27

#valore atteso di insolveza
(vadi = pins*crediti)
#P(X>E) con R e con RMF
pbinom(vadi,crediti,pins,lower.tail=FALSE)
Binomiale(crediti,pins,da=round(vadi,0),dettaglio=FALSE,grafico=FALSE)
#P(X>c10) con R e con RMF
pbinom(c10,crediti,pins,lower.tail=FALSE)
Binomiale(crediti,pins,da=c10+1,dettaglio=FALSE,grafico=FALSE)

#Il numero di insolvenze attese a due anni è 25.6, ma la probabilità che se ne verifichino di più è di 0.49. Allo stesso tempo, la probabilità che il numero di insolvenze a due anni sia maggiore di 27 è di 0.34


#ESERCIZIO 7
mi = 6.1
sigma = 9.7
c13 = 16.8

#standardizzazione dei due estremi 
(estremo1 = ((mi-sigma)-mi)/sigma)
(estremo2 = (c13-mi)/sigma)
#calcolo della probabilità
pnorm(estremo2)-pnorm(estremo1)


#ESERCIZIO 8
#generazione del vettore
set.seed(226091); x<- sample(-3:5,size=300,replace=TRUE); set.seed(NULL)

#a) calcolo della mediana
frequenze(x,cumul=TRUE) #cerco la modalità che lascia prima di se il 50% e dopo di se il 50%
#le mediana sarà 1
median(x,na.rm=TRUE) #mediana con i dati grazzi

#b) dimostro che la media degli scarti in valore assoluto dalla mediana è minore della media degli scarti in valore assoluto da un qualsiasi altro valore
ValoreCasuale = runif(1,-200,100) #generazione del numero casuale
oggetto1 = sum(abs(x-median(x,na.rm=TRUE))/length(x))
oggetto2 = sum(abs(x-ValoreCasuale)/length(x))
0 > oggetto1-oggetto2 #se la dimostrazione è avvenuta correttamente deve risultare "TRUE"


#ESERCIZIO 9
#vettori dei contagiati 
contagiatiM = c(rep(5,444638),rep(15,667351),rep(25,697747),rep(35,675651),rep(45,791996),rep(55,778178),rep(65,469786),rep(75,305004),rep(85,163360),rep(95,32024))
(length(contagiatiM)) #devono essere 5025735
contagiatiF= c(rep(5,413787),rep(15,644080),rep(25,694093),rep(35,743312),rep(45,903908),rep(55,827714),rep(65,466019),rep(75,309141),rep(85,229827),rep(95,93094))
(length(contagiatiF)) #devono essere 5324975
contagiatiT = c(rep(5,444638+413787),rep(15,667351+644080),rep(25,697747+694093),rep(35,675651+743312),rep(45,791996+903908),rep(55,778178+827714),rep(65,469786+466019),rep(75,305004+309141),rep(85,163360+229827),rep(95,32024+93094))
length(contagiatiT) #devono essere 10350710

#vettori dei morti
mortiM = c(rep(5,7),rep(15,15),rep(25,61),rep(35,222),rep(45,948),rep(55,3808),rep(65,10785),rep(75,24300),rep(85,31274),rep(95,9870))
(length(mortiM)) #devono essere 81290
mortiF = c(rep(5,10),rep(15,13),rep(25,34),rep(35,128),rep(45,429),rep(55,1535),rep(65,4278),rep(75,11978),rep(85,26480),rep(95,18144))
(length(mortiF)) #devono essere 63029
mortiT = c(rep(5,7+10),rep(15,15+13),rep(25,61+34),rep(35,222+128),rep(45,948+429),rep(55,3808+1535),rep(65,10785+4278),rep(75,24300+11978),rep(85,31274+26480),rep(95,9870+18144))
(length(mortiT)) #devono essere 144319

#per avere tutti i grafici in un'unica schermata per comodità
par(mfrow=c(2,3))

#istogramma per la variabile "età al contagio"
istogramma(contagiatiM,da=0,a=100,nclassi=10, stampa=FALSE, nome="Età al contagio - Maschi")
istogramma(contagiatiF,da=0,a=100,nclassi=10, stampa=FALSE, nome="Età al contagio - Femmine")
istogramma(contagiatiT,da=0,a=100,nclassi=10, stampa=FALSE, nome="Età al contagio - Totali")
#istogramma per la variabile età al decesso"
istogramma(mortiM,da=0,a=100,nclassi=10, stampa=FALSE, nome="Età al decesso - Maschi")
istogramma(mortiF,da=0,a=100,nclassi=10, stampa=FALSE, nome="Età al decesso - Femmine")
istogramma(mortiT,da=0,a=100,nclassi=10, stampa=FALSE, nome="Età al decesso - Totali")

#Osservando i grafici è possibile notare come le classi d'età maggiormente contagiate sono quelle centrali, mentre gli anziani hanno probabilità maggiore di morire, se incontrano il virus. 

#chiudo la finestra multipla dei grafici
par(mfrow=c(1,1))

#media, mediana e sd per i maschi per la variabile "età al contagio"  
(miCM = media(contagiatiM))
frequenze(contagiatiM,cumul=TRUE) #la classe mediana è 40-49
(medCM = 40+((0.5-0.49453204)/(0.65212014-0.49453204))*10)
(sqmCM = sd(contagiatiM, na.rm=TRUE))

#media, mediana e sd per i maschi per la variabile "età al decesso" 
(miMM = media(mortiM))
frequenze(mortiM,cumul=TRUE) #la classe mediana è 70-79
(medMM = 70+((0.5-0.1949317)/(0.4938615-0.1949317))*10)
(sqmMM = sd(mortiM, na.rm=TRUE))

#media, mediana e sd per le femmine per la variabile "età al contagio" 
(miCF = media(contagiatiF))
frequenze(contagiatiF,cumul=TRUE) #la classe mediana è 40-49
(medCF = 40+((0.5-0.46859788)/(0.63834666-0.46859788))*10)
(sqmCF = sd(contagiatiF, na.rm=TRUE))

#media, mediana e sd per le femmine per la variabile "età al decesso" 
(miMF = media(mortiF))
frequenze(mortiF,cumul=TRUE) #la classe mediana è 80-89
(medMF = 80+((0.5-0.2920084406)/(0.7121325104-0.2920084406))*10)
(sqmMF = sd(mortiF, na.rm=TRUE))

#media, mediana e sd per la totalità dei dati per la variabile "età al contagio" 
(miCT = media(contagiatiT))
frequenze(contagiatiT,cumul=TRUE) #la classe mediana è 40-49
(medCT = 40+((0.5-0.48119008)/(0.64503430-0.48119008))*10)
(sqmCT = sd(contagiatiT, na.rm=TRUE))

#media, mediana e sd per la totalità dei dati per la variabile "età al decesso" 
(miMT = media(mortiT))
frequenze(mortiT,cumul=TRUE) #la classe mediana è 80-89
(medMT = 80+((0.5-0.4057054165)/(0.8058883446-0.4057054165))*10)
(sqmMT = sd(mortiT, na.rm=TRUE))

#riassunto per medie e mediane per la variabile "età al contagio" e "età al decesso"
MisuraDiSintesi=c("media - maschi","mediana - maschi","media - femmine","mediana - femmine","media - totale","mediana - totale")
EtàAlContagio=round(c(miCM,medCM,miCF,medCF,miCT,medCT),4)
EtàAlDecesso=round(c(miMM,medMM,miMF,medMF,miMT,medMT),4)
cbind(MisuraDiSintesi,EtàAlContagio,EtàAlDecesso)

#Come si evince dal prospetto di sintesi soprariportato media e mediana, per la variabile "età al contagio" sono molto simili, a riprova del fatto che assume una forma tendenzialmente campanulare con una asimmetria destra 
#Discorso simile anche per la variabile "età al decesso", che presenta anche'essa media e mediana appartenti alla stessa classe. L'istogramma  evidenzia però un'asimmetria destra pronunciata, mettendo in luce che con un numero basso di contagiati, in proporzione alle altri classi, gli anziani hanno un tasso di mortalità nettamente più elevato

#Perché l'età mediana al contagio ottenuta eseguendo lo script (41.15) è diversa da quella fornita dall'Istituto Superiore di Sanità (47)? Questa discrepanza tra dati è dovuta al fatto che l'ISS utilizza i dati grezzi per calcolare le misure di sintesi, mentre lo script sopra eseguito fa riferimento a una distribuzione per classi (minore contenuto informativo). Entrambi i dati ricadono infatti nella classe mediana 40-49.