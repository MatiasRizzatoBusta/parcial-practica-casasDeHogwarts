%casa(nombre).
casa(gryffindor).
casa(hufflepuff).
casa(ravenclaw).
casa(slytherin).

%magos(Nombre).
mago(harry).
mago(draco).
mago(hermione).
mago(ron).
mago(luna).

%casaOdiada(nombre,casa odiada).
casaOdiada(harry,slytherin).
casaOdiada(draco,hufflepuff).

%sangre(mago,Sangre).
sangre(harry,mestizo).
sangre(draco,pura).
sangre(hermione,impura).

%caracteristicas(mago,caracteristicas).
caracteristicas(harry,corajudo).
caracteristicas(harry,amistoso).
caracteristicas(harry,orgulloso).
caracteristicas(harry,inteligente).
caracteristicas(draco,inteligente).
caracteristicas(draco,orgulloso).
caracteristicas(hermione,inteligente).
caracteristicas(hermione,orgulloso).
caracteristicas(hermione,responsable).

%clasesCasa(casa,caracteristica imortante).
caracteristicasCasa(gryffindor,corajudo).
caracteristicasCasa(slytherin,orgulloso).
caracteristicasCasa(slytherin,inteligente).
caracteristicasCasa(ravenclaw,inteligente).
caracteristicasCasa(ravenclaw,responsable).
caracteristicasCasa(hufflepuff,amistoso).

%permiteEntrar(Casa,Mago).
permiteEntrar(Casa,Mago):- % no le pongo _ pq solo quiero que entren magos
    casa(Casa),
    mago(Mago), 
    Casa \= slytherin.

permiteEntrar(slytherin,Mago):-
    mago(Mago), %le agrego mago pq abajo estoy usando un not. puedo no ponerlo y hacer sangre(Mago,Sangre),Sangre \= impura.
    not(sangre(Mago,impura)).

cumpleCaracteristicasCasa(Casa,Mago):-
    casa(Casa), % ligo la casa y el mago para el forall
    mago(Mago),
    forall(caracteristicasCasa(Casa,Caracteristica),caracteristicas(Mago,Caracteristica)).%todas las requisitos de la casa son cumplidos.

puedeEntrar(Casa,Mago):-
    mago(Mago),
    permiteEntrar(Casa,Mago),
    not(casaOdiada(Mago,Casa)), %no lo pongo antes pq sino anda, pero solo me devuelve a hermione.los demas los tenia q probar 1x1.
    cumpleCaracteristicasCasa(Casa,Mago).

puedeEntrar(gryffindor,hermione).%pq lo hackeo.

cadenaDeAmistades(Mago):-
    mago(Mago),
    caracteristicas(Mago,amistoso),
    puedeEntrar(Casa,Mago),
    puedeEntrar(Casa,OtroMago),
    OtroMago \= Mago,
    cadenaDeAmistades(OtroMago).

%lugar(Lugar,PuntosQueSaca).
lugar(mazmorras,0).
lugar(bosque,-50).
lugar(seccionRestringidaBiblioteca,-10).
lugar(tercerPiso,-75).

accion(irA(Lugar),Puntos):-
    lugar(Lugar,Puntos).

accion(fueraDeLaCama,-50).
accion(ganarAjedrezMagico,50).
accion(salvarAmigos,50).
accion(ganarleAVoldemort,60).
accion(mazmorras,0).

accion(responderPregunta(Pregunta),Puntos):-
    puntosPregunta(Pregunta,Puntos).

puntosPregunta(Pregunta,Puntos):-
    pregunta(Pregunta,Puntos,_).

puntosPregunta(Pregunta,Puntos):-
    pregunta(Pregunta,PuntosTotal,snape),
    Puntos is PuntosTotal / 2.

%rPregunta(preg,dificultad,profesor)
pregunta(dondeEstaUnBezoar,20,snape).
pregunta(comoLevantarVarita,25,flitwick).

queHizo(harry,fueraDeLaCama).
queHizo(harry,irA(bosque)).
queHizo(harry,irA(tercerPiso)).
queHizo(harry,ganarleAVoldemort).
queHizo(hermione,irA(tercerPiso)). 
queHizo(hermione,irA(seccionRestringidaBiblioteca)).
queHizo(hermione,salvarAmigos).
queHizo(hermione,responderPregunta(dondeEstaUnBezoar)).
queHizo(hermione,responderPregunta(comoLevantarVarita)).
queHizo(draco,irA(mazmorras)).
queHizo(ron,ganarAjedrezMagico).

esDe(hermione,gryffindor).
esDe(ron,gryffindor).
esDe(harry,gryffindor).
esDe(draco,slytherin).
esDe(luna,ravenclaw).

esBuenAlumno(Mago):-
    queHizo(Mago,_), %no pongo mago(Mago), porque aca ya lo estoy ligando.
    forall(queHizo(Mago,Accion),(accion(Accion,Puntos),Puntos >=0)).

esRecurrente(Accion):-
    accion(Accion,_),%ligo la accion.
    findall(Accion,queHizo(_,Accion),ListaDeAccion),
    length(ListaDeAccion, CantDeVecesQueSeRealizo),
    CantDeVecesQueSeRealizo > 1.

puntajeCasa(Casa,PuntajeTotal):-
    casa(Casa),
    findall(Puntaje,(esDe(Mago,Casa),tomoPuntosPorAccion(Mago,Puntaje)),ListaConLosPuntos), 
    sumlist(ListaConLosPuntos, PuntajeTotal).

%el esDe en el medio toma a cada mago de la casa y busca su puntaje x accion,y los guarda en una lista
   
tomoPuntosPorAccion(Mago,Puntos):-
    queHizo(Mago,Accion),
    accion(Accion,Puntos).

casaGanadora(Casa):-
    puntajeCasa(Casa,Puntaje), %no pongo casa(Casa) pq la ligo en puntaje casa.
    forall((puntajeCasa(OtraCasa,OtroPuntaje),OtraCasa \= Casa), Puntaje > OtroPuntaje).