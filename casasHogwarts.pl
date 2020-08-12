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
permiteEntrar(Casa,Mago):-
    casa(Casa),
    mago(Mago).

permiteEntrar(slytherin,Mago):-
    mago(Mago),
    not(sangre(Mago,impura)).

cumpleCaracteristicasCasa(Casa,Mago):-
    casa(Casa),
    mago(Mago),
    forall(caracteristicasCasa(Casa,Caracteristica),caracteristicas(Mago,Caracteristica)).%todas las requisitos de la casa son cumplidos.

puedeEntrar(Casa,Mago):-
    mago(Mago),
    not(casaOdiada(Mago,Casa)),
    permiteEntrar(Casa,Mago),
    cumpleCaracteristicasCasa(Casa,Mago).

puedeEntrar(gryffindor,hermione).%pq lo hackeo.

cadenaDeAmistades(Mago):-
    mago(Mago),
    caracteristicas(Mago,amistoso),
    puedeEntrar(Casa,Mago),
    puedeEntrar(Casa,OtroMago),
    OtroMago \= Mago,
    cadenaDeAmistades(OtroMago).

accion(tercerPiso,-75).
accion(seccionRestringidaBiblioteca,-10).
accion(bosque,-50).
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

esDe(hermione,gryffindor).
esDe(ron,gryffindor).
esDe(harry,gryffindor).
esDe(draco,slytherin).
esDe(luna,ravenclaw).

queHizo(harry,fueraDeLaCama).
queHizo(harry,bosque).
queHizo(harry,tercerPiso).
queHizo(harry,ganarleAVoldemort).
queHizo(hermione,tercerPiso).
queHizo(hermione,seccionRestringidaBiblioteca).
queHizo(hermione,salvarAmigos).
queHizo(draco,mazmorras).
queHizo(ron,ganarAjedrezMagico).

esBuenAlumno(Mago):-
    mago(Mago),
    queHizo(Mago,_),
    forall(queHizo(Mago,Accion),(accion(Accion,Puntos),Puntos >=0)).

esRecurrente(Accion):-
    accion(Accion,_),
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
    casa(Casa),
    puntajeCasa(Casa,Puntaje),
    forall((puntajeCasa(OtraCasa,OtroPuntaje),OtraCasa \= Casa), Puntaje > OtroPuntaje).