%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PARTE 1 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
casa(gryffindor).
casa(hufflepuff).
casa(slytherin).
casa(ravenclaw).

mago(harry,sangre(mestiza)).
mago(draco,sangre(pura)).
mago(hermione,sangre(impura)).
mago(neville,sangre(pura)).
mago(luna,sangre(pura)).

caracteristicaMago(harry,inteligente).
caracteristicaMago(harry,corajudo).
caracteristicaMago(harry,amistoso).
caracteristicaMago(harry,orgulloso).
caracteristicaMago(draco,inteligente).
caracteristicaMago(draco,orgulloso).
caracteristicaMago(hermione,inteligente).
caracteristicaMago(hermione,orgulloso).
caracteristicaMago(hermione,responsable).
caracteristicaMago(neville,corajudo).
caracteristicaMago(neville,responsable).
caracteristicaMago(neville,amistoso).

casaQueOdia(harry,slytherin). 
casaQueOdia(draco,hufflepuff).
%no los puedo poner dentro de mago como functor pq por ejemplo hermione no odia a ninguna y si le pongo _ queda como que odia a todas.

criterioCaracterCasa(gryffindor,corajudo).
criterioCaracterCasa(slytherin,orgulloso).
criterioCaracterCasa(slytherin,inteligente).
criterioCaracterCasa(ravenclaw,inteligente).
criterioCaracterCasa(ravenclaw,responsable).
criterioCaracterCasa(hufflepuff,amistoso).

permiteEntrar(Casa,Mago):-
    casa(Casa),
    mago(Mago,_),
    cumpleCriterioCasa(Casa,Mago). %no le mando solo la sangre y caracteristicas pq si agrego mas casas y cambia algun criterio tengo
    %que cambiar todo.

cumpleCriterioCasa(Casa,Mago):-
    casa(Casa),
    Casa \= slytherin, % si no le pongo esto entra con slytherin por aca tmb.
    mago(Mago,_). %que solo ande con magos de la base de datos.

cumpleCriterioCasa(slytherin,Mago):-
    mago(Mago,_),
    not(mago(Mago,sangre(impura))).

tieneCaracterApropiado(Mago,Casa):-
    casa(Casa),
    mago(Mago,_),
    forall(criterioCaracterCasa(Casa,CaracteristicaBuscada),caracteristicaMago(Mago,CaracteristicaBuscada)).
%para cada caracteristica de la casa el mago la tiene. si lo hago al reves estoy haciendo que la casa tenga todas las caracteristicas
%entonces con que la casa no tenga una que el mago tiene rompe y no es lo que me piden.

puedeQuedarSeleccionado(Mago,Casa):-
    casa(Casa),
    mago(Mago,_),
    permiteEntrar(Casa,Mago),
    tieneCaracterApropiado(Mago,Casa),
    not(casaQueOdia(Mago,Casa)).

puedeQuedarSeleccionado(hermione,gryffindor). %hackeo el sistema.

cadenaDeAmistades(ListaMagos):-
    todosAmistosos(ListaMagos),
    todosEnLaMismaCasa(ListaMagos).

todosAmistosos(ListaMagos):-
    forall(member(Mago,ListaMagos),caracteristicaMago(Mago,amistoso)).

todosEnLaMismaCasa(ListaMagos):-
    member(Mago,ListaMagos),
    puedeQuedarSeleccionado(Mago,Casa),
    forall((member(OtroMago,ListaMagos),OtroMago \= Mago),(puedeQuedarSeleccionado(OtroMago,OtraCasa),OtraCasa == Casa)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PARTE 2 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%irA(Lugar),puntos
lugar(bosque,-50).
lugar(mazmorras,0).
lugar(tercerPiso,-75).
lugar(seccionProhibidaBiblioteca,-10).

%ACCIONES CON SUS PUNTOS:
puntosAccion(fueraDeLaCama,-50).
puntosAccion(ganarAjedrezMagico,50).
puntosAccion(salvarAmigos,50).
puntosAccion(ganarleAVoldemort,60).
puntosAccion(tenerCoraje,70).
puntosAccion(ayudarHarry,20).
puntosAccion(explicarHechizos,40).


puntosAccion(irA(Lugar),PuntosLugar):-
    lugar(Lugar,PuntosLugar).

puntosAccion(responderPregunta(Pregunta,Profesor),PuntosPregunta):-
    Profesor \= snape,
    puntosPregunta(Pregunta,PuntosPregunta).

puntosPregunta(responderPregunta(Pregunta,snape),PuntosPregunta):-
    puntosPregunta(Pregunta,Puntos),
    PuntosPregunta is (Puntos / 2).

%PREGUNTAS CON SUS DIFICULTADES: (la dificultad de la pregunta son los puntos que da)
puntosPregunta(dondeSeEncuentraUnBezoar,20).
puntosPregunta(comoLevantarVarita,25).

/*
El polimorfismo me permite darle un trato diferente a cada tipo de accion.Gracias a esto solo tengo que agregar las funciones 
que vaya a usar para conseguir los puntos de la accion responder pregunta sin necesidad de modificar el resto de las funciones que
usan los puntos de las acciones.
*/

%ACCIONES DE LOS ALUMNOS:
queHizo(harry,fueraDeLaCama).
queHizo(harry,ira(bosque)).
queHizo(harry,irA(tercerPiso)).
queHizo(harry,ganarleAVoldemort).
queHizo(hermione,irA(tercerPiso)).
queHizo(hermione,irA(seccionProhibidaBiblioteca)).
queHizo(hermione,salvarAmigos).
queHizo(draco,irA(mazmorras)).
queHizo(ron,ganarAjedrezMagico).
queHizo(neville,ganarAjedrezMagico).
queHizo(neville,tenerCoraje).
queHizo(luna,ayudarHarry).
queHizo(luna,explicarHechizos).
queHizo(hermione,responderPregunta(dondeSeEncuentraUnBezoar,snape)).
queHizo(hermione,responderPregunta(comoLevantarVarita,flitwick)).

%CASAS DE LOS MAGOS:
esDe(hermione,gryffindor).
esDe(ron,gryffindor).
esDe(harry,gryffindor).
esDe(draco,slytherin).
esDe(luna,ravenclaw).
esDe(neville,gryffindor).

esBuenAlumno(Mago):-
    queHizo(Mago,_), %hizo algo
    forall(queHizo(Mago,Accion),not(hizoAccionMala(Accion))). %todas son acciones buenas.
    
hizoAccionMala(Accion):-
    puntosAccion(Accion,Puntos),
    Puntos < 0.

esAccionRecurrente(Accion):-
    queHizo(_,Accion),
    findall(Accion,queHizo(_,Accion),ListaCantDeVecesHecha),
    length(ListaCantDeVecesHecha, CantDeVecesHecha),
    CantDeVecesHecha > 1.
/*
otraForma sin forall 
esAccionRecurrente(Accion):-
    queHizo(Mago,Accion),
    queHizo(OtroMago,Accion),
    Mago \= OtroMago.
    
*/
    
puntosCasa(Casa,PuntosCasa):-
    casa(Casa),
    findall(Puntos,(esDe(Mago,Casa),queHizo(Mago,Accion),puntosAccion(Accion,Puntos)),ListaPuntos),
    sumlist(ListaPuntos,PuntosCasa).

%todo el choclo del medio es para traer a cada mago de la casa y poder traer cada accion del mago y buscar los puntos de cada
%accion que hizo ese mago. esto lo va a hacer con todos los magos de cada casa.

casaGanadora(Casa):-
    casa(Casa),
    puntosCasa(Casa,PuntosCasa),
    forall((casa(OtraCasa),OtraCasa \= Casa),(puntosCasa(OtraCasa,OtrosPuntos), PuntosCasa > OtrosPuntos)).

