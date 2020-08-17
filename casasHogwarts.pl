casa(gryffindor).
casa(hufflepuff).
casa(slytherin).
casa(ravenclaw).

mago(harry,sangre(mestiza)).
mago(draco,sangre(pura)).
mago(hermione,sangre(impura)).
mago(neville,sangre(pura)).

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
