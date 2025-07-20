:- discontiguous alumno/2.
:- discontiguous aprobo/2.
:- discontiguous mostrar_ciclo/1.

% Ciclo 1
curso(metodos_de_estudio, 1, 2).
curso(desarrollo_personal, 1, 2).
curso(calculo1, 1, 4).
curso(biologia, 1, 4).
curso(medio_ambiente, 1, 3).
curso(redaccion1, 1, 2).
curso(algebra, 1, 4).

% Ciclo 2
curso(redaccion2, 2, 3).
curso(investigacion_formativa, 2, 3).
curso(realidad_nacional, 2, 2).
curso(calculo2, 2, 4).
curso(fisica, 2, 4).
curso(quimica, 2, 4).
curso(introduccion_ingenieria, 2, 2).

% Ciclo 3
curso(programacion1, 3, 4).
curso(teoria_sistemas, 3, 3).
curso(organizacion, 3, 3).
curso(ingenieria_economica, 3, 3).
curso(estadistica, 3, 3).
curso(ecuaciones_diferenciales, 3, 3).
curso(matematica_discreta, 3, 3).

% Ciclo 4
curso(programacion2, 4, 4).
curso(marketing, 4, 2).
curso(contabilidad, 4, 3).
curso(procesos_negocio, 4, 3).
curso(metodos_numericos, 4, 3).
curso(probabilidad, 4, 3).
curso(fisica_electronica, 4, 4).

% Ciclo 2
prerequisito(redaccion2, redaccion1).
prerequisito(calculo2, calculo1).

% Ciclo 3
prerequisito(teoria_sistemas, introduccion_ingenieria).
prerequisito(ecuaciones_diferenciales, calculo2).
prerequisito(matematica_discreta, calculo2).

% Ciclo 4
prerequisito(programacion2, programacion1).
prerequisito(marketing, organizacion).
prerequisito(marketing, desarrollo_personal).
prerequisito(contabilidad, organizacion).
prerequisito(procesos_negocio, organizacion).
prerequisito(metodos_numericos, estadistica).
prerequisito(metodos_numericos, ecuaciones_diferenciales).
prerequisito(probabilidad, estadistica).
prerequisito(fisica_electronica, ecuaciones_diferenciales).

%Alumnos
alumno(carlos, 20).

%Cursos de ciclo1
aprobo(carlos, metodos_de_estudio).
aprobo(carlos, desarrollo_personal).
%aprobo(carlos, calculo1).
aprobo(carlos, biologia).
aprobo(carlos, medio_ambiente).
aprobo(carlos, algebra).
aprobo(carlos, redaccion1).

%Cursos de ciclo2
aprobo(carlos, introduccion_ingenieria).
aprobo(carlos, redaccion2).
aprobo(carlos, investigacion_formativa).
aprobo(carlos, realidad_nacional).
%aprobo(carlos, calculo2).
aprobo(carlos, fisica).
aprobo(carlos, quimica).

%Cursos de ciclo 3
aprobo(carlos, programacion1).
aprobo(carlos, teoria_sistemas).
aprobo(carlos, organizacion).
aprobo(carlos, ingenieria_economica).
aprobo(carlos, estadistica).
%aprobo(carlos, ecuaciones_diferenciales).
%aprobo(carlos, matematica_discreta).


alumno(ana, 18).
%Cursos de ciclo1
aprobo(ana, metodos_de_estudio).
aprobo(ana, desarrollo_personal).
%aprobo(ana, calculo1).
%aprobo(ana, biologia).
aprobo(ana, medio_ambiente).
aprobo(ana, algebra).
aprobo(ana, redaccion1).


alumno(juan, 21).
%Cursos de ciclo1
aprobo(juan, metodos_de_estudio).
aprobo(juan, desarrollo_personal).
%aprobo(juan, calculo1).
aprobo(juan, biologia).
aprobo(juan, medio_ambiente).
aprobo(juan, algebra).
aprobo(juan, redaccion1).

%Cursos de ciclo2
%aprobo(juan, introduccion_ingenieria).
%aprobo(juan, redaccion2).
%aprobo(juan, investigacion_formativa).
aprobo(juan, realidad_nacional).
%aprobo(juan, calculo2).
aprobo(juan, fisica).
aprobo(juan, quimica).

alumno(luis, 20).
%Cursos de ciclo1
aprobo(luis, metodos_de_estudio).
aprobo(luis, desarrollo_personal).
aprobo(luis, calculo1).
aprobo(luis, biologia).
aprobo(luis, medio_ambiente).
%aprobo(luis, algebra).
aprobo(luis, redaccion1).

%Cursos de ciclo2
aprobo(luis, introduccion_ingenieria).
aprobo(luis, redaccion2).
aprobo(luis, investigacion_formativa).
aprobo(luis, realidad_nacional).
aprobo(luis, calculo2).
aprobo(luis, fisica).
aprobo(luis, quimica).

%Cursos de ciclo 3
aprobo(luis, programacion1).
aprobo(luis, teoria_sistemas).
%aprobo(luis, organizacion).
aprobo(luis, ingenieria_economica).
%aprobo(luis, estadistica).
aprobo(luis, ecuaciones_diferenciales).
aprobo(luis, matematica_discreta).

%Cursos del ciclo 4
aprobo(luis, programacion2).
%aprobo(luis, marketing).
%aprobo(luis, contabilidad).
%aprobo(luis, procesos_negocio).
%aprobo(luis, metodos_numericos).
%aprobo(luis, probabilidad).
aprobo(luis, fisica_electronica).

%Consultas

%Cuantos prerequistos tiene un curso
cantidad_prerequisitos(Curso, Cantidad) :-
    findall(Pre, prerequisito(Curso, Pre), Lista),
    length(Lista, Cantidad).

%Cursos con más de un prerequisito
mostrar_cursos_con_multiples_prerequisitos :-
    curso(Curso, _, _),
    cantidad_prerequisitos(Curso, N),
    N > 1,
    write(Curso), nl,
    fail.

%Muestra que curso siguen directamente despues de un curso dado
cursos_despues_de(CursoAprobado, ListaCursos) :-
    findall(CursoDisponible, prerequisito(CursoDisponible, CursoAprobado), ListaCursos).

desbloquea_con_detalle(CursoAprobado, CursoDestino, OtrosRequisitos) :-
    prerequisito(CursoDestino, CursoAprobado),
    findall(Pre,
        (prerequisito(CursoDestino, Pre), Pre \= CursoAprobado),
        OtrosRequisitos).

%Muestra los cursos que se desbloquean directamente con más detalle
mostrar_cursos_desbloqueados(CursoAprobado) :-
    findall((CursoDestino, Otros), 
        desbloquea_con_detalle(CursoAprobado, CursoDestino, Otros),
        Resultados),
    ( Resultados = [] ->
        format('~w no desbloquea ningún curso.~n', [CursoAprobado])
    ;
        format('Si apruebas ~w, se desbloquean:~n', [CursoAprobado]),
        mostrar_lista_detallada(Resultados)
    ).


mostrar_lista_detallada([]).
mostrar_lista_detallada([(Curso, []) | Resto]) :-
    format('- ~w~n', [Curso]),
    mostrar_lista_detallada(Resto).
mostrar_lista_detallada([(Curso, Otros) | Resto]) :-
    format('- ~w (también requiere: ~w)~n', [Curso, Otros]),
    mostrar_lista_detallada(Resto).

mostrar_lista_simple([]).
mostrar_lista_simple([X | XS]) :-
    format('- ~w~n', [X]),
    mostrar_lista_simple(XS).



% Base: un curso es prerequisito directo de otro
es_prerequisito_de(Curso, OtroCurso) :-
    prerequisito(OtroCurso, Curso).

% Recursivo: si Curso es prerequisito de Medio, y Medio de OtroCurso
es_prerequisito_de(Curso, OtroCurso) :-
    prerequisito(Intermedio, Curso),
    es_prerequisito_de(Intermedio, OtroCurso).

ruta_dependientes(CursoBase, ListaFinal) :-
    findall(Curso, es_prerequisito_de(CursoBase, Curso), Lista),
    list_to_set(Lista, ListaFinal).

mostrar_lista_con_ciclo([]).
mostrar_lista_con_ciclo([(Curso, Ciclo, []) | Resto]) :-
    format('- ~w (Ciclo ~w)~n', [Curso, Ciclo]),
    mostrar_lista_con_ciclo(Resto).
mostrar_lista_con_ciclo([(Curso, Ciclo, Otros) | Resto]) :-
    format('- ~w (Ciclo ~w, también requiere: ~w)~n', [Curso, Ciclo, Otros]),
    mostrar_lista_con_ciclo(Resto).

%Muestra el flujo de un curso sin detalles
mostrar_dependientes(CursoBase) :-
    ruta_dependientes(CursoBase, Lista),
    ( Lista = [] ->
        format('~w no es prerrequisito de ningún curso.~n', [CursoBase])
    ;
        format('Cursos que dependen de ~w:~n', [CursoBase]),
        mostrar_lista_simple(Lista)
    ).




% Recorre todos los cursos desbloqueados en cadena con sus otros prerequisitos
cursos_desbloqueados_con_detalle(CursoBase, ResultadosFinales) :-
    cursos_desbloqueados_recursivo([CursoBase], [], [], Resultados),
    list_to_set(Resultados, ResultadosFinales).

% cursos_desbloqueados_recursivo(Pendientes, Visitados, Acumulado, ResultadoFinal)
cursos_desbloqueados_recursivo([], _, Acumulado, Acumulado).
cursos_desbloqueados_recursivo([Actual | Resto], Visitados, Acum, ResultadoFinal) :-
    findall((CursoDestino, Ciclo, Otros),
        (
            desbloquea_con_detalle(Actual, CursoDestino, Otros),
            curso(CursoDestino, Ciclo, _),
            \+ member((CursoDestino, Ciclo, Otros), Acum)  % evitar duplicados
        ),
        Nuevos),
    % Extraer solo nombres de cursos nuevos para seguir explorando
    findall(C,
        member((C, _, _), Nuevos),
        NuevosPendientes),
    append(Resto, NuevosPendientes, SiguienteCola),
    append(Acum, Nuevos, NuevoAcum),
    cursos_desbloqueados_recursivo(SiguienteCola, [Actual | Visitados], NuevoAcum, ResultadoFinal).

% Extrae los nombres de cursos desde tuplas (Curso, Ciclo, Otros)
extraer_cursos_de_tuplas([], []).
extraer_cursos_de_tuplas([(Curso, _, _) | Resto], [Curso | Cursos]) :-
    extraer_cursos_de_tuplas(Resto, Cursos).

%Mustra el flujo de un curso detalladamente
mostrar_dependientes_detallado(CursoBase) :-
    cursos_desbloqueados_con_detalle(CursoBase, Resultados),
    ( Resultados = [] ->
        format('~w no desbloquea ningún curso.~n', [CursoBase])
    ;
        format('Cursos que dependen de ~w (directa o indirectamente):~n', [CursoBase]),
        mostrar_lista_con_ciclo(Resultados),
    	extraer_cursos_de_tuplas(Resultados, SoloCursos),
        calcular_creditos(SoloCursos, Total),
        format('Créditos que se habilitan tras aprobar ~w: ~w~n', [CursoBase, Total])
    ).

%RUTA INVERSA

ruta_requisitos(CursoFinal, ListaFinal) :-
    findall(CursoReq, es_prerequisito_de(CursoReq, CursoFinal), Lista),
    list_to_set(Lista, ListaFinal).  % eliminamos duplicados

mostrar_lista_cursos_ciclo([]).
mostrar_lista_cursos_ciclo([Curso | Resto]) :-
    curso(Curso, Ciclo, _),
    format('- ~w (Ciclo ~w)~n', [Curso, Ciclo]),
    mostrar_lista_cursos_ciclo(Resto).

% Calcula los créditos totales de una lista de cursos
calcular_creditos([], 0).
calcular_creditos([Curso | Resto], Total) :-
    curso(Curso, _, Cred),
    calcular_creditos(Resto, Subtotal),
    Total is Cred + Subtotal.

% Muestra los prerequisitos en detalle + créditos totales
mostrar_requisitos_detallado(CursoFinal) :-
    ruta_requisitos(CursoFinal, Requisitos),
    ( Requisitos = [] ->
        format('~w no requiere cursos previos.~n', [CursoFinal])
    ;
        format('Para llevar ~w necesitas haber aprobado:~n', [CursoFinal]),
        mostrar_lista_cursos_ciclo(Requisitos),
        calcular_creditos(Requisitos, TotalCreditos),
        format('Créditos totales requeridos antes de ~w: ~w~n', [CursoFinal, TotalCreditos])
    ).



%CONSULTAS DE ALUMNOS

% Obtener info de cursos aprobados: nombre, ciclo, créditos
curso_aprobado_info(Alumno, Curso, Ciclo, Creditos) :-
    aprobo(Alumno, Curso),
    curso(Curso, Ciclo, Creditos).

% Mostrar los cursos aprobados divididos por ciclo, con créditos
mostrar_cursos_aprobados_detallado(Alumno) :-
    findall((Curso, Ciclo, Cred),
        curso_aprobado_info(Alumno, Curso, Ciclo, Cred),
        ListaConCiclos),
    ListaConCiclos \= [],
    sort(2, @=<, ListaConCiclos, Ordenados),  % ordenado por ciclo
    agrupar_por_ciclo(Ordenados),
    sumar_creditos_aprobados(ListaConCiclos, Total),
    format('~nTotal de créditos aprobados: ~w~n', [Total]).

mostrar_cursos_aprobados_detallado(Alumno) :-
    format('~w no ha aprobado ningún curso.~n', [Alumno]).

agrupar_por_ciclo([]).
agrupar_por_ciclo([(Curso, Ciclo, Cred) | Resto]) :-
    mostrar_ciclo(Ciclo),
    format('- ~w (~w créditos)~n', [Curso, Cred]),
    agrupar_por_ciclo_aux(Ciclo, Resto).

agrupar_por_ciclo_aux(_, []).
agrupar_por_ciclo_aux(Ciclo, [(Curso, Ciclo, Cred) | Resto]) :-
    format('- ~w (~w créditos)~n', [Curso, Cred]),
    agrupar_por_ciclo_aux(Ciclo, Resto).
agrupar_por_ciclo_aux(_, Resto) :-
    agrupar_por_ciclo(Resto).

mostrar_ciclo(Ciclo) :-
    format('~n--- Ciclo ~w ---~n', [Ciclo]).
sumar_creditos_aprobados(Lista, Total) :-
    findall(Cred, member((_, _, Cred), Lista), Creditos),
    sumlist(Creditos, Total).


% Regla que obtiene cursos que el alumno NO ha aprobado
curso_no_aprobado_info(Alumno, Curso, Ciclo, Creditos) :-
    curso(Curso, Ciclo, Creditos),
    \+ aprobo(Alumno, Curso).

% Mostrar cursos pendientes con detalle
mostrar_cursos_pendientes_detallado(Alumno) :-
    findall((Curso, Ciclo, Cred),
        curso_no_aprobado_info(Alumno, Curso, Ciclo, Cred),
        ListaFaltantes),
    ListaFaltantes \= [],
    sort(2, @=<, ListaFaltantes, Ordenados),  % ordenamos por ciclo
    agrupar_por_ciclo(Ordenados),
    sumar_creditos_aprobados(Ordenados, Total),
    format('~nTotal de créditos pendientes: ~w~n', [Total]).

mostrar_cursos_pendientes_detallado(Alumno) :-
    format('~w ya aprobó todos los cursos registrados.~n', [Alumno]).

curso_no_aprobado_con_requisitos(Alumno, Curso, Ciclo, Prereqs) :-
    curso(Curso, Ciclo, _),
    \+ aprobo(Alumno, Curso),
    findall(Pre, prerequisito(Curso, Pre), Prereqs).
mostrar_ciclo(Ciclo) :-
    format('~n--- Ciclo ~w ---~n', [Ciclo]).

agrupar_y_mostrar_con_requisitos([]).
agrupar_y_mostrar_con_requisitos([(Curso, Ciclo, []) | Resto]) :-
    mostrar_ciclo(Ciclo),
    format('- ~w (no tiene prerrequisitos)~n', [Curso]),
    agrupar_y_mostrar_con_requisitos_aux(Ciclo, Resto).

agrupar_y_mostrar_con_requisitos([(Curso, Ciclo, Prereqs) | Resto]) :-
    mostrar_ciclo(Ciclo),
    format('- ~w (prerrequisitos: ~w)~n', [Curso, Prereqs]),
    agrupar_y_mostrar_con_requisitos_aux(Ciclo, Resto).

agrupar_y_mostrar_con_requisitos_aux(_, []).
agrupar_y_mostrar_con_requisitos_aux(Ciclo, [(Curso, Ciclo, []) | Resto]) :-
    format('- ~w (no tiene prerrequisitos)~n', [Curso]),
    agrupar_y_mostrar_con_requisitos_aux(Ciclo, Resto).
agrupar_y_mostrar_con_requisitos_aux(Ciclo, [(Curso, Ciclo, Prereqs) | Resto]) :-
    format('- ~w (prerrequisitos: ~w)~n', [Curso, Prereqs]),
    agrupar_y_mostrar_con_requisitos_aux(Ciclo, Resto).
agrupar_y_mostrar_con_requisitos_aux(_, Resto) :-
    agrupar_y_mostrar_con_requisitos(Resto).


%Mostrar cursos con sus prerequistos
mostrar_cursos_pendientes_con_requisitos(Alumno) :-
    findall((Curso, Ciclo, Prereqs),
        curso_no_aprobado_con_requisitos(Alumno, Curso, Ciclo, Prereqs),
        ListaFaltantes),
    ListaFaltantes \= [],
    sort(2, @=<, ListaFaltantes, Ordenados),
    agrupar_y_mostrar_con_requisitos(Ordenados).

mostrar_cursos_pendientes_con_requisitos(Alumno) :-
    format('~w ya aprobó todos los cursos registrados.~n', [Alumno]).


% Cuenta cuántos alumnos aprobaron un curso
cantidad_aprobados(Curso, Cantidad) :-
    findall(Alumno, aprobo(Alumno, Curso), Lista),
    length(Lista, Cantidad).

% Cuenta el total de alumnos registrados
total_alumnos(Total) :-
    findall(A, alumno(A, _), Lista),
    length(Lista, Total).

%Mustra el porcentaje de alumnos que aprobaron un curso
mostrar_aprobados(Curso) :-
    total_alumnos(Total),
    findall(Alumno, aprobo(Alumno, Curso), Aprobados),
    length(Aprobados, Cantidad),
    (Total > 0 -> Porcentaje is (Cantidad * 100) / Total ; Porcentaje = 0),
    format('Curso: ~w~n', [Curso]),
    format('Alumnos que lo aprobaron: ~w de ~w (~2f%%)~n', [Cantidad, Total, Porcentaje]),
    ( Aprobados == [] ->
        writeln('Ningún alumno ha aprobado este curso.')
    ;
        writeln('Lista de alumnos que lo aprobaron:'),
        mostrar_lista_simple(Aprobados)
    ).

curso_con_porcentaje(Curso, Porcentaje, Aprobados) :-
    curso(Curso, _, _),
    total_alumnos(Total),
    findall(Alumno, aprobo(Alumno, Curso), Aprobados),
    length(Aprobados, Cantidad),
    (Total > 0 -> Porcentaje is (Cantidad * 100) / Total ; Porcentaje = 0).

mostrar_cursos_menor_aprobacion :-
    findall((Curso, Porcentaje, Aprobados),
            curso_con_porcentaje(Curso, Porcentaje, Aprobados),
            Lista),
    findall(P, member((_, P, _), Lista), Porcentajes),
    min_list(Porcentajes, Min),
    include(con_porcentaje(Min), Lista, Filtrados),
    format('Cursos con menor aprobación (~2f%%):~n', [Min]),
    mostrar_lista_cursos_y_alumnos(Filtrados).

con_porcentaje(Porcentaje, (_, Porcentaje, _)).


mostrar_cursos_mayor_aprobacion :-
    findall((Curso, Porcentaje, Aprobados),
            curso_con_porcentaje(Curso, Porcentaje, Aprobados),
            Lista),
    findall(P, member((_, P, _), Lista), Porcentajes),
    max_list(Porcentajes, Max),
    include(con_porcentaje(Max), Lista, Filtrados),
    format('Cursos con mayor aprobación (~2f%%):~n', [Max]),
    mostrar_lista_cursos_y_alumnos(Filtrados).


mostrar_lista_cursos_y_alumnos([]).
mostrar_lista_cursos_y_alumnos([(Curso, _, []) | Resto]) :-
    format('- ~w: Ningún alumno lo aprobó.~n', [Curso]),
    mostrar_lista_cursos_y_alumnos(Resto).

mostrar_lista_cursos_y_alumnos([(Curso, _, Aprobados) | Resto]) :-
    format('- ~w: Aprobado por: ~w~n', [Curso, Aprobados]),
    mostrar_lista_cursos_y_alumnos(Resto).

ha_aprobado(Curso, Alumno) :-
    aprobo(Alumno, Curso).


alumno_mayor_de(Nombre, EdadMinima) :-
    alumno(Nombre, Edad),
    Edad > EdadMinima.

porcentaje_aprobados_mayores(Curso, EdadMinima, Porcentaje, Aprobadores) :-
    findall(Nombre, alumno_mayor_de(Nombre, EdadMinima), Mayores),
    include(ha_aprobado(Curso), Mayores, Aprobadores),
    length(Mayores, TotalMayores),
    length(Aprobadores, TotalAprobadores),
    ( TotalMayores =:= 0 -> Porcentaje = 0
    ; Porcentaje is (TotalAprobadores / TotalMayores) * 100
    ).


mostrar_porcentaje_aprobados_mayores(Curso, EdadMinima) :-
    porcentaje_aprobados_mayores(Curso, EdadMinima, Porcentaje, Aprobadores),
    format('~2f%% de los alumnos mayores de ~w años aprobaron el curso "~w".~n', 
           [Porcentaje, EdadMinima, Curso]),
    ( Aprobadores = [] ->
        write('Ninguno lo aprobó.'), nl
    ;
        write('Alumnos que lo aprobaron: '), write(Aprobadores), nl
    ).











