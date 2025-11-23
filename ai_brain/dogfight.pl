% ==========================================
%  CEREBRO DE COMBATE AVANZADO
%  Sistema de IA con Memoria y Predicción
% ==========================================

% Base de conocimiento dinámica
:- dynamic historial_posiciones/4.
:- dynamic ultimo_ataque/1.
:- dynamic contador_frames/1.

% Inicialización
:- retractall(historial_posiciones(_, _, _, _)).
:- retractall(ultimo_ataque(_)).
:- retractall(contador_frames(_)).
:- assertz(contador_frames(0)).
:- assertz(ultimo_ataque(0)).

% ==========================================
% UTILIDADES MATEMÁTICAS
% ==========================================

% Distancia Euclidiana
distancia(X1, Y1, X2, Y2, D) :-
    DX is X1 - X2,
    DY is Y1 - Y2,
    D is sqrt(DX*DX + DY*DY).

% Ángulo entre dos puntos (en grados)
angulo_hacia(X1, Y1, X2, Y2, Angulo) :-
    DX is X2 - X1,
    DY is Y2 - Y1,
    Rad is atan2(DY, DX),
    Angulo is Rad * 180 / pi.

% Distancia angular (menor diferencia entre dos ángulos)
diferencia_angular(A1, A2, Diff) :-
    D is abs(A1 - A2),
    (D > 180 -> Diff is 360 - D ; Diff is D).

% ==========================================
% SISTEMA DE MEMORIA
% ==========================================

% Agregar posición al historial
recordar_posicion(Px, Py, Tiempo) :-
    % Guardar nueva posición
    assertz(historial_posiciones(Px, Py, Tiempo, 1)),
    
    % Mantener solo las últimas 15 posiciones
    findall(T, historial_posiciones(_, _, T, _), Tiempos),
    length(Tiempos, N),
    (N > 15 -> 
        min_list(Tiempos, TMin),
        retract(historial_posiciones(_, _, TMin, _))
    ; true).

% Obtener velocidad del jugador basada en historial
calcular_velocidad(VelX, VelY) :-
    findall([X,Y,T], historial_posiciones(X, Y, T, _), Historia),
    length(Historia, N),
    N >= 2,
    
    % Tomar última y penúltima posición
    last(Historia, [X1, Y1, T1]),
    reverse(Historia, [_, [X0, Y0, T0] | _]),
    
    DT is max(1, T1 - T0),
    VelX is (X1 - X0) / DT,
    VelY is (Y1 - Y0) / DT.

calcular_velocidad(0, 0).  % Si no hay suficiente historial

% Predecir posición futura
predecir_posicion(Px, Py, Frames, PredX, PredY) :-
    calcular_velocidad(VelX, VelY),
    PredX is Px + VelX * Frames,
    PredY is Py + VelY * Frames.

% ==========================================
% ANÁLISIS TÁCTICO
% ==========================================

% Determinar si el jugador está huyendo
esta_huyendo(Ex, Ey, Px, Py) :-
    findall([X,Y,T], historial_posiciones(X, Y, T, _), Historia),
    length(Historia, N),
    N >= 3,
    
    % Comparar distancias: si aumentan, está huyendo
    last(Historia, [X1, Y1, _]),
    reverse(Historia, [_, _, [X0, Y0, _] | _]),
    
    distancia(Ex, Ey, X0, Y0, D0),
    distancia(Ex, Ey, X1, Y1, D1),
    
    D1 > D0.  % La distancia está aumentando

% Verificar si tenemos línea de tiro clara
linea_de_tiro_clara(Ex, Ey, Px, Py, AnguloEnemigo) :-
    angulo_hacia(Ex, Ey, Px, Py, AnguloObjetivo),
    diferencia_angular(AnguloEnemigo, AnguloObjetivo, Diff),
    Diff < 20.  % Margen de 20 grados

% ==========================================
% SISTEMA DE DECISIÓN MEJORADO
% ==========================================

% MÁXIMA PRIORIDAD: Ataque con predicción
tomar_decision_avanzada(Ex, Ey, Px, Py, Tiempo, 'INTERCEPTAR', Msg) :-
    % Predecir posición futura
    predecir_posicion(Px, Py, 10, PredX, PredY),
    
    % Calcular distancia a posición predicha
    distancia(Ex, Ey, PredX, PredY, DistPred),
    
    % Si la predicción está cerca, interceptar
    DistPred < 180,
    
    % Mensaje informativo
    format(atom(Msg), '¡Cortando ruta de escape! [Pred: ~1f]', [DistPred]).

% ALTA PRIORIDAD: Ataque directo con línea de tiro
tomar_decision_avanzada(Ex, Ey, Px, Py, Tiempo, 'ATACAR', '¡FUEGO ABIERTO!') :-
    distancia(Ex, Ey, Px, Py, Dist),
    Dist < 200,
    
    % Verificar cooldown de ataque (no spamear)
    ultimo_ataque(UltimoT),
    TiempoTranscurrido is Tiempo - UltimoT,
    TiempoTranscurrido > 30,
    
    % Actualizar tiempo de último ataque
    retract(ultimo_ataque(_)),
    assertz(ultimo_ataque(Tiempo)).

% PRIORIDAD MEDIA: Persecución agresiva si huye
tomar_decision_avanzada(Ex, Ey, Px, Py, _Tiempo, 'PERSEGUIR_AGRESIVO', '¡No escaparás!') :-
    esta_huyendo(Ex, Ey, Px, Py),
    distancia(Ex, Ey, Px, Py, Dist),
    Dist < 450.

% PRIORIDAD MEDIA: Flanqueo táctico
tomar_decision_avanzada(Ex, Ey, Px, Py, Tiempo, 'FLANQUEAR', 'Posicionándome...') :-
    distancia(Ex, Ey, Px, Py, Dist),
    Dist >= 200,
    Dist < 400,
    
    % Intentar flanquear cada 100 frames
    ModTiempo is Tiempo mod 100,
    ModTiempo < 50.

% PRIORIDAD BAJA: Persecución estándar
tomar_decision_avanzada(Ex, Ey, Px, Py, _Tiempo, 'PERSEGUIR', 'Acercándome...') :-
    distancia(Ex, Ey, Px, Py, Dist),
    Dist >= 200,
    Dist < 500.

% MÍNIMA PRIORIDAD: Patrulla
tomar_decision_avanzada(_Ex, _Ey, _Px, _Py, _Tiempo, 'PATRULLAR', 'Escaneando sector...') :-
    true.  % Acción por defecto

% ==========================================
% BUCLE PRINCIPAL
% ==========================================

main_loop :-
    read(Input),
    (Input == end_of_file -> 
        true 
    ; 
        procesar_avanzado(Input), 
        main_loop
    ).

% Procesar con el nuevo sistema
procesar_avanzado([Ex, Ey, Px, Py, Tiempo]) :-
    % Actualizar memoria
    recordar_posicion(Px, Py, Tiempo),
    
    % Incrementar contador interno si no se proporciona
    (var(Tiempo) -> 
        contador_frames(T),
        TiempoReal is T + 1,
        retract(contador_frames(_)),
        assertz(contador_frames(TiempoReal))
    ; 
        TiempoReal = Tiempo
    ),
    
    % Tomar decisión con el sistema mejorado
    tomar_decision_avanzada(Ex, Ey, Px, Py, TiempoReal, Accion, Mensaje),
    
    % Debug: calcular velocidad
    calcular_velocidad(VelX, VelY),
    Velocidad is sqrt(VelX*VelX + VelY*VelY),
    
    % Responder en formato JSON extendido
    format('{"action": "~w", "message": "~w", "playerSpeed": ~2f}~n', 
           [Accion, Mensaje, Velocidad]),
    flush_output.

% Versión con 4 parámetros (retrocompatibilidad)
procesar_avanzado([Ex, Ey, Px, Py]) :-
    contador_frames(Tiempo),
    procesar_avanzado([Ex, Ey, Px, Py, Tiempo]).

% ==========================================
% DEBUGGING Y UTILIDADES
% ==========================================

% Limpiar toda la memoria (útil para reset)
limpiar_memoria :-
    retractall(historial_posiciones(_, _, _, _)),
    retractall(ultimo_ataque(_)),
    retract(contador_frames(_)),
    assertz(contador_frames(0)),
    assertz(ultimo_ataque(0)).

% Mostrar estado de la memoria (para debugging)
mostrar_memoria :-
    findall([X,Y,T], historial_posiciones(X, Y, T, _), Historia),
    format('Historial (~w posiciones):~n', [length(Historia)]),
    maplist(writeln, Historia).

% ==========================================
% PUNTO DE ENTRADA
% ==========================================

:- initialization(main_loop, main).