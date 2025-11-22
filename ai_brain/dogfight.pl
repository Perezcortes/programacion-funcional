% ==========================================
%  CEREBRO DE COMBATE (Lógica de Predicados)
% ==========================================

% 1. Función Auxiliar: Distancia Euclidiana
distancia(X1, Y1, X2, Y2, D) :-
    DX is X1 - X2,
    DY is Y1 - Y2,
    D is sqrt(DX*DX + DY*DY).

% 2. Reglas de Comportamiento (La "Inteligencia")

% CASO A: Muy cerca -> ATACAR (Agresivo)
tomar_decision(Dist, 'ATACAR', '¡Te tengo en la mira!') :-
    Dist < 200.

% CASO B: Distancia media -> PERSEGUIR (Táctico)
tomar_decision(Dist, 'PERSEGUIR', 'Adquiriendo objetivo...') :-
    Dist >= 200,
    Dist < 500.

% CASO C: Lejos -> PATRULLAR (Pasivo)
tomar_decision(Dist, 'PATRULLAR', 'Escaneando sector...') :-
    Dist >= 500.

% 3. Bucle Principal (Servidor de IA)
main_loop :-
    % Lee una lista de Python: [Ex, Ey, Px, Py]. (Ojo al punto final)
    read(Input),
    (Input == end_of_file -> true ; 
     procesar(Input), main_loop).

procesar([Ex, Ey, Px, Py]) :-
    distancia(Ex, Ey, Px, Py, Dist),
    % Consultar la base de conocimiento para decidir
    tomar_decision(Dist, Accion, Mensaje),
    % Responder en formato JSON simple
    format('{"action": "~w", "message": "~w"}~n', [Accion, Mensaje]),
    flush_output.

% Punto de entrada inicial
:- main_loop.