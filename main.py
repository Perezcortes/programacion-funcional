import pygame
import sys
import subprocess
import json
import os
import math

# --- Configuración Inicial ---
pygame.init()
pygame.joystick.init()

WIDTH, HEIGHT = 800, 600
screen = pygame.display.set_mode((WIDTH, HEIGHT))
pygame.display.set_caption("WW2 Dogfight: Python + Haskell + Prolog")
clock = pygame.time.Clock()

# Colores y Fuentes
DARK_GREEN = (30, 40, 30)
WHITE = (200, 200, 200)
RED = (200, 50, 50)     # Enemigo
BLUE = (50, 100, 200)   # Jugador
HUD_COLOR = (50, 200, 50)
font_hud = pygame.font.SysFont("Consolas", 16)

# --- 1. CONEXIÓN CON HASKELL (Física) ---
haskell_exe = os.path.join("physics_engine", "movement_binary.exe")
if not os.path.exists(haskell_exe):
    print("Error: No se encuentra el binario de Haskell.")
    sys.exit(1)

proc_hs = subprocess.Popen(
    [haskell_exe],
    stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
    text=True, bufsize=0
)

# --- 2. CONEXIÓN CON PROLOG (Inteligencia) ---
prolog_file = os.path.join("ai_brain", "dogfight.pl")
# Invocamos swipl silenciando el banner de bienvenida (-q) y cargando el script
proc_pl = subprocess.Popen(
    ["swipl", "-q", "-g", "main_loop", "-t", "halt", prolog_file],
    stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
    text=True, bufsize=0
)
print("Sistemas Funcionales y Lógicos en línea.")

# --- Funciones de Comunicación ---
def ask_haskell(state, rot, thrust):
    msg = f"{state['x']} {state['y']} {state['angle']} {state['speed']} {rot} {thrust}\n"
    proc_hs.stdin.write(msg)
    return json.loads(proc_hs.stdout.readline())

def ask_prolog(ex, ey, px, py):
    # Prolog espera una lista terminada en punto: [ex, ey, px, py].
    msg = f"[{ex}, {ey}, {px}, {py}].\n"
    proc_pl.stdin.write(msg)
    try:
        line = proc_pl.stdout.readline()
        if not line: return {"action": "ESPERA", "message": "Prolog offline"}
        return json.loads(line)
    except json.JSONDecodeError:
        return {"action": "ERROR", "message": "Fallo mental"}

# --- Entidades ---
player = {"x": 200.0, "y": 300.0, "angle": 0.0, "speed": 0.0}
enemy =  {"x": 600.0, "y": 300.0, "action": "PATRULLAR", "msg": "Iniciando..."}

def draw_plane(surface, x, y, angle, color):
    # Dibuja un triángulo orientado
    img = pygame.Surface((40, 40), pygame.SRCALPHA)
    pygame.draw.polygon(img, color, [(40, 20), (0, 0), (0, 40)])
    rotated = pygame.transform.rotate(img, -angle)
    rect = rotated.get_rect(center=(x, y))
    surface.blit(rotated, rect.topleft)

# --- Bucle Principal ---
running = True
joystick = pygame.joystick.Joystick(0) if pygame.joystick.get_count() > 0 else None
if joystick: joystick.init()

while running:
    # A. Eventos
    for event in pygame.event.get():
        if event.type == pygame.QUIT: running = False

    # B. Input Jugador
    rot, thrust = 0.0, 0.0
    if joystick:
        rot = joystick.get_axis(0)
        thrust = -joystick.get_axis(1)
    else:
        keys = pygame.key.get_pressed()
        if keys[pygame.K_LEFT]:  rot = -1.0
        if keys[pygame.K_RIGHT]: rot = 1.0
        if keys[pygame.K_UP]:    thrust = 1.0
        if keys[pygame.K_DOWN]:  thrust = -0.5

    # C. PROCESO 1: HASKELL (Física del Jugador)
    player = ask_haskell(player, rot, thrust)

    # D. PROCESO 2: PROLOG (Cerebro del Enemigo)
    # Preguntamos a Prolog qué hacer basándonos en posiciones
    decision = ask_prolog(enemy['x'], enemy['y'], player['x'], player['y'])
    enemy['action'] = decision['action']
    enemy['msg'] = decision['message']

    # E. Lógica simple del enemigo en Python (Obedeciendo a Prolog)
    # Python solo ejecuta, Prolog decide "QUÉ" hacer.
    target_angle = math.atan2(player['y'] - enemy['y'], player['x'] - enemy['x'])
    enemy_speed = 2.0
    
    if enemy['action'] == 'ATACAR':
        enemy_speed = 4.0 # Más rápido
        # Moverse hacia el jugador
        enemy['x'] += math.cos(target_angle) * enemy_speed
        enemy['y'] += math.sin(target_angle) * enemy_speed
    
    elif enemy['action'] == 'PERSEGUIR':
        enemy_speed = 2.5
        # Moverse lento
        enemy['x'] += math.cos(target_angle) * enemy_speed
        enemy['y'] += math.sin(target_angle) * enemy_speed
        
    elif enemy['action'] == 'PATRULLAR':
        # Movimiento simple en círculos o quieto
        enemy['x'] += 0.5 
        if enemy['x'] > 800: enemy['x'] = 0

    # F. Renderizado
    screen.fill(DARK_GREEN)
    
    # Dibujar Jugador (Azul)
    draw_plane(screen, player['x'], player['y'], player['angle'], BLUE)
    
    # Dibujar Enemigo (Rojo - cambia a Naranja si ataca)
    enemy_color = RED if enemy['action'] == 'ATACAR' else (200, 150, 50)
    draw_plane(screen, enemy['x'], enemy['y'], 0, enemy_color) # Ángulo 0 simplificado para enemigo

    # HUD
    hud_text = [
        f"HASKELL: Pos({int(player['x'])},{int(player['y'])}) Vel({player['speed']:.1f})",
        f"PROLOG : Estado[{enemy['action']}]",
        f"RADIO  : \"{enemy['msg']}\""
    ]
    
    for i, line in enumerate(hud_text):
        screen.blit(font_hud.render(line, True, WHITE), (10, 530 + i*20))

    pygame.display.flip()
    clock.tick(60)

proc_hs.terminate()
proc_pl.terminate()
pygame.quit()