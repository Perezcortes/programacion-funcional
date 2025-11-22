import pygame
import sys
import subprocess
import json
import os
import math

# --- Configuración Inicial ---
pygame.init()
pygame.joystick.init()
pygame.font.init()

# Audio (Intentar iniciar, no crashear si falla)
try:
    # Frecuencia estándar, tamaño de sample, estéreo, buffer pequeño para latencia baja
    pygame.mixer.init(44100, -16, 2, 512)
except pygame.error:
    print("Advertencia: No se pudo iniciar el audio.")

WIDTH, HEIGHT = 800, 600
screen = pygame.display.set_mode((WIDTH, HEIGHT))
pygame.display.set_caption("WW2 Dogfight: Functional Edition")
clock = pygame.time.Clock()

# Colores
WHITE = (255, 255, 255)
BLACK = (0, 0, 0)
GREEN = (50, 255, 50)
YELLOW = (255, 255, 0)
RED = (255, 50, 50)
DARK_OVERLAY = (0, 0, 0, 180) # Sombra semitransparente para el menú

# Fuentes
font_title = pygame.font.SysFont("Stencil", 50, bold=True)
font_sub = pygame.font.SysFont("Consolas", 20)
font_info = pygame.font.SysFont("Arial", 16)

# --- CARGA DE ASSETS ---
def load_asset(path, type='image'):
    """Carga recursos de forma segura, retorna dummies si fallan."""
    full_path = os.path.join("assets", path)
    if not os.path.exists(full_path):
        print(f"FALTA ASSET: {full_path}")
        if type == 'image': return pygame.Surface((64, 64)).convert_alpha()
        if type == 'sound': return None
    try:
        if type == 'image': return pygame.image.load(full_path).convert_alpha()
        elif type == 'sound': return pygame.mixer.Sound(full_path)
    except Exception as e:
        print(f"Error cargando {full_path}: {e}")
        return None

# Fondo
bg_raw = load_asset(os.path.join("sprites", "background.jpg"), 'image')
bg_image = pygame.transform.scale(bg_raw, (WIDTH, HEIGHT))
bg_y = 0

# --- SPRITES (CORRECCIÓN DE ORIENTACIÓN) ---
# Cargar imágenes crudas
player_raw = load_asset(os.path.join("sprites", "player.png"), 'image')
enemy_raw = load_asset(os.path.join("sprites", "enemy.png"), 'image')

# Escalar a 64x64
player_scaled = pygame.transform.scale(player_raw, (64, 64))
enemy_scaled = pygame.transform.scale(enemy_raw, (64, 64))

# CORRECCIÓN DE ÁNGULO: Las imágenes originales miran hacia ARRIBA.
# La matemática asume que 0 grados es a la DERECHA.
# Rotamos -90 grados para que el jugador mire a la derecha.
player_sprite = pygame.transform.rotate(player_scaled, -90)
# Rotamos +90 grados para que el enemigo mire a la izquierda (hacia el jugador).
enemy_sprite = pygame.transform.rotate(enemy_scaled, 90)

# --- SONIDOS (CORRECCIÓN .WAV) ---
# Se cambió .ogg por .wav según tu indicación
snd_engine = load_asset(os.path.join("sounds", "engine_loop.wav"), 'sound')
if snd_engine: snd_engine.set_volume(0.3)
snd_alert = load_asset(os.path.join("sounds", "alert.wav"), 'sound')

# --- MOTORES EXTERNOS ---
haskell_exe = os.path.join("physics_engine", "movement_binary.exe")
if not os.path.exists(haskell_exe): sys.exit("Error: Falta binario Haskell")

# Iniciar subprocesos
proc_hs = subprocess.Popen([haskell_exe], stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True, bufsize=0)
prolog_file = os.path.join("ai_brain", "dogfight.pl")
proc_pl = subprocess.Popen(["swipl", "-q", "-g", "main_loop", "-t", "halt", prolog_file], stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True, bufsize=0)

# --- FUNCIONES ---
def draw_text_centered(surface, text, font, color, y_offset=0):
    text_obj = font.render(text, True, color)
    rect = text_obj.get_rect(center=(WIDTH//2, HEIGHT//2 + y_offset))
    surface.blit(text_obj, rect)

def draw_sprite_rotated(surface, sprite, x, y, angle):
    """Dibuja un sprite rotado. Pygame rota en sentido anti-horario."""
    rotated = pygame.transform.rotate(sprite, -angle)
    rect = rotated.get_rect(center=(x, y))
    surface.blit(rotated, rect.topleft)

def ask_haskell(state, rot, thrust):
    try:
        proc_hs.stdin.write(f"{state['x']} {state['y']} {state['angle']} {state['speed']} {rot} {thrust}\n")
        return json.loads(proc_hs.stdout.readline())
    except: return state

def ask_prolog(ex, ey, px, py):
    try:
        proc_pl.stdin.write(f"[{ex}, {ey}, {px}, {py}].\n")
        line = proc_pl.stdout.readline()
        return json.loads(line) if line else {"action": "WAIT", "message": "..."}
    except: return {"action": "ERR", "message": "I/O Error"}

# --- VARIABLES DE ESTADO ---
game_state = "MENU" # MENU o PLAYING
player = {"x": 200.0, "y": 300.0, "angle": 0.0, "speed": 0.0}
# El enemigo empieza mirando a la izquierda (180 grados)
enemy =  {"x": 600.0, "y": 300.0, "action": "PATRULLAR", "msg": "Waiting...", "angle": 180.0}
last_enemy_action = "PATRULLAR"
blink_timer = 0

# --- BUCLE PRINCIPAL ---
running = True
joystick = None

while running:
    # Detección dinámica de joystick
    if pygame.joystick.get_count() > 0 and joystick is None:
        joystick = pygame.joystick.Joystick(0)
        joystick.init()
    elif pygame.joystick.get_count() == 0:
        joystick = None

    # Eventos
    for event in pygame.event.get():
        if event.type == pygame.QUIT: running = False
        
        # TRANSICIÓN DE MENÚ A JUEGO
        if game_state == "MENU":
            start_game = False
            if event.type == pygame.KEYDOWN:
                if event.key == pygame.K_SPACE or event.key == pygame.K_RETURN: start_game = True
            if event.type == pygame.JOYBUTTONDOWN: start_game = True
            
            if start_game:
                game_state = "PLAYING"
                if snd_engine: snd_engine.play(-1) # Arrancar motor al empezar

    # RENDERIZADO DE FONDO (Común para ambos estados)
    screen.blit(bg_image, (0, 0))

    # ==========================================
    # ESTADO: MENÚ
    # ==========================================
    if game_state == "MENU":
        overlay = pygame.Surface((WIDTH, HEIGHT), pygame.SRCALPHA)
        overlay.fill(DARK_OVERLAY)
        screen.blit(overlay, (0,0))

        draw_text_centered(screen, "WW2 FUNCTIONAL DOGFIGHT", font_title, WHITE, -150)
        draw_text_centered(screen, "EXPERIENCIA RECOMENDADA CON AUDÍFONOS", font_sub, (100, 200, 255), -80)

        blink_timer += 1
        color_pulse = GREEN if (blink_timer // 30) % 2 == 0 else (100, 255, 100)
        
        if joystick:
            name = joystick.get_name()[:20]
            pygame.draw.rect(screen, GREEN, (WIDTH//2 - 150, HEIGHT//2 - 20, 300, 40), 2, border_radius=10)
            draw_text_centered(screen, f"JOYSTICK DETECTADO: {name}", font_info, color_pulse, 0)
            draw_text_centered(screen, "Presiona [CUALQUIER BOTÓN] para Despegar", font_sub, WHITE, 40)
        else:
            pygame.draw.rect(screen, YELLOW, (WIDTH//2 - 150, HEIGHT//2 - 20, 300, 40), 2, border_radius=10)
            draw_text_centered(screen, "JOYSTICK NO ENCONTRADO", font_info, YELLOW, 0)
            draw_text_centered(screen, "Modo Teclado: Usa Flechas", font_info, WHITE, 40)
            draw_text_centered(screen, "Presiona [ESPACIO] para Despegar", font_sub, WHITE, 80)

    # ==========================================
    # ESTADO: JUEGO (PLAYING)
    # ==========================================
    elif game_state == "PLAYING":
        # 1. Inputs
        rot, thrust = 0.0, 0.0
        if joystick:
            # Ajusta los ejes si tu joystick responde raro (ej. usa eje 2 o 3)
            rot = joystick.get_axis(0)
            thrust = -joystick.get_axis(1)
        else:
            keys = pygame.key.get_pressed()
            if keys[pygame.K_LEFT]:  rot = -1.0
            if keys[pygame.K_RIGHT]: rot = 1.0
            if keys[pygame.K_UP]:    thrust = 1.0
            if keys[pygame.K_DOWN]:  thrust = -0.5

        # 2. Comunicación con Motores
        player = ask_haskell(player, rot, thrust)
        decision = ask_prolog(enemy['x'], enemy['y'], player['x'], player['y'])
        enemy['action'] = decision['action']
        enemy['msg'] = decision['message']

        # Sonido Alarma
        if enemy['action'] == 'ATACAR' and last_enemy_action != 'ATACAR':
            if snd_alert: snd_alert.play()
        last_enemy_action = enemy['action']

        # Lógica Enemigo (Python)
        target_angle = math.atan2(player['y'] - enemy['y'], player['x'] - enemy['x'])
        enemy['angle'] = math.degrees(target_angle)
        spd_mult = 4.5 if enemy['action'] == 'ATACAR' else 2.0
        enemy['x'] += math.cos(target_angle) * spd_mult
        enemy['y'] += math.sin(target_angle) * spd_mult

        # 3. Renderizado Juego
        # Fondo Scrolling
        bg_y += 3 + (player['speed'] * 0.4)
        if bg_y >= HEIGHT: bg_y = 0
        screen.blit(bg_image, (0, bg_y))
        screen.blit(bg_image, (0, bg_y - HEIGHT))

        # Sprites Rotados
        draw_sprite_rotated(screen, player_sprite, player['x'], player['y'], player['angle'])
        draw_sprite_rotated(screen, enemy_sprite, enemy['x'], enemy['y'], enemy['angle'])

        # HUD
        hud_bar = pygame.Surface((WIDTH, 80), pygame.SRCALPHA)
        hud_bar.fill((0, 0, 0, 150))
        screen.blit(hud_bar, (0, HEIGHT-80))

        col_ai = RED if enemy['action'] == 'ATACAR' else GREEN
        screen.blit(font_sub.render(f"HASKELL FÍSICA | V:{player['speed']:.1f} | A:{int(player['angle'])%360}°", True, GREEN), (20, 20))
        screen.blit(font_title.render(f"IA: {enemy['action']}", True, col_ai), (20, HEIGHT-75))
        screen.blit(font_sub.render(f"MSG: {enemy['msg']}", True, WHITE), (20, HEIGHT-35))

    pygame.display.flip()
    clock.tick(60)

# Limpieza final
if snd_engine: snd_engine.stop()
pygame.mixer.quit()
proc_hs.terminate()
proc_pl.terminate()
pygame.quit()