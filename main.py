import pygame
import sys
import subprocess
import json
import os
import math

# --- Configuración Inicial ---
pygame.init()
pygame.joystick.init()
try:
    pygame.mixer.init(44100, -16, 2, 512)
except pygame.error:
    print("Advertencia: No se pudo iniciar el audio.")

WIDTH, HEIGHT = 800, 600
screen = pygame.display.set_mode((WIDTH, HEIGHT))
pygame.display.set_caption("WW2 Dogfight: Final Functional Version")
clock = pygame.time.Clock()

# Colores y Fuentes
WHITE = (220, 220, 220)
HUD_COLOR = (50, 255, 50)
ALERT_COLOR = (255, 50, 50)
font_hud = pygame.font.SysFont("Consolas", 16, bold=True)
font_big = pygame.font.SysFont("Stencil", 24)

# --- CARGA DE ASSETS ---
def load_asset(path, type='image'):
    """Ayuda a cargar recursos de forma segura."""
    full_path = os.path.join("assets", path)
    if not os.path.exists(full_path):
        print(f"FALTA ASSET: {full_path}")
        if type == 'image': return pygame.Surface((64, 64)).convert_alpha()
        if type == 'sound': return None
    
    try:
        if type == 'image':
            img = pygame.image.load(full_path).convert_alpha()
            return img 
        elif type == 'sound':
            return pygame.mixer.Sound(full_path)
    except Exception as e:
        print(f"Error cargando {full_path}: {e}")
        return None

# --- FONDO SCROLLING ---
bg_raw = load_asset(os.path.join("sprites", "background.jpg"), 'image')
bg_image = pygame.transform.scale(bg_raw, (WIDTH, HEIGHT))
bg_y = 0
scroll_speed_base = 3

# --- SPRITES DE AVIONES ---
# Jugador
player_sprite_raw = load_asset(os.path.join("sprites", "player.png"), 'image')
player_sprite = pygame.transform.scale(player_sprite_raw, (64, 64))

# Enemigo
enemy_raw = load_asset(os.path.join("sprites", "enemy.png"), 'image')
enemy_scaled = pygame.transform.scale(enemy_raw, (64, 64))
# Rotamos 180 grados asumiendo que la imagen original mira a la derecha
enemy_sprite = pygame.transform.rotate(enemy_scaled, 180) 

# Sonidos
snd_engine = load_asset(os.path.join("sounds", "engine_loop.wav"), 'sound')
snd_alert = load_asset(os.path.join("sounds", "alert.wav"), 'sound')

if snd_engine:
    snd_engine.set_volume(0.3)
    snd_engine.play(loops=-1)

# --- CONEXIÓN CON MOTORES EXTERNOS ---
haskell_exe = os.path.join("physics_engine", "movement_binary.exe")
if not os.path.exists(haskell_exe):
    sys.exit("Error: Falta physics_engine/movement_binary.exe")

proc_hs = subprocess.Popen(
    [haskell_exe],
    stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
    text=True, bufsize=0
)

prolog_file = os.path.join("ai_brain", "dogfight.pl")
proc_pl = subprocess.Popen(
    ["swipl", "-q", "-g", "main_loop", "-t", "halt", prolog_file],
    stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
    text=True, bufsize=0
)
print("Motores listos. Assets cargados.")

# --- Funciones Auxiliares ---
def ask_haskell(state, rot, thrust):
    msg = f"{state['x']} {state['y']} {state['angle']} {state['speed']} {rot} {thrust}\n"
    try:
        proc_hs.stdin.write(msg)
        return json.loads(proc_hs.stdout.readline())
    except Exception: return state

def ask_prolog(ex, ey, px, py):
    msg = f"[{ex}, {ey}, {px}, {py}].\n"
    try:
        proc_pl.stdin.write(msg)
        line = proc_pl.stdout.readline()
        if not line: return {"action": "ESPERA", "message": "Prolog desconectado"}
        return json.loads(line)
    except Exception: return {"action": "ERROR", "message": "Error I/O"}

def draw_sprite_rotated(surface, sprite, x, y, angle):
    rotated_image = pygame.transform.rotate(sprite, -angle)
    new_rect = rotated_image.get_rect(center=(x, y))
    surface.blit(rotated_image, new_rect.topleft)

# --- Entidades ---
player = {"x": 200.0, "y": 300.0, "angle": 0.0, "speed": 0.0}
enemy =  {"x": 600.0, "y": 300.0, "action": "PATRULLAR", "msg": "Iniciando...", "angle": 180.0}
last_enemy_action = "PATRULLAR"

# --- Bucle Principal ---
running = True
joystick = pygame.joystick.Joystick(0) if pygame.joystick.get_count() > 0 else None
if joystick: joystick.init()

while running:
    for event in pygame.event.get():
        if event.type == pygame.QUIT: running = False

    # 1. Input
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

    # 2. HASKELL & PROLOG
    player = ask_haskell(player, rot, thrust)
    decision = ask_prolog(enemy['x'], enemy['y'], player['x'], player['y'])
    enemy['action'] = decision['action']
    enemy['msg'] = decision['message']

    if enemy['action'] == 'ATACAR' and last_enemy_action != 'ATACAR':
        if snd_alert: snd_alert.play()
    last_enemy_action = enemy['action']

    # 3. Lógica Enemigo (Python)
    target_angle_rad = math.atan2(player['y'] - enemy['y'], player['x'] - enemy['x'])
    enemy['angle'] = math.degrees(target_angle_rad)
    
    enemy_speed = 2.0
    if enemy['action'] == 'ATACAR': enemy_speed = 4.5
    elif enemy['action'] == 'PERSEGUIR': enemy_speed = 3.0
    elif enemy['action'] == 'PATRULLAR':
        enemy_speed = 1.5
        enemy['angle'] = 180
    
    enemy['x'] += math.cos(math.radians(enemy['angle'])) * enemy_speed
    enemy['y'] += math.sin(math.radians(enemy['angle'])) * enemy_speed

    # --- RENDERIZADO ---
    
    # Fondo infinito vertical
    bg_y += scroll_speed_base + (player['speed'] * 0.4)
    if bg_y >= HEIGHT:
        bg_y = 0
        
    screen.blit(bg_image, (0, bg_y))
    screen.blit(bg_image, (0, bg_y - HEIGHT))
    
    # Aviones
    draw_sprite_rotated(screen, player_sprite, player['x'], player['y'], player['angle'])
    draw_sprite_rotated(screen, enemy_sprite, enemy['x'], enemy['y'], enemy['angle'])

    # HUD
    hud_bg = pygame.Surface((WIDTH, 30))
    hud_bg.set_alpha(150)
    hud_bg.fill((0,0,0))
    screen.blit(hud_bg, (0,0))
    
    hud_hs = f"[HASKELL ENGINE] POS:({int(player['x']):03},{int(player['y']):03}) | ANG:{int(player['angle'])%360:03}° | SPD:{player['speed']:.1f}"
    screen.blit(font_hud.render(hud_hs, True, HUD_COLOR), (10, 8))

    hud_bg_bot = pygame.Surface((WIDTH, 60))
    hud_bg_bot.set_alpha(150)
    hud_bg_bot.fill((0,0,0))
    screen.blit(hud_bg_bot, (0, HEIGHT-60))
    
    ai_text_color = ALERT_COLOR if enemy['action'] == 'ATACAR' else WHITE
    screen.blit(font_big.render(f"ENEMY AI: {enemy['action']}", True, ai_text_color), (20, HEIGHT-50))
    screen.blit(font_hud.render(f"RADIO: \"{enemy['msg']}\"", True, WHITE), (20, HEIGHT-25))

    pygame.display.flip()
    clock.tick(60)

# Limpieza
if snd_engine: snd_engine.stop()
pygame.mixer.quit()
proc_hs.terminate()
proc_pl.terminate()
pygame.quit()