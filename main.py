import pygame
import sys
import subprocess
import json
import os
import math
import random
import serial
import serial.tools.list_ports
from dataclasses import dataclass
from typing import List, Optional

# --- Configuración Inicial ---
pygame.init()
pygame.joystick.init()
pygame.font.init()

try:
    pygame.mixer.init(44100, -16, 2, 512)
except pygame.error:
    print("Advertencia: No se pudo iniciar el audio.")

WIDTH, HEIGHT = 800, 600
screen = pygame.display.set_mode((WIDTH, HEIGHT))
pygame.display.set_caption("WW2 Dogfight: Pico Ultimate Edition")
clock = pygame.time.Clock()

# Colores
WHITE = (255, 255, 255)
BLACK = (0, 0, 0)
GREEN = (50, 255, 50)
YELLOW = (255, 255, 0)
RED = (255, 50, 50)
ORANGE = (255, 150, 0)
GOLD = (255, 215, 0)
DARK_OVERLAY = (0, 0, 0, 180)

# Fuentes
font_title = pygame.font.SysFont("Stencil", 50, bold=True)
font_sub = pygame.font.SysFont("Consolas", 20)
font_info = pygame.font.SysFont("Arial", 16)
font_hud = pygame.font.SysFont("Arial", 18, bold=True)
font_big = pygame.font.SysFont("Arial", 36, bold=True)

# --- CLASE PARA LEER LA PICO (SERIAL) ---
class PicoController:
    def __init__(self):
        self.ser = None
        self.x = 0.0 # -1.0 a 1.0
        self.y = 0.0 # -1.0 a 1.0
        self.btn = False
        self.btn_pressed = False  # Para detectar cuando se presiona, no mantener
        self.connected = False
        self.port_name = ""
        self.raw_data = "" # Para debugging visual
        self.prev_btn = False  # Para detectar flanco de subida
        self.connect()

    def connect(self):
        ports = list(serial.tools.list_ports.comports())
        for p in ports:
            if "COM1" in p.device:
                continue
            if "USB" in p.description or "Serial" in p.description:
                try:
                    self.ser = serial.Serial(p.device, 115200, timeout=0.01)
                    self.port_name = p.device
                    self.connected = True
                    print(f"✅ Pico conectada en {self.port_name}")
                    return
                except:
                    pass
        print("⚠️ Pico no encontrada (Usando modo Teclado).")

    def update(self):
        # Resetear btn_pressed cada frame
        self.btn_pressed = False
        
        if not self.ser or not self.ser.is_open: 
            self.x, self.y, self.btn = 0, 0, False
            return

        try:
            if self.ser.in_waiting > 0:
                content = self.ser.read(self.ser.in_waiting).decode('utf-8', errors='ignore')
                lines = content.split('\n')
                
                for line in reversed(lines):
                    line = line.strip()
                    if not line: continue
                    self.raw_data = line
                    
                    parts = line.split(',')
                    if len(parts) == 3:
                        try:
                            raw_x = int(parts[0])
                            raw_y = int(parts[1])
                            raw_btn = int(parts[2])

                            # --- LÓGICA ANALÓGICA PARA AVIONES (SUAVE) ---
                            # Normalizar (-1.0 a 1.0)
                            norm_x = (raw_x - 32768) / 32768.0
                            norm_y = (raw_y - 32768) / 32768.0
                            
                            # Zona muerta pequeña para evitar drift
                            if abs(norm_x) < 0.1: norm_x = 0
                            if abs(norm_y) < 0.1: norm_y = 0
                            
                            # Curva de respuesta suave (no snap digital)
                            self.x = norm_x * abs(norm_x)  # Respuesta cuadrática
                            self.y = norm_y * abs(norm_y)  # Más control en movimientos pequeños
                            
                            # Botón con detección de flanco
                            current_btn = (raw_btn == 0)
                            if current_btn and not self.prev_btn:
                                self.btn_pressed = True
                            self.btn = current_btn
                            self.prev_btn = current_btn
                            break 
                        except ValueError:
                            continue
        except Exception:
            self.connected = False
            try: 
                self.ser.close()
            except: 
                pass

pico_ctrl = PicoController()

# --- SISTEMA DE PARTÍCULAS ---
@dataclass
class Particle:
    x: float
    y: float
    vx: float
    vy: float
    color: tuple
    lifetime: int
    max_lifetime: int
    size: int = 3

    def update(self) -> bool:
        self.x += self.vx
        self.y += self.vy
        self.lifetime -= 1
        self.vx *= 0.98
        self.vy *= 0.98
        return self.lifetime > 0

    def draw(self, screen):
        alpha = int(255 * (self.lifetime / self.max_lifetime))
        surf = pygame.Surface((self.size * 2, self.size * 2), pygame.SRCALPHA)
        color_with_alpha = (*self.color, alpha)
        pygame.draw.circle(surf, color_with_alpha, (self.size, self.size), self.size)
        screen.blit(surf, (int(self.x) - self.size, int(self.y) - self.size))

def create_explosion(x: float, y: float, particles: List[Particle], intensity: int = 20):
    for _ in range(intensity):
        angle = random.uniform(0, 2 * math.pi)
        speed = random.uniform(2, 6)
        particles.append(Particle(
            x=x, y=y,
            vx=math.cos(angle) * speed,
            vy=math.sin(angle) * speed,
            color=random.choice([(255, 150, 0), (255, 100, 0), (255, 200, 100)]),
            lifetime=random.randint(20, 40),
            max_lifetime=40,
            size=random.randint(2, 4)
        ))

def create_smoke_trail(x: float, y: float, particles: List[Particle]):
    particles.append(Particle(
        x=x + random.uniform(-3, 3),
        y=y + random.uniform(-3, 3),
        vx=random.uniform(-0.5, 0.5),
        vy=random.uniform(-0.5, 0.5),
        color=(100, 100, 100),
        lifetime=30,
        max_lifetime=30,
        size=2
    ))

# --- SISTEMA DE BALAS ---
@dataclass
class Bullet:
    x: float
    y: float
    vx: float
    vy: float
    owner: str
    lifetime: int = 180

    def update(self) -> bool:
        self.x += self.vx
        self.y += self.vy
        self.lifetime -= 1
        return (0 <= self.x <= WIDTH and 0 <= self.y <= HEIGHT and self.lifetime > 0)

    def draw(self, screen):
        color = YELLOW if self.owner == "player" else RED
        pygame.draw.circle(screen, color, (int(self.x), int(self.y)), 3)
        pygame.draw.line(screen, color, (int(self.x), int(self.y)), 
                         (int(self.x - self.vx), int(self.y - self.vy)), 2)

def check_collision(bullet: Bullet, target: dict) -> bool:
    dx = bullet.x - target['x']
    dy = bullet.y - target['y']
    distance = math.sqrt(dx*dx + dy*dy)
    return distance < 32

# --- CARGA DE ASSETS ---
def load_asset(path, type='image'):
    full_path = os.path.join("assets", path)
    if not os.path.exists(full_path):
        if type == 'image': 
            surf = pygame.Surface((64, 64))
            surf.fill((100, 100, 100))
            return surf.convert_alpha()
        if type == 'sound': return None
    try:
        if type == 'image': return pygame.image.load(full_path).convert_alpha()
        elif type == 'sound': return pygame.mixer.Sound(full_path)
    except Exception as e:
        print(f"Error cargando {full_path}: {e}")
        return None

# Assets
bg_raw = load_asset(os.path.join("sprites", "background.jpg"), 'image')
bg_image = pygame.transform.scale(bg_raw, (WIDTH, HEIGHT))

player_raw = load_asset(os.path.join("sprites", "player.png"), 'image')
enemy_raw = load_asset(os.path.join("sprites", "enemy.png"), 'image')
player_scaled = pygame.transform.scale(player_raw, (64, 64))
enemy_scaled = pygame.transform.scale(enemy_raw, (64, 64))
player_sprite = pygame.transform.rotate(player_scaled, -90)
enemy_sprite = pygame.transform.rotate(enemy_scaled, 90)

snd_engine = load_asset(os.path.join("sounds", "engine_loop.wav"), 'sound')
if snd_engine: snd_engine.set_volume(0.3)
snd_alert = load_asset(os.path.join("sounds", "alert.wav"), 'sound')
snd_shoot = load_asset(os.path.join("sounds", "shoot.wav"), 'sound')
snd_explosion = load_asset(os.path.join("sounds", "explosion.wav"), 'sound')
snd_victory = load_asset(os.path.join("sounds", "victory.wav"), 'sound')

# --- MOTORES EXTERNOS ---
if os.name == 'nt':
    haskell_exe = os.path.join("physics_engine", "movement_binary.exe")
else:
    haskell_exe = os.path.join("physics_engine", "movement_binary")

if not os.path.exists(haskell_exe): 
    print(f"Error: Falta binario Haskell en {haskell_exe}")
    sys.exit(1)

try:
    proc_hs = subprocess.Popen([haskell_exe], stdin=subprocess.PIPE, 
                               stdout=subprocess.PIPE, text=True, bufsize=0)
except Exception as e:
    print(f"Error al iniciar Haskell: {e}")
    sys.exit(1)

prolog_file = os.path.join("ai_brain", "dogfight.pl")
try:
    proc_pl = subprocess.Popen(["swipl", "-q", "-g", "main_loop", "-t", "halt", prolog_file], 
                               stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True, bufsize=0)
except Exception as e:
    print(f"Error al iniciar Prolog: {e}")
    proc_hs.terminate()
    sys.exit(1)

# --- FUNCIONES AUXILIARES ---
def draw_text_centered(surface, text, font, color, y_offset=0):
    text_obj = font.render(text, True, color)
    rect = text_obj.get_rect(center=(WIDTH//2, HEIGHT//2 + y_offset))
    surface.blit(text_obj, rect)

def draw_sprite_rotated(surface, sprite, x, y, angle):
    rotated = pygame.transform.rotate(sprite, -angle)
    rect = rotated.get_rect(center=(x, y))
    surface.blit(rotated, rect.topleft)

def draw_health_bar(surface, x, y, width, height, health, max_health, label=""):
    pygame.draw.rect(surface, (50, 50, 50), (x, y, width, height))
    health_width = int((health / max_health) * width)
    color = GREEN if health > 60 else (YELLOW if health > 30 else RED)
    pygame.draw.rect(surface, color, (x, y, health_width, height))
    pygame.draw.rect(surface, WHITE, (x, y, width, height), 2)
    if label:
        text = font_info.render(f"{label}: {int(health)}/{max_health}", True, WHITE)
        surface.blit(text, (x, y - 20))

def draw_radar(surface, player, enemies, x, y, size):
    radar_surf = pygame.Surface((size, size), pygame.SRCALPHA)
    pygame.draw.circle(radar_surf, (0, 50, 0, 150), (size//2, size//2), size//2)
    pygame.draw.circle(radar_surf, GREEN, (size//2, size//2), size//2, 2)
    pygame.draw.circle(radar_surf, YELLOW, (size//2, size//2), 4)
    scale = size / (WIDTH * 1.5)
    for enemy in enemies:
        if enemy.get('health', 0) > 0:
            rel_x = (enemy['x'] - player['x']) * scale
            rel_y = (enemy['y'] - player['y']) * scale
            radar_x = int(size//2 + rel_x)
            radar_y = int(size//2 + rel_y)
            if 0 <= radar_x < size and 0 <= radar_y < size:
                pygame.draw.circle(radar_surf, RED, (radar_x, radar_y), 3)
    surface.blit(radar_surf, (x, y))

def ask_haskell(state, rot, thrust):
    try:
        input_str = f"{state['x']} {state['y']} {state['angle']} {state['speed']} {rot} {thrust}\n"
        proc_hs.stdin.write(input_str)
        proc_hs.stdin.flush()
        response = proc_hs.stdout.readline()
        result = json.loads(response)
        return {
            'x': result['x'],
            'y': result['y'],
            'angle': result['angle'],
            'speed': result['speed'],
            'health': state.get('health', 100),
            'max_health': state.get('max_health', 100)
        }
    except: 
        return state

def ask_prolog(ex, ey, px, py, time_step):
    try:
        proc_pl.stdin.write(f"[{ex}, {ey}, {px}, {py}, {time_step}].\n")
        proc_pl.stdin.flush()
        line = proc_pl.stdout.readline()
        return json.loads(line) if line else {"action": "WAIT", "message": "..."}
    except: 
        return {"action": "PATRULLAR", "message": "..."}

# --- RESET DEL JUEGO ---
def reset_game():
    player = {"x": 200.0, "y": 300.0, "angle": 0.0, "speed": 0.0, "health": 100, "max_health": 100}
    enemies = [{"x": 600.0, "y": 300.0, "action": "PATRULLAR", "msg": "...", "angle": 180.0, "health": 100, "max_health": 100, "shoot_cooldown": 0}]
    return player, enemies, [], [], 0, 0, False, False, False, False

# --- VARIABLES ---
game_state = "MENU"
player, enemies, bullets, particles, score, player_shoot_cooldown, game_over, victory, enemy_destroyed, player_destroyed = reset_game()
time_step = 0
blink_timer = 0
bg_y = 0
screen_shake = 0
waiting_for_start = True  # Variable para controlar si esperamos inicio
game_over_display_timer = 0  # Para mostrar mensajes de game over

# --- BUCLE PRINCIPAL ---
running = True
joystick = None

while running:
    dt = clock.tick(60) / 1000.0
    time_step += 1
    
    # 1. Leer Pico
    pico_ctrl.update()

    # 2. Joystick USB (Backup)
    if pygame.joystick.get_count() > 0 and joystick is None:
        joystick = pygame.joystick.Joystick(0)
        joystick.init()
    elif pygame.joystick.get_count() == 0:
        joystick = None

    # 3. Eventos
    start_game = False
    for event in pygame.event.get():
        if event.type == pygame.QUIT: 
            running = False
        
        if game_state == "MENU":
            if event.type == pygame.KEYDOWN:
                if event.key in [pygame.K_SPACE, pygame.K_RETURN]: 
                    start_game = True
            if event.type == pygame.JOYBUTTONDOWN: 
                start_game = True
            if pico_ctrl.btn_pressed: 
                start_game = True
        
        elif game_state == "GAME_OVER":
            if event.type == pygame.KEYDOWN or event.type == pygame.JOYBUTTONDOWN or pico_ctrl.btn_pressed:
                game_state = "MENU"
                waiting_for_start = True
                game_over_display_timer = 0
                enemy_destroyed = False
                player_destroyed = False

    # 4. Manejo de inicio del juego
    if game_state == "MENU" and start_game:
        game_state = "PLAYING"
        waiting_for_start = False
        if snd_engine: 
            snd_engine.play(-1)
        player, enemies, bullets, particles, score, player_shoot_cooldown, game_over, victory, enemy_destroyed, player_destroyed = reset_game()

    # 5. Renderizado Fondo
    screen.blit(bg_image, (0, 0))

    if game_state == "MENU":
        overlay = pygame.Surface((WIDTH, HEIGHT), pygame.SRCALPHA)
        overlay.fill(DARK_OVERLAY)
        screen.blit(overlay, (0,0))
        draw_text_centered(screen, "WW2 FUNCTIONAL DOGFIGHT", font_title, WHITE, -150)
        
        blink_timer += 1
        color_pulse = GREEN if (blink_timer // 30) % 2 == 0 else (100, 255, 100)
        
        if pico_ctrl.connected:
            pygame.draw.rect(screen, GREEN, (WIDTH//2 - 150, HEIGHT//2 - 20, 300, 40), 2, border_radius=10)
            draw_text_centered(screen, f"HARDWARE: PICO EN {pico_ctrl.port_name}", font_info, color_pulse, 0)
            draw_text_centered(screen, "Presiona BOTÓN del joystick para Jugar", font_sub, WHITE, 40)
            
        elif joystick:
            name = joystick.get_name()[:15]
            pygame.draw.rect(screen, GREEN, (WIDTH//2 - 150, HEIGHT//2 - 20, 300, 40), 2, border_radius=10)
            draw_text_centered(screen, f"JOYSTICK: {name}", font_info, color_pulse, 0)
            draw_text_centered(screen, "Presiona [BOTÓN] para Jugar", font_sub, WHITE, 40)
        else:
            pygame.draw.rect(screen, YELLOW, (WIDTH//2 - 150, HEIGHT//2 - 20, 300, 40), 2, border_radius=10)
            draw_text_centered(screen, "TECLADO DETECTADO", font_info, YELLOW, 0)
            draw_text_centered(screen, "Espacio para Jugar", font_sub, WHITE, 40)
        
        draw_text_centered(screen, "CONTROLES: Movimiento + Disparar", font_info, (200, 200, 200), 180)

    elif game_state == "PLAYING" and not game_over:
        # --- LÓGICA DE CONTROL MEJORADA PARA AVIONES ---
        rot, thrust = 0.0, 0.0
        shoot = False

        # 1. Control Pico (Analógico mejorado)
        if pico_ctrl.connected:
            # Eje X = Rotación (giro izquierda/derecha)
            rot += pico_ctrl.x * 2.5  # Multiplicador para rotación más rápida
            
            # Eje Y = Aceleración/Desaceleración (invertido)
            thrust += -pico_ctrl.y
            
            # Botón para disparar
            if pico_ctrl.btn_pressed: 
                shoot = True

        # 2. Joystick USB
        if joystick:
            # Eje 0: Rotación (giro)
            rot += joystick.get_axis(0) * 2.0
            
            # Eje 1: Aceleración/Desaceleración
            thrust += -joystick.get_axis(1)
            
            if joystick.get_numbuttons() > 0:
                if joystick.get_button(0): 
                    shoot = True

        # 3. Teclado (con mejor control)
        keys = pygame.key.get_pressed()
        if keys[pygame.K_LEFT]: rot -= 1.5
        if keys[pygame.K_RIGHT]: rot += 1.5
        if keys[pygame.K_UP]: thrust += 1.0
        if keys[pygame.K_DOWN]: thrust -= 0.7
        if keys[pygame.K_SPACE] or keys[pygame.K_x]: 
            shoot = True
        # Movimiento lateral opcional con A/D
        if keys[pygame.K_a]: 
            rad = math.radians(player['angle'] + 90)  # Lateral izquierdo
            player['x'] += math.cos(rad) * 3.0
            player['y'] += math.sin(rad) * 3.0
        if keys[pygame.K_d]: 
            rad = math.radians(player['angle'] - 90)  # Lateral derecho
            player['x'] += math.cos(rad) * 3.0
            player['y'] += math.sin(rad) * 3.0

        # Suavizar controles (opcional)
        rot = max(-1.0, min(1.0, rot * 0.9))
        thrust = max(-1.0, min(1.0, thrust))

        # Física Haskell
        player = ask_haskell(player, rot, thrust)
        if thrust > 0.1 and time_step % 3 == 0: 
            create_smoke_trail(player['x'], player['y'], particles)

        # Disparo Jugador
        player_shoot_cooldown = max(0, player_shoot_cooldown - 1)
        if shoot and player_shoot_cooldown == 0:
            rad = math.radians(player['angle'])
            bullets.append(Bullet(
                player['x'] + math.cos(rad)*30, 
                player['y'] + math.sin(rad)*30, 
                math.cos(rad)*12 + player['speed']*0.5, 
                math.sin(rad)*12, 
                "player"
            ))
            player_shoot_cooldown = 15
            if snd_shoot: 
                snd_shoot.play()

        # IA Prolog + Física Enemigo
        for enemy in enemies:
            if enemy['health'] <= 0: 
                continue
            dec = ask_prolog(enemy['x'], enemy['y'], player['x'], player['y'], time_step)
            enemy['action'] = dec.get('action', 'PATRULLAR')
            
            if enemy['action'] == 'ATACAR' and snd_alert and time_step % 120 == 0: 
                snd_alert.play()
            
            tgt = math.atan2(player['y'] - enemy['y'], player['x'] - enemy['x'])
            enemy['angle'] = math.degrees(tgt)
            sp = 4.5 if enemy['action'] == 'ATACAR' else 2.0
            enemy['x'] = (enemy['x'] + math.cos(tgt)*sp) % WIDTH
            enemy['y'] = (enemy['y'] + math.sin(tgt)*sp) % HEIGHT
            
            enemy['shoot_cooldown'] = max(0, enemy['shoot_cooldown'] - 1)
            if enemy['action'] == 'ATACAR' and enemy['shoot_cooldown'] == 0:
                bullets.append(Bullet(
                    enemy['x'], enemy['y'], 
                    math.cos(tgt)*10, 
                    math.sin(tgt)*10, 
                    "enemy"
                ))
                enemy['shoot_cooldown'] = random.randint(40, 60)

        # Actualizar Balas
        bullets = [b for b in bullets if b.update()]
        for b in bullets[:]:
            hit = False
            if b.owner == "player":
                for e in enemies:
                    if e['health'] > 0 and check_collision(b, e):
                        e['health'] -= 20
                        create_explosion(e['x'], e['y'], particles, 15)
                        score += 10
                        
                        if e['health'] <= 0:
                            create_explosion(e['x'], e['y'], particles, 40)
                            score += 100
                            enemy_destroyed = True  # ¡Enemigo destruido!
                            game_over_display_timer = 120  # 2 segundos de mensaje
                            if snd_explosion:
                                snd_explosion.play()
                            if snd_victory:
                                snd_victory.play()
                        else:
                            if snd_alert: 
                                snd_alert.play()
                        hit = True
                        break
            elif check_collision(b, player):
                player['health'] -= 15
                create_explosion(player['x'], player['y'], particles, 10)
                screen_shake = 8
                
                if player['health'] <= 0:
                    create_explosion(player['x'], player['y'], particles, 40)
                    player_destroyed = True  # ¡Jugador destruido!
                    game_over_display_timer = 120  # 2 segundos de mensaje
                    if snd_explosion:
                        snd_explosion.play()
                hit = True
            
            if hit: 
                bullets.remove(b)

        # Actualizar Partículas
        particles = [p for p in particles if p.update()]

        # Victoria/Derrota
        if player['health'] <= 0: 
            game_over = True
            victory = False
            if snd_engine:
                snd_engine.stop()
        elif all(e['health'] <= 0 for e in enemies): 
            game_over = True
            victory = True
            if snd_engine:
                snd_engine.stop()

        # Render
        bg_y = (bg_y + 3 + player['speed']*0.4) % HEIGHT
        sx = random.randint(-screen_shake, screen_shake) if screen_shake > 0 else 0
        sy = random.randint(-screen_shake, screen_shake) if screen_shake > 0 else 0
        screen_shake = max(0, screen_shake - 1)
        
        screen.blit(bg_image, (sx, bg_y + sy))
        screen.blit(bg_image, (sx, bg_y - HEIGHT + sy))

        for p in particles: 
            p.draw(screen)
        for b in bullets: 
            b.draw(screen)
        
        draw_sprite_rotated(screen, player_sprite, player['x'], player['y'], player['angle'])
        for e in enemies:
            if e['health'] > 0: 
                draw_sprite_rotated(screen, enemy_sprite, e['x'], e['y'], e['angle'])

        # HUD
        hud_h = 120
        overlay = pygame.Surface((WIDTH, hud_h), pygame.SRCALPHA)
        overlay.fill((0,0,0,180))
        screen.blit(overlay, (0, HEIGHT-hud_h))
        
        draw_health_bar(screen, 20, HEIGHT-100, 200, 20, player['health'], 100, "TU AVIÓN")
        alive = [e for e in enemies if e['health'] > 0]
        if alive:
            draw_health_bar(screen, WIDTH-220, HEIGHT-100, 200, 20, alive[0]['health'], 100, "ENEMIGO")
        
        draw_radar(screen, player, enemies, WIDTH-110, 10, 100)
        screen.blit(font_title.render(f"SCORE: {score}", True, YELLOW), (WIDTH//2 - 100, 20))
        
        # Mostrar mensajes temporales de destrucción
        if game_over_display_timer > 0:
            game_over_display_timer -= 1
            if enemy_destroyed:
                # Mensaje de ENEMIGO DESTRUIDO
                msg_surf = pygame.Surface((400, 60), pygame.SRCALPHA)
                msg_surf.fill((0, 0, 0, 180))
                pygame.draw.rect(msg_surf, GREEN, (0, 0, 400, 60), 3)
                screen.blit(msg_surf, (WIDTH//2 - 200, HEIGHT//2 - 30))
                
                msg_text = font_big.render("¡ENEMIGO DESTRUIDO!", True, GREEN)
                msg_rect = msg_text.get_rect(center=(WIDTH//2, HEIGHT//2))
                screen.blit(msg_text, msg_rect)
                
                bonus_text = font_info.render(f"+100 PUNTOS!", True, YELLOW)
                bonus_rect = bonus_text.get_rect(center=(WIDTH//2, HEIGHT//2 + 25))
                screen.blit(bonus_text, bonus_rect)
            
            elif player_destroyed:
                # Mensaje de JUGADOR DESTRUIDO
                msg_surf = pygame.Surface((400, 60), pygame.SRCALPHA)
                msg_surf.fill((0, 0, 0, 180))
                pygame.draw.rect(msg_surf, RED, (0, 0, 400, 60), 3)
                screen.blit(msg_surf, (WIDTH//2 - 200, HEIGHT//2 - 30))
                
                msg_text = font_big.render("¡AVIÓN DAÑADO!", True, RED)
                msg_rect = msg_text.get_rect(center=(WIDTH//2, HEIGHT//2))
                screen.blit(msg_text, msg_rect)

    elif game_state == "PLAYING" and game_over:
        # Pantalla de GAME OVER definitiva
        overlay = pygame.Surface((WIDTH, HEIGHT), pygame.SRCALPHA)
        overlay.fill((0, 0, 0, 200))
        screen.blit(overlay, (0, 0))
        
        # Mostrar fondo del juego detrás
        bg_y = (bg_y + 2) % HEIGHT
        screen.blit(bg_image, (0, bg_y))
        screen.blit(bg_image, (0, bg_y - HEIGHT))
        
        # Dibujar partículas restantes
        for p in particles: 
            p.draw(screen)
        
        if victory:
            # VICTORIA
            draw_text_centered(screen, "¡VICTORIA!", font_title, GREEN, -80)
            draw_text_centered(screen, f"SCORE FINAL: {score}", font_sub, YELLOW, -20)
            draw_text_centered(screen, "¡ENEMIGO DESTRUIDO!", font_big, GOLD, 20)
            
            # Mostrar estrellas de victoria
            for i in range(3):
                star_size = 20 + i * 5
                star_x = WIDTH//2 - 100 + i * 100
                star_y = HEIGHT//2 + 70
                pygame.draw.polygon(screen, GOLD, [
                    (star_x, star_y - star_size),
                    (star_x + star_size//3, star_y - star_size//3),
                    (star_x + star_size, star_y),
                    (star_x + star_size//3, star_y + star_size//3),
                    (star_x, star_y + star_size),
                    (star_x - star_size//3, star_y + star_size//3),
                    (star_x - star_size, star_y),
                    (star_x - star_size//3, star_y - star_size//3)
                ])
        else:
            # DERROTA
            draw_text_centered(screen, "GAME OVER", font_title, RED, -80)
            draw_text_centered(screen, f"SCORE FINAL: {score}", font_sub, YELLOW, -20)
            draw_text_centered(screen, "¡HAS SIDO DERRIBADO!", font_big, ORANGE, 20)
            
            # Mostrar cruz de derrota
            cross_size = 40
            cross_x = WIDTH//2
            cross_y = HEIGHT//2 + 70
            pygame.draw.line(screen, RED, (cross_x - cross_size, cross_y - cross_size), 
                            (cross_x + cross_size, cross_y + cross_size), 5)
            pygame.draw.line(screen, RED, (cross_x + cross_size, cross_y - cross_size), 
                            (cross_x - cross_size, cross_y + cross_size), 5)
        
        # Instrucciones para continuar
        blink_timer += 1
        if (blink_timer // 30) % 2 == 0:
            draw_text_centered(screen, "Presiona CUALQUIER TECLA o BOTÓN para continuar", font_info, WHITE, 100)

    elif game_state == "GAME_OVER":
        # Esta pantalla ya no se usa mucho, pero la mantenemos por compatibilidad
        overlay = pygame.Surface((WIDTH, HEIGHT), pygame.SRCALPHA)
        overlay.fill(DARK_OVERLAY)
        screen.blit(overlay, (0,0))
        txt, col = ("¡VICTORIA!", GREEN) if victory else ("DERRIBADO", RED)
        draw_text_centered(screen, txt, font_title, col, -50)
        draw_text_centered(screen, f"SCORE FINAL: {score}", font_sub, WHITE, 20)
        draw_text_centered(screen, "Presiona CUALQUIER TECLA o BOTÓN para Reiniciar", font_info, WHITE, 60)

    pygame.display.flip()

# Salida
if snd_engine: 
    snd_engine.stop()
try: 
    pygame.mixer.quit()
except: 
    pass
proc_hs.terminate()
proc_pl.terminate()
if pico_ctrl.ser: 
    pico_ctrl.ser.close()
pygame.quit()
sys.exit()