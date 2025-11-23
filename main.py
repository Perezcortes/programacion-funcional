import pygame
import sys
import subprocess
import json
import os
import math
import random
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
pygame.display.set_caption("WW2 Dogfight: Ultimate Edition")
clock = pygame.time.Clock()

# Colores
WHITE = (255, 255, 255)
BLACK = (0, 0, 0)
GREEN = (50, 255, 50)
YELLOW = (255, 255, 0)
RED = (255, 50, 50)
ORANGE = (255, 150, 0)
DARK_OVERLAY = (0, 0, 0, 180)

# Fuentes
font_title = pygame.font.SysFont("Stencil", 50, bold=True)
font_sub = pygame.font.SysFont("Consolas", 20)
font_info = pygame.font.SysFont("Arial", 16)
font_hud = pygame.font.SysFont("Arial", 18, bold=True)

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
        print(f"FALTA ASSET: {full_path}")
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
        if type == 'image': 
            surf = pygame.Surface((64, 64))
            surf.fill((100, 100, 100))
            return surf.convert_alpha()
        return None

# Fondo
bg_raw = load_asset(os.path.join("sprites", "background.jpg"), 'image')
bg_image = pygame.transform.scale(bg_raw, (WIDTH, HEIGHT))

# Sprites
player_raw = load_asset(os.path.join("sprites", "player.png"), 'image')
enemy_raw = load_asset(os.path.join("sprites", "enemy.png"), 'image')
player_scaled = pygame.transform.scale(player_raw, (64, 64))
enemy_scaled = pygame.transform.scale(enemy_raw, (64, 64))
player_sprite = pygame.transform.rotate(player_scaled, -90)
enemy_sprite = pygame.transform.rotate(enemy_scaled, 90)

# Sonidos
snd_engine = load_asset(os.path.join("sounds", "engine_loop.wav"), 'sound')
if snd_engine: snd_engine.set_volume(0.3)
snd_alert = load_asset(os.path.join("sounds", "alert.wav"), 'sound')
snd_shoot = None

# --- MOTORES EXTERNOS ---
haskell_exe = os.path.join("physics_engine", "movement_binary.exe")
if not os.path.exists(haskell_exe): 
    print(f"Error: Falta binario Haskell en {haskell_exe}")
    print("Compilalo con: cd physics_engine && ghc -O2 --make Movement.hs -o movement_binary.exe")
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
        # Mantener campos que no vienen de Haskell
        return {
            'x': result['x'],
            'y': result['y'],
            'angle': result['angle'],
            'speed': result['speed'],
            'health': state.get('health', 100),
            'max_health': state.get('max_health', 100)
        }
    except Exception as e:
        print(f"Error Haskell: {e}")
        return state

def ask_prolog(ex, ey, px, py, time_step):
    try:
        proc_pl.stdin.write(f"[{ex}, {ey}, {px}, {py}, {time_step}].\n")
        proc_pl.stdin.flush()
        line = proc_pl.stdout.readline()
        return json.loads(line) if line else {"action": "WAIT", "message": "..."}
    except Exception as e:
        return {"action": "PATRULLAR", "message": "..."}

# --- FUNCIÓN DE RESET DEL JUEGO ---
def reset_game():
    """Reinicia todas las variables del juego"""
    player = {
        "x": 200.0, 
        "y": 300.0, 
        "angle": 0.0, 
        "speed": 0.0, 
        "health": 100, 
        "max_health": 100
    }
    enemies = [{
        "x": 600.0, 
        "y": 300.0, 
        "action": "PATRULLAR", 
        "msg": "...", 
        "angle": 180.0, 
        "health": 100, 
        "max_health": 100, 
        "shoot_cooldown": 0
    }]
    bullets = []
    particles = []
    score = 0
    player_shoot_cooldown = 0
    game_over = False
    victory = False
    
    return player, enemies, bullets, particles, score, player_shoot_cooldown, game_over, victory

# --- INICIALIZACIÓN DE VARIABLES ---
game_state = "MENU"
player, enemies, bullets, particles, score, player_shoot_cooldown, game_over, victory = reset_game()
time_step = 0
blink_timer = 0
bg_y = 0
screen_shake = 0

# --- BUCLE PRINCIPAL ---
running = True
joystick = None

while running:
    dt = clock.tick(60) / 1000.0
    time_step += 1
    
    # Detección dinámica de joystick
    if pygame.joystick.get_count() > 0 and joystick is None:
        joystick = pygame.joystick.Joystick(0)
        joystick.init()
    elif pygame.joystick.get_count() == 0:
        joystick = None

    # Eventos
    for event in pygame.event.get():
        if event.type == pygame.QUIT: 
            running = False
        
        if game_state == "MENU":
            start_game = False
            if event.type == pygame.KEYDOWN:
                if event.key in [pygame.K_SPACE, pygame.K_RETURN]: start_game = True
            if event.type == pygame.JOYBUTTONDOWN: start_game = True
            
            if start_game:
                game_state = "PLAYING"
                if snd_engine: snd_engine.play(-1)
                player, enemies, bullets, particles, score, player_shoot_cooldown, game_over, victory = reset_game()
        
        elif game_state == "GAME_OVER":
            if event.type == pygame.KEYDOWN or event.type == pygame.JOYBUTTONDOWN:
                game_state = "MENU"

    # RENDERIZADO DE FONDO
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
            draw_text_centered(screen, f"JOYSTICK: {name}", font_info, color_pulse, 0)
            draw_text_centered(screen, "Presiona [CUALQUIER BOTÓN] para Despegar", font_sub, WHITE, 40)
        else:
            pygame.draw.rect(screen, YELLOW, (WIDTH//2 - 150, HEIGHT//2 - 20, 300, 40), 2, border_radius=10)
            draw_text_centered(screen, "JOYSTICK NO ENCONTRADO", font_info, YELLOW, 0)
            draw_text_centered(screen, "Modo Teclado: Usa Flechas", font_info, WHITE, 40)
            draw_text_centered(screen, "Presiona [ESPACIO] para Despegar", font_sub, WHITE, 80)
        
        draw_text_centered(screen, "CONTROLES: Movimiento + [X] Disparar", font_info, (200, 200, 200), 140)

    # ==========================================
    # ESTADO: JUEGO
    # ==========================================
    elif game_state == "PLAYING" and not game_over:
        # 1. INPUTS
        rot, thrust, shoot = 0.0, 0.0, False
        if joystick:
            rot = joystick.get_axis(0)
            thrust = -joystick.get_axis(1)
            if joystick.get_numbuttons() > 0:
                shoot = joystick.get_button(0) or (joystick.get_button(1) if joystick.get_numbuttons() > 1 else False)
        else:
            keys = pygame.key.get_pressed()
            if keys[pygame.K_LEFT]:  rot = -1.0
            if keys[pygame.K_RIGHT]: rot = 1.0
            if keys[pygame.K_UP]:    thrust = 1.0
            if keys[pygame.K_DOWN]:  thrust = -0.5
            shoot = keys[pygame.K_x]

        # 2. FÍSICA DEL JUGADOR
        player = ask_haskell(player, rot, thrust)
        
        # Estela de humo
        if thrust > 0.1 and time_step % 3 == 0:
            create_smoke_trail(player['x'], player['y'], particles)

        # 3. DISPARO DEL JUGADOR
        player_shoot_cooldown = max(0, player_shoot_cooldown - 1)
        if shoot and player_shoot_cooldown == 0:
            angle_rad = math.radians(player['angle'])
            bullet_speed = 12
            bullets.append(Bullet(
                x=player['x'] + math.cos(angle_rad) * 30,
                y=player['y'] + math.sin(angle_rad) * 30,
                vx=math.cos(angle_rad) * bullet_speed + player['speed'] * 0.5,
                vy=math.sin(angle_rad) * bullet_speed,
                owner="player"
            ))
            player_shoot_cooldown = 15
            if snd_shoot: snd_shoot.play()

        # 4. IA Y ENEMIGOS
        for enemy in enemies:
            if enemy['health'] <= 0:
                continue
                
            decision = ask_prolog(enemy['x'], enemy['y'], player['x'], player['y'], time_step)
            enemy['action'] = decision.get('action', 'PATRULLAR')
            enemy['msg'] = decision.get('message', '...')

            if enemy['action'] == 'ATACAR':
                if snd_alert and time_step % 120 == 0: 
                    if snd_alert: snd_alert.play()

            target_angle = math.atan2(player['y'] - enemy['y'], player['x'] - enemy['x'])
            enemy['angle'] = math.degrees(target_angle)
            
            spd_mult = 4.5 if enemy['action'] == 'ATACAR' else 2.0
            enemy['x'] += math.cos(target_angle) * spd_mult
            enemy['y'] += math.sin(target_angle) * spd_mult
            
            enemy['x'] = enemy['x'] % WIDTH
            enemy['y'] = enemy['y'] % HEIGHT

            enemy['shoot_cooldown'] = max(0, enemy['shoot_cooldown'] - 1)
            if enemy['action'] == 'ATACAR' and enemy['shoot_cooldown'] == 0:
                bullets.append(Bullet(
                    x=enemy['x'],
                    y=enemy['y'],
                    vx=math.cos(target_angle) * 10,
                    vy=math.sin(target_angle) * 10,
                    owner="enemy"
                ))
                enemy['shoot_cooldown'] = random.randint(40, 60)

        # 5. ACTUALIZAR BALAS Y COLISIONES
        bullets = [b for b in bullets if b.update()]
        
        for bullet in bullets[:]:
            if bullet.owner == "player":
                for enemy in enemies:
                    if enemy['health'] > 0 and check_collision(bullet, enemy):
                        enemy['health'] -= 20
                        if bullet in bullets: bullets.remove(bullet)
                        create_explosion(enemy['x'], enemy['y'], particles, intensity=15)
                        screen_shake = 5
                        score += 10
                        if enemy['health'] <= 0:
                            create_explosion(enemy['x'], enemy['y'], particles, intensity=40)
                            score += 100
                            if snd_alert: snd_alert.play()
                        break
            else:
                if check_collision(bullet, player):
                    player['health'] -= 15
                    if bullet in bullets: bullets.remove(bullet)
                    create_explosion(player['x'], player['y'], particles, intensity=10)
                    screen_shake = 8

        # 6. ACTUALIZAR PARTÍCULAS
        particles = [p for p in particles if p.update()]

        # 7. CONDICIONES DE VICTORIA/DERROTA
        if player['health'] <= 0:
            game_over = True
            victory = False
            if snd_engine: snd_engine.stop()
        elif all(e['health'] <= 0 for e in enemies):
            game_over = True
            victory = True
            if snd_engine: snd_engine.stop()

        # 8. RENDERIZADO
        bg_y += 3 + (player['speed'] * 0.4)
        if bg_y >= HEIGHT: bg_y = 0
        
        shake_x = random.randint(-screen_shake, screen_shake) if screen_shake > 0 else 0
        shake_y = random.randint(-screen_shake, screen_shake) if screen_shake > 0 else 0
        screen_shake = max(0, screen_shake - 1)
        
        screen.blit(bg_image, (shake_x, bg_y + shake_y))
        screen.blit(bg_image, (shake_x, bg_y - HEIGHT + shake_y))

        for p in particles:
            p.draw(screen)

        for b in bullets:
            b.draw(screen)

        draw_sprite_rotated(screen, player_sprite, player['x'], player['y'], player['angle'])
        for enemy in enemies:
            if enemy['health'] > 0:
                draw_sprite_rotated(screen, enemy_sprite, enemy['x'], enemy['y'], enemy['angle'])

        hud_bar = pygame.Surface((WIDTH, 120), pygame.SRCALPHA)
        hud_bar.fill((0, 0, 0, 180))
        screen.blit(hud_bar, (0, HEIGHT-120))

        draw_health_bar(screen, 20, HEIGHT - 110, 200, 20, player['health'], 
                       player['max_health'], "TU AVIÓN")
        
        alive_enemies = [e for e in enemies if e['health'] > 0]
        if alive_enemies:
            enemy = alive_enemies[0]
            draw_health_bar(screen, WIDTH - 220, HEIGHT - 110, 200, 20, 
                          enemy['health'], enemy['max_health'], "ENEMIGO")
            col_ai = RED if enemy['action'] == 'ATACAR' else GREEN
            screen.blit(font_hud.render(f"IA: {enemy['action']}", True, col_ai), 
                       (WIDTH - 220, HEIGHT - 70))

        screen.blit(font_sub.render(f"VELOCIDAD: {player['speed']:.1f} | ÁNGULO: {int(player['angle'])%360}°", 
                                   True, GREEN), (20, 20))
        screen.blit(font_title.render(f"SCORE: {score}", True, YELLOW), (WIDTH//2 - 100, 20))

        draw_radar(screen, player, enemies, WIDTH - 110, 10, 100)

    elif game_state == "PLAYING" and game_over:
        game_state = "GAME_OVER"
        
    if game_state == "GAME_OVER":
        overlay = pygame.Surface((WIDTH, HEIGHT), pygame.SRCALPHA)
        overlay.fill(DARK_OVERLAY)
        screen.blit(overlay, (0,0))
        
        if victory:
            draw_text_centered(screen, "¡VICTORIA!", font_title, GREEN, -100)
            draw_text_centered(screen, "Has derrotado al enemigo", font_sub, WHITE, -40)
        else:
            draw_text_centered(screen, "DERRIBADO", font_title, RED, -100)
            draw_text_centered(screen, "Tu avión fue destruido", font_sub, WHITE, -40)
        
        draw_text_centered(screen, f"PUNTUACIÓN FINAL: {score}", font_title, YELLOW, 20)
        draw_text_centered(screen, "Presiona cualquier tecla para continuar", font_info, WHITE, 80)

    pygame.display.flip()

# Limpieza final
if snd_engine: snd_engine.stop()
pygame.mixer.quit()
proc_hs.terminate()
proc_pl.terminate()
pygame.quit()
sys.exit()