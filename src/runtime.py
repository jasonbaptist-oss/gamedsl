"""
runtime.py — GameDSL Runtime (Enhanced with Visual FX)
=======================================================

Enhancements over the base runtime:
  - Attack flash effects: when fire_player_ability or monster ability
    triggers, a short visual flash/burst is drawn at the target cell.
  - Health bars: drawn above every player and monster entity each frame.
  - Kill/death explosions: particle burst when a monster dies.
  - Win screen overlay: drawn over the game when check_win_condition()
    returns True.
  - Player death overlay: dimmed red flash when a player dies.
"""

import random
import math

try:
    import pygame
except ImportError:
    pygame = None  # allows --headless runs without pygame installed

CELL_SIZE = 32
TICK_RATE = 60  # ticks per simulated second

# ============================================================
# VISUAL EFFECT DATA STRUCTURES
# ============================================================

class AttackFlash:
    """A short rectangular flash drawn over a target cell."""
    def __init__(self, x, y, color, duration=0.15):
        self.x = x
        self.y = y
        self.color = color       # (R, G, B)
        self.duration = duration
        self.age = 0.0
        self.alive = True

    def update(self, dt):
        self.age += dt
        if self.age >= self.duration:
            self.alive = False

    def draw(self, screen):
        if not self.alive:
            return
        alpha = int(220 * (1.0 - self.age / self.duration))
        surf = pygame.Surface((CELL_SIZE, CELL_SIZE), pygame.SRCALPHA)
        surf.fill((*self.color, alpha))
        screen.blit(surf, (self.x * CELL_SIZE, self.y * CELL_SIZE))


class Particle:
    """A single particle for kill/death explosions."""
    def __init__(self, px, py, color):
        angle = random.uniform(0, 2 * math.pi)
        speed = random.uniform(40, 120)   # pixels/sec
        self.px = float(px)
        self.py = float(py)
        self.vx = math.cos(angle) * speed
        self.vy = math.sin(angle) * speed
        self.color = color
        self.radius = random.randint(3, 7)
        self.lifetime = random.uniform(0.35, 0.7)
        self.age = 0.0
        self.alive = True

    def update(self, dt):
        self.age += dt
        if self.age >= self.lifetime:
            self.alive = False
            return
        self.px += self.vx * dt
        self.py += self.vy * dt
        self.vy += 60 * dt   # slight gravity

    def draw(self, screen):
        if not self.alive:
            return
        frac = 1.0 - self.age / self.lifetime
        r = max(1, int(self.radius * frac))
        alpha = int(255 * frac)
        surf = pygame.Surface((r * 2 + 2, r * 2 + 2), pygame.SRCALPHA)
        pygame.draw.circle(surf, (*self.color, alpha), (r + 1, r + 1), r)
        screen.blit(surf, (int(self.px) - r - 1, int(self.py) - r - 1))


class BannerOverlay:
    """Full-screen semi-transparent banner for WIN / GAME OVER."""
    def __init__(self, text, bg_color, text_color, duration=None):
        self.text = text
        self.bg_color = bg_color
        self.text_color = text_color
        self.duration = duration   # None = show forever
        self.age = 0.0
        self.alive = True

    def update(self, dt):
        self.age += dt
        if self.duration is not None and self.age >= self.duration:
            self.alive = False

    def draw(self, screen, font_large, font_small=None):
        w, h = screen.get_size()
        overlay = pygame.Surface((w, h), pygame.SRCALPHA)
        # Fade in over 0.3 s
        alpha = min(200, int(200 * self.age / 0.3))
        overlay.fill((*self.bg_color, alpha))
        screen.blit(overlay, (0, 0))
        label = font_large.render(self.text, True, self.text_color)
        screen.blit(label, label.get_rect(center=(w // 2, h // 2)))


# ============================================================
# VFX MANAGER (singleton attached to run_game)
# ============================================================

class VFXManager:
    def __init__(self):
        self.flashes = []
        self.particles = []
        self.banners = []

    def add_attack_flash(self, x, y, color=(255, 220, 50)):
        self.flashes.append(AttackFlash(x, y, color))

    def add_kill_burst(self, x, y, color=(255, 80, 30), count=18):
        cx = x * CELL_SIZE + CELL_SIZE // 2
        cy = y * CELL_SIZE + CELL_SIZE // 2
        for _ in range(count):
            self.particles.append(Particle(cx, cy, color))

    def add_player_death_burst(self, x, y):
        self.add_kill_burst(x, y, color=(80, 80, 255), count=20)

    def add_banner(self, text, bg=(0, 0, 0), fg=(255, 255, 255), duration=None):
        self.banners.append(BannerOverlay(text, bg, fg, duration))

    def update(self, dt):
        for lst in (self.flashes, self.particles, self.banners):
            for fx in lst:
                fx.update(dt)
        self.flashes = [f for f in self.flashes if f.alive]
        self.particles = [p for p in self.particles if p.alive]
        self.banners = [b for b in self.banners if b.alive]

    def draw_world_layer(self, screen):
        """Draw effects that live in world-space (flashes, particles)."""
        for f in self.flashes:
            f.draw(screen)
        for p in self.particles:
            p.draw(screen)

    def draw_ui_layer(self, screen, font_large):
        """Draw fullscreen overlays on top of everything."""
        for b in self.banners:
            b.draw(screen, font_large)


# Module-level VFX manager shared between run_game and helpers
_vfx = VFXManager()


# ============================================================
# ABILITY
# ============================================================

class Ability:
    DEFAULT_FIELDS = (
        "damage", "range", "spread", "required_kills",
        "damage_multiplier", "damage_reduction",
        "health_regen", "speed_boost", "activates_at", "key",
    )

    def __init__(self, name, type, img=None, **fields):
        self.name = name
        self.type = type
        self.img = img
        for attr in self.DEFAULT_FIELDS:
            setattr(self, attr, fields.get(attr))
        self.shape = fields.get("shape", "manhattan")


# ============================================================
# DAMAGE ZONE GEOMETRY
# ============================================================

def in_range(ax, ay, bx, by, shape, rng, spread, facing):
    dx, dy = bx - ax, by - ay
    if shape == "manhattan":
        return abs(dx) + abs(dy) <= rng
    if shape == "chebyshev":
        return max(abs(dx), abs(dy)) <= rng
    if shape == "directional":
        fdx, fdy = facing
        dot = dx * fdx + dy * fdy
        cross = abs(dx * fdy - dy * fdx)
        max_side = spread if spread is not None else 0
        return 0 < dot <= rng and cross <= max_side
    return False


# ============================================================
# PLAYER
# ============================================================

class Player:
    def __init__(self, name, health, controls, img=None,
                 active=True, position=None):
        self.name = name
        self.health = health
        self.max_health = health
        self.controls = controls
        self.img = img
        self.active = active
        self.x, self.y = position if position else (0, 0)
        self.facing = (1, 0)
        self.abilities = []
        self._ability_cooldowns = {}
        self._unlocked_timed = set()
        self._unlocked_kills = set()

    def is_alive(self):
        return self.active and self.health > 0

    def try_move(self, dx, dy, game_state):
        nx, ny = self.x + dx, self.y + dy
        if game_state.is_blocked(nx, ny):
            return
        if not (0 <= nx < game_state.grid_width and 0 <= ny < game_state.grid_height):
            return
        self.x, self.y = nx, ny
        if dx or dy:
            self.facing = (dx, dy)


# ============================================================
# MONSTER
# ============================================================

class MonsterInstance:
    def __init__(self, mtype, position):
        self.type_name = mtype.name
        self.mtype = mtype
        self.health = mtype.health
        self.x, self.y = position
        self.abilities = mtype.abilities
        self.movement = mtype.movement
        self.facing = (1, 0)
        self.alive = True

    def is_alive(self):
        return self.alive and self.health > 0

    def move(self, dx, dy):
        self.x += dx
        self.y += dy
        if dx or dy:
            self.facing = (dx, dy)

    def move_random(self):
        dirs = [(0, 1), (0, -1), (1, 0), (-1, 0)]
        random.shuffle(dirs)
        dx, dy = dirs[0]
        self.move(dx, dy)

    def move_towards(self, player_name, game_state):
        target = game_state.players.get(player_name)
        if not target or not target.is_alive():
            return
        dx = target.x - self.x
        dy = target.y - self.y
        if abs(dx) >= abs(dy):
            self.move(1 if dx > 0 else (-1 if dx < 0 else 0), 0)
        else:
            self.move(0, 1 if dy > 0 else (-1 if dy < 0 else 0))


class MonsterType:
    def __init__(self, name, health, movement, count, img=None, position=None):
        self.name = name
        self.health = health
        self.img = img
        self.movement = movement
        self.count = count
        self.default_position = position
        self.abilities = []
        self.instances = []


# ============================================================
# OBSTACLE
# ============================================================

class Obstacle:
    def __init__(self, name, x, y, width=1, height=1, img=None):
        self.name = name
        self.cells = {(x + dx, y + dy)
                      for dx in range(width) for dy in range(height)}
        self.img = img


# ============================================================
# GAME STATE
# ============================================================

class GameState:
    def __init__(self, grid_width=20, grid_height=20):
        self.time_elapsed = 0.0
        self.players_killed = 0
        self.players_alive = 0
        self._monsters_killed = {}
        self.players = {}
        self.monster_types = {}
        self.obstacles = []
        self.grid_width = grid_width
        self.grid_height = grid_height
        self._blocked_cells = set()
        self._pending_assigns = []

    def configure(self, players, monster_registry, obstacles,
                  grid_width, grid_height):
        self.time_elapsed = 0.0
        self.players_killed = 0
        self._monsters_killed = {}
        self.players = {p.name: p for p in players}
        self.players_alive = sum(1 for p in players if p.active)
        self.monster_types = monster_registry
        self.obstacles = obstacles
        self.grid_width = grid_width
        self.grid_height = grid_height
        self._blocked_cells = set()
        for o in obstacles:
            self._blocked_cells |= o.cells
        for mt in monster_registry.values():
            for _ in range(mt.count):
                pos = mt.default_position or self._random_free_cell()
                inst = MonsterInstance(mt, pos)
                mt.instances.append(inst)
        for target_name, ability_names in self._pending_assigns:
            self._apply_assign(target_name, ability_names)
        self._pending_assigns.clear()

    def is_blocked(self, x, y):
        return (x, y) in self._blocked_cells

    def monster_killed_count(self, name):
        return self._monsters_killed.get(name, 0)

    def monster_count(self, name):
        mt = self.monster_types.get(name)
        return len(mt.instances) if mt else 0

    def monster_health(self, name):
        mt = self.monster_types.get(name)
        if mt and mt.instances:
            return mt.instances[0].health
        return 0

    def player_health(self, name):
        p = self.players.get(name)
        return p.health if p else 0

    def spawn(self, name, position=None):
        if name in self.players:
            p = self.players[name]
            p.active = True
            if position:
                p.x, p.y = position
            self.players_alive = sum(1 for pl in self.players.values() if pl.is_alive())
        elif name in self.monster_types:
            mt = self.monster_types[name]
            pos = position or self._random_free_cell()
            inst = MonsterInstance(mt, pos)
            mt.instances.append(inst)

    def assign_abilities(self, target_name, ability_names):
        if not self.players and not self.monster_types:
            self._pending_assigns.append((target_name, ability_names))
        else:
            self._apply_assign(target_name, ability_names)

    def _apply_assign(self, target_name, ability_names):
        abilities = [ability_registry[n] for n in ability_names]
        if target_name in self.players:
            self.players[target_name].abilities = abilities
        elif target_name in self.monster_types:
            mt = self.monster_types[target_name]
            mt.abilities = abilities
            for inst in mt.instances:
                inst.abilities = abilities

    def wait(self, seconds):
        self.time_elapsed += seconds

    def on_monster_death(self, name, x=None, y=None):
        self._monsters_killed[name] = self._monsters_killed.get(name, 0) + 1
        if x is not None and y is not None:
            _vfx.add_kill_burst(x, y)

    def on_player_death(self, name, x=None, y=None):
        self.players_killed += 1
        self.players_alive = sum(1 for p in self.players.values() if p.is_alive())
        if x is not None and y is not None:
            _vfx.add_player_death_burst(x, y)
        if self.players_alive == 0:
            _vfx.add_banner("GAME OVER", bg=(120, 0, 0), fg=(255, 80, 80))

    def _random_free_cell(self):
        attempts = 0
        while attempts < 500:
            x = random.randint(0, self.grid_width - 1)
            y = random.randint(0, self.grid_height - 1)
            if (x, y) not in self._blocked_cells:
                return (x, y)
            attempts += 1
        return (0, 0)


ability_registry = {}


# ============================================================
# PER-TICK SIMULATION
# ============================================================

def step_monster_simple_movement(game_state, dt):
    for mt in game_state.monster_types.values():
        if callable(mt.movement):
            continue
        for inst in list(mt.instances):
            if not inst.is_alive():
                continue
            if mt.movement == "random":
                inst.move_random()
            elif isinstance(mt.movement, str) and mt.movement.startswith("towards:"):
                pname = mt.movement.split(":", 1)[1]
                inst.move_towards(pname, game_state)


def step_ability_damage(game_state, dt):
    # Player active/timed/kill_unlocked abilities are KEY-TRIGGERED only.
    # They are fired in fire_player_ability() via handle_keydown(), NOT here.
    # This function only handles monster AUTO abilities (key == "AUTO").

    # Monster AUTO abilities attacking players
    for mt in game_state.monster_types.values():
        for inst in list(mt.instances):
            if not inst.is_alive():
                continue
            for ab in inst.abilities:
                for p in game_state.players.values():
                    if not p.is_alive():
                        continue
                    if in_range(inst.x, inst.y, p.x, p.y,
                                ab.shape, ab.range or 0, ab.spread, inst.facing):
                        p.health -= (ab.damage or 0) * dt
                        if p.health <= 0:
                            p.active = False
                            game_state.on_player_death(p.name, p.x, p.y)


def step_unlocks(game_state):
    for p in game_state.players.values():
        for ab in p.abilities:
            if ab.type == "timed" and ab.name not in p._unlocked_timed:
                if game_state.time_elapsed >= (ab.activates_at or 0):
                    p._unlocked_timed.add(ab.name)
            if ab.type == "kill_unlocked" and ab.name not in p._unlocked_kills:
                total_kills = sum(game_state._monsters_killed.values())
                if total_kills >= (ab.required_kills or 0):
                    p._unlocked_kills.add(ab.name)


def run_tick(game_state, dt):
    step_monster_simple_movement(game_state, dt)
    step_ability_damage(game_state, dt)
    step_unlocks(game_state)
    game_state.time_elapsed += dt


# ============================================================
# HEALTH BAR RENDERING
# ============================================================

def draw_health_bar(screen, x, y, current, maximum, bar_w=CELL_SIZE - 4, bar_h=4):
    """Draw a health bar above a cell at grid coords (x, y)."""
    if maximum <= 0:
        return
    ratio = max(0.0, min(1.0, current / maximum))
    px = x * CELL_SIZE + 2
    py = y * CELL_SIZE - bar_h - 2

    # Background (dark red)
    pygame.draw.rect(screen, (100, 20, 20), (px, py, bar_w, bar_h))

    # Foreground: green -> yellow -> red depending on ratio
    if ratio > 0.6:
        color = (40, 200, 40)
    elif ratio > 0.3:
        color = (220, 180, 20)
    else:
        color = (220, 40, 40)

    filled_w = int(bar_w * ratio)
    if filled_w > 0:
        pygame.draw.rect(screen, color, (px, py, filled_w, bar_h))

    # Thin border
    pygame.draw.rect(screen, (200, 200, 200), (px, py, bar_w, bar_h), 1)


# ============================================================
# ATTACK DIRECTION INDICATOR
# ============================================================

def draw_facing_arrow(screen, px, py, facing, color=(255, 255, 100)):
    """Draw a small triangle on the player cell indicating facing direction."""
    cx = px * CELL_SIZE + CELL_SIZE // 2
    cy = py * CELL_SIZE + CELL_SIZE // 2
    dx, dy = facing
    half = CELL_SIZE // 2 - 4
    tip = (cx + dx * half, cy + dy * half)
    perp = (-dy, dx)
    base_l = (cx - dx * 4 + perp[0] * 5, cy - dy * 4 + perp[1] * 5)
    base_r = (cx - dx * 4 - perp[0] * 5, cy - dy * 4 - perp[1] * 5)
    pygame.draw.polygon(screen, color, [tip, base_l, base_r])


# ============================================================
# MAIN GAME LOOP
# ============================================================

def run_game(players, monster_registry, obstacles, check_win_condition,
             grid_width, grid_height, duration, game_state=None,
             script=None, headless=False):
    if game_state is None:
        game_state = GameState()
    game_state.configure(players, monster_registry, obstacles,
                          grid_width, grid_height)

    script_gen = script() if script is not None else None
    script_wait_until = 0.0
    if script_gen is not None:
        try:
            wait_secs = next(script_gen)
            script_wait_until = game_state.time_elapsed + wait_secs
        except StopIteration:
            script_gen = None

    def advance_script():
        nonlocal script_gen, script_wait_until
        if script_gen is None:
            return
        if game_state.time_elapsed < script_wait_until:
            return
        try:
            wait_secs = next(script_gen)
            script_wait_until = game_state.time_elapsed + wait_secs
        except StopIteration:
            script_gen = None

    # ---- HEADLESS (CI / test) path ----
    if headless or pygame is None:
        dt = 1.0 / TICK_RATE
        max_ticks = int(duration * TICK_RATE) + 1
        tick = 0
        while game_state.time_elapsed < duration and tick < max_ticks:
            run_tick(game_state, dt)
            advance_script()
            tick += 1
            if check_win_condition():
                print(f"WIN condition met at t={game_state.time_elapsed:.2f}s "
                      f"(tick {tick})")
                return game_state
        print(f"Game ended (time limit reached) at t={game_state.time_elapsed:.2f}s")
        return game_state

    # ---- PYGAME path ----
    pygame.init()
    screen = pygame.display.set_mode(
        (grid_width * CELL_SIZE, grid_height * CELL_SIZE))
    pygame.display.set_caption("GameDSL")
    clock = pygame.time.Clock()

    # Fonts
    font_large = pygame.font.SysFont(None, max(36, CELL_SIZE * 2))
    font_small = pygame.font.SysFont(None, 16)

    images = {}

    def load_img(path):
        if not path:
            return None
        if path not in images:
            try:
                images[path] = pygame.image.load(path).convert_alpha()
            except Exception:
                images[path] = None
        return images[path]

    # Reset VFX state at game start
    global _vfx
    _vfx = VFXManager()

    win_triggered = False
    running = True

    while running and game_state.time_elapsed < duration:
        dt = clock.tick(TICK_RATE) / 1000.0

        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                running = False
            elif event.type == pygame.KEYDOWN:
                handle_keydown(event.key, game_state)

        run_tick(game_state, dt)
        advance_script()
        _vfx.update(dt)

        if not win_triggered and game_state.time_elapsed > 0 and check_win_condition():
            win_triggered = True
            _vfx.add_banner("YOU WIN!", bg=(0, 80, 0), fg=(80, 255, 80))
            print(f"WIN condition met at t={game_state.time_elapsed:.2f}s")

        # ---- DRAW ----
        screen.fill((18, 18, 22))

        # Obstacles
        for o in obstacles:
            for (cx, cy) in o.cells:
                rect = (cx * CELL_SIZE, cy * CELL_SIZE, CELL_SIZE, CELL_SIZE)
                img = load_img(o.img)
                if img:
                    screen.blit(pygame.transform.scale(img, (CELL_SIZE, CELL_SIZE)), rect[:2])
                else:
                    pygame.draw.rect(screen, (120, 118, 110), rect)

        # Monsters
        for mt in game_state.monster_types.values():
            for inst in mt.instances:
                if not inst.is_alive():
                    continue
                rect = (inst.x * CELL_SIZE, inst.y * CELL_SIZE, CELL_SIZE, CELL_SIZE)
                img = load_img(mt.img)
                if img:
                    screen.blit(pygame.transform.scale(img, (CELL_SIZE, CELL_SIZE)), rect[:2])
                else:
                    pygame.draw.rect(screen, (200, 70, 70), rect)
                    # Draw a small skull marker
                    skull = font_small.render("☠", True, (255, 180, 180))
                    screen.blit(skull, (rect[0] + 2, rect[1] + 2))
                draw_health_bar(screen, inst.x, inst.y, inst.health, inst.mtype.health)

        # Players
        for p in game_state.players.values():
            if not p.is_alive():
                continue
            rect = (p.x * CELL_SIZE, p.y * CELL_SIZE, CELL_SIZE, CELL_SIZE)
            img = load_img(p.img)
            if img:
                screen.blit(pygame.transform.scale(img, (CELL_SIZE, CELL_SIZE)), rect[:2])
            else:
                pygame.draw.rect(screen, (90, 80, 200), rect)
                label = font_small.render(p.name[0].upper(), True, (200, 220, 255))
                screen.blit(label, (rect[0] + CELL_SIZE // 2 - label.get_width() // 2,
                                    rect[1] + CELL_SIZE // 2 - label.get_height() // 2))
            draw_facing_arrow(screen, p.x, p.y, p.facing)
            draw_health_bar(screen, p.x, p.y, p.health, p.max_health)

        # VFX world layer (flashes + particles)
        _vfx.draw_world_layer(screen)

        # HUD: time + kills in top-left
        kills = sum(game_state._monsters_killed.values())
        hud_text = f"t={game_state.time_elapsed:.1f}s  kills={kills}"
        hud = font_small.render(hud_text, True, (180, 180, 180))
        screen.blit(hud, (4, 4))

        # VFX UI layer (banners)
        _vfx.draw_ui_layer(screen, font_large)

        pygame.display.flip()

        # Keep running 1.5 s after win so the banner is visible
        if win_triggered and _vfx.banners:
            banner = _vfx.banners[-1]
            if banner.age > 1.5:
                running = False

    pygame.quit()
    return game_state


# ============================================================
# INPUT HANDLING
# ============================================================

def handle_keydown(pg_key, game_state):
    if pygame is None:
        return
    key_name = pygame.key.name(pg_key).upper()

    for p in game_state.players.values():
        if not p.is_alive():
            continue
        if key_name == p.controls.get("up"):
            p.try_move(0, -1, game_state)
        elif key_name == p.controls.get("down"):
            p.try_move(0, 1, game_state)
        elif key_name == p.controls.get("left"):
            p.try_move(-1, 0, game_state)
        elif key_name == p.controls.get("right"):
            p.try_move(1, 0, game_state)

        for ab in p.abilities:
            if ab.key != key_name:
                continue
            if ab.type == "active":
                fire_player_ability(p, ab, game_state)
            elif ab.type == "timed" and ab.name in p._unlocked_timed:
                fire_player_ability(p, ab, game_state)
            elif ab.type == "kill_unlocked" and ab.name in p._unlocked_kills:
                fire_player_ability(p, ab, game_state)


def fire_player_ability(player, ability, game_state):
    """Fire an ability for a player: deal damage to monsters AND other players, spawn VFX."""
    hit_any = False
    multiplier = ability.damage_multiplier or 1
    dmg = (ability.damage or 0) * multiplier

    # --- Hit monsters ---
    for mt in game_state.monster_types.values():
        for inst in list(mt.instances):
            if not inst.is_alive():
                continue
            if in_range(player.x, player.y, inst.x, inst.y,
                        ability.shape, ability.range or 0,
                        ability.spread, player.facing):
                inst.health -= dmg
                _vfx.add_attack_flash(inst.x, inst.y, color=(255, 200, 40))
                hit_any = True
                if inst.health <= 0:
                    inst.alive = False
                    mt.instances.remove(inst)
                    game_state.on_monster_death(mt.name, inst.x, inst.y)

    # --- Hit other players (PvP) ---
    for target in game_state.players.values():
        if target is player or not target.is_alive():
            continue
        if in_range(player.x, player.y, target.x, target.y,
                    ability.shape, ability.range or 0,
                    ability.spread, player.facing):
            target.health -= dmg
            _vfx.add_attack_flash(target.x, target.y, color=(255, 80, 80))
            hit_any = True
            if target.health <= 0:
                target.active = False
                game_state.on_player_death(target.name, target.x, target.y)

    # If nothing was in range, flash the cell directly in front as feedback
    if not hit_any:
        fx, fy = player.x + player.facing[0], player.y + player.facing[1]
        _vfx.add_attack_flash(fx, fy, color=(120, 120, 255))