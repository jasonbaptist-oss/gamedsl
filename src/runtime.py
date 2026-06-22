"""
runtime.py — Minimal GameDSL runtime.

The OCaml code generator (codegen.ml) emits a Python file that does:

    from runtime import (
        GameState, Player, MonsterType, Ability, Obstacle, run_game
    )

This module provides those names. It is intentionally minimal —
enough to actually run a generated file end to end with pygame,
demonstrating the full pipeline (.gdsl -> tokens -> AST -> checked
AST -> generated .py -> running game), without claiming to be a
complete, production game engine.
"""

import time
import random
import sys

try:
    import pygame
except ImportError:
    pygame = None  # allows --headless runs without pygame installed

CELL_SIZE = 32


class Ability:
    def __init__(self, name, type, img=None, **fields):
        self.name = name
        self.type = type
        self.img = img
        for k, v in fields.items():
            setattr(self, k, v)
        # sane defaults for fields not provided
        for attr in ("damage", "range", "spread", "required_kills",
                     "damage_multiplier", "damage_reduction",
                     "health_regen", "speed_boost", "activates_at"):
            if not hasattr(self, attr):
                setattr(self, attr, None)
        if not hasattr(self, "shape"):
            self.shape = "manhattan"


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

    def move(self, dx, dy):
        self.x += dx
        self.y += dy
        if dx or dy:
            self.facing = (dx, dy)


class MonsterInstance:
    """A single living monster, spawned from a MonsterType."""
    def __init__(self, mtype, position):
        self.type_name = mtype.name
        self.health = mtype.health
        self.x, self.y = position
        self.abilities = []
        self.movement = mtype.movement


class MonsterType:
    def __init__(self, name, health, movement, count, img=None, position=None):
        self.name = name
        self.health = health
        self.img = img
        self.movement = movement   # "random" | "stationary" | "towards:X" | callable
        self.count = count
        self.default_position = position
        self.abilities = []   # filled in by assign_abilities
        self.instances = []


class Obstacle:
    def __init__(self, name, x, y, width=1, height=1, img=None):
        self.name = name
        self.cells = {(x + dx, y + dy)
                      for dx in range(width) for dy in range(height)}
        self.img = img


class GameState:
    def __init__(self):
        self.time_elapsed = 0.0
        self.players_killed = 0
        self.players_alive = 0
        self._monsters_killed = {}   # name -> cumulative count
        self.players = {}
        self.monster_types = {}
        self.obstacles = []

    # ---- runtime variable accessors (called by generated code) ----
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

    # ---- mutating operations (called by generated code) ----
    def spawn(self, name, position=None):
        if name in self.players:
            p = self.players[name]
            p.active = True
            if position:
                p.x, p.y = position
            self.players_alive += 1
        elif name in self.monster_types:
            mt = self.monster_types[name]
            pos = position or self._random_free_cell()
            inst = MonsterInstance(mt, pos)
            inst.abilities = mt.abilities
            mt.instances.append(inst)

    def assign_abilities(self, target_name, ability_names):
        abilities = [ability_registry[n] for n in ability_names]
        if target_name in self.players:
            self.players[target_name].abilities = abilities
        elif target_name in self.monster_types:
            self.monster_types[target_name].abilities = abilities

    def wait(self, seconds):
        # in the headless test runner this just advances the clock;
        # in the pygame loop it is handled by run_game's own timing
        self.time_elapsed += seconds

    def on_monster_death(self, name):
        self._monsters_killed[name] = self._monsters_killed.get(name, 0) + 1

    def on_player_death(self):
        self.players_killed += 1
        self.players_alive -= 1

    def _random_free_cell(self):
        blocked = set()
        for o in self.obstacles:
            blocked |= o.cells
        while True:
            x = random.randint(0, GRID_WIDTH - 1) if "GRID_WIDTH" in globals() else 0
            y = random.randint(0, GRID_HEIGHT - 1) if "GRID_HEIGHT" in globals() else 0
            if (x, y) not in blocked:
                return (x, y)


# global registries the generated code populates directly
ability_registry = {}


def run_game(players, monster_registry, obstacles, check_win_condition,
             grid_width, grid_height, duration, headless=False):
    """
    Minimal game loop. With headless=True (or pygame unavailable),
    runs a fast simulation loop with no rendering — useful for CI
    and for the test suite to exercise generated code without a
    display. With pygame available and headless=False, opens a
    window and renders the grid each frame.
    """
    game_state = GameState()
    game_state.players = {p.name: p for p in players}
    game_state.players_alive = sum(1 for p in players if p.active)
    game_state.monster_types = monster_registry
    game_state.obstacles = obstacles

    if headless or pygame is None:
        tick = 0
        while game_state.time_elapsed < duration and tick < 100000:
            game_state.time_elapsed += 1.0 / 60.0
            tick += 1
            if check_win_condition():
                print(f"WIN condition met at t={game_state.time_elapsed:.2f}s")
                return
        print(f"Game ended (time limit) at t={game_state.time_elapsed:.2f}s")
        return

    pygame.init()
    screen = pygame.display.set_mode(
        (grid_width * CELL_SIZE, grid_height * CELL_SIZE))
    clock = pygame.time.Clock()
    running = True
    while running and game_state.time_elapsed < duration:
        dt = clock.tick(60) / 1000.0
        game_state.time_elapsed += dt
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                running = False
        screen.fill((20, 20, 25))
        for p in players:
            if p.active:
                pygame.draw.rect(
                    screen, (90, 80, 200),
                    (p.x * CELL_SIZE, p.y * CELL_SIZE, CELL_SIZE, CELL_SIZE))
        for mt in monster_registry.values():
            for inst in mt.instances:
                pygame.draw.rect(
                    screen, (200, 70, 70),
                    (inst.x * CELL_SIZE, inst.y * CELL_SIZE, CELL_SIZE, CELL_SIZE))
        pygame.display.flip()
        if check_win_condition():
            print(f"WIN condition met at t={game_state.time_elapsed:.2f}s")
            running = False
    pygame.quit()