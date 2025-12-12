## 🚀 Quick Start
> **⚠️ Performance Note**: The default `pddl/pddl_solver.py` is implemented using `pyperplan` to strictly adhere to course's assignment requirements (Just to demonstrate that we have fully mastered the PDDL intelligent planning format and have a clear understanding of the usage of the Pyperplan library in Python), which causes significant latency. For optimal performance, we highly recommend replacing it with the contents of `solver_without_pyperplan_record`. This alternative uses the identical algorithm but eliminates the external dependency, achieving millisecond-level response times.
1. Start Prolog: swipl
2. Load main file: ['Breakout.pl']
3. Start game: play.

## 🎮 Controls

- **W.**: Move Up
- **A.**: Move Left
- **S.**: Move Down
- **D.**: Move Right


## 📂 Project Architecture

This project adopts a strict modular design, completely decoupling the game engine, enemy behavior, combat system, item system, and map data. Below are the detailed design descriptions for each file:

### 🏗️ Core Engine
**1. `main.pl` (Entry Point)**
- **Function**: The bootloader of the program.
- **Design**: Responsible only for loading `game_engine.pl` and providing the global startup predicate `play/0`, keeping the entry point extremely minimal.

**2. `game_engine.pl` (Game Engine)**
- **Function**: The central nervous system of the game, integrating combat, items, map, and rendering modules.
- **Design Details**:
  - **State Management**: Maintains dynamic predicates such as player position, HP, Attack (ATK), Score, Turn count, and enemy enhancement levels.
  - **Input Loop**: Real-time listening for keyboard input (WASD) and mapping inputs to game actions.
  - **Command Mode**: Supports pressing `t` to enter command mode, allowing execution of debug commands like `tp`.
  - **Physics & Collision**: Handles movement requests, detecting collisions with walls, boundaries, and events (like exits).
  - **UI Rendering**: Draws the map on the console, dynamically displaying current HP, ATK, and Score at the top. Optimized rendering order to ensure the map refreshes after enemy movement to show the latest enemy positions.
  - **Logic Scheduling**: After each player action, sequentially triggers item detection, combat resolution, event checks (like victory conditions), and dynamic difficulty checks.
  - **Modular Calls**: Integrates `check_exit` logic, triggering settlement when the player reaches the exit.
  - **Debug Commands**: Added `dehealthy(X)` command for testing HP deduction mechanisms.

### ⚔️ Combat & Numerical System
**3. `combat_logic.pl` (Combat Logic)**
- **Function**: Encapsulates all combat resolution, damage calculation, and loot drop logic.
- **Mechanisms**:
  - **8-Neighbor Detection**: Automatically detects enemies within the surrounding 8 grids of the player.
  - **Numerical Confrontation**: If `Player ATK >= Enemy ATK`, the enemy is eliminated; otherwise, the player loses HP and the enemy is stunned.
  - **Loot Drop**: Defeating specific enemies has a probability of dropping treasures:
    - **Hidden Bee (B)**: Double Drop — Guaranteed Diamond (O) at [26, 12] + Random original drop (5% Diamond, 35% Gold, 60% Silver).
    - **BFS Chaser (C)**: Double Drop — Guaranteed Diamond (O) at [30, 20] + Random original drop (5% Diamond, 35% Gold, 60% Silver).
    - **Timid Watched (T)**: Guaranteed Diamond (O) at [34, 12] (No random drop, maintains ATK absorption setting).
    - **Random Walker (R)**: Random drop (1% Diamond, 29% Gold, 70% Silver).

**4. `scaling_manager.pl` (Dynamic Difficulty Management)**
- **Function**: Controls the increase in game difficulty over time.
- **Mechanisms**: Every **10 turns**, the ATK of all enemies on the map automatically increases by **10%**.

### 🤖 Enemy System (enemies/)
The enemy system is designed as a plugin structure, unified and scheduled by a manager.

**5. `ai_manager.pl` (AI Manager)**
- **Function**: Unifies scheduling of `init_enemies` and `enemies_tick`, responsible for distributing initialization and action instructions for all enemies.

**6. `ai_utils.pl` (AI Utilities)**
- **Function**: Provides general grid calculation, collision detection, and boundary check predicates.

**7. `bfs_chaser.pl` (Smart Chaser - Code 'C')**
- **Behavior**: Uses BFS algorithm to find the shortest path to chase the player after awakening. Probabilistic loot drop upon defeat.

**8. `random_walker.pl` (Patroller - Code 'R')**
- **Behavior**: Patrols randomly within a restricted area. Probabilistic loot drop upon defeat.

**9. `hidden_bee.pl` (Original static_boss.pl - Code 'B')**
- **Behavior**: Periodically releases spikes (^) moving right to attack the player. Probabilistic loot drop upon defeat.

**10. `timid_watched.pl` (Mid-Boss - Code 'T')**
- **Behavior**: Wanders in an area; upon defeat, the player can absorb its attack power.

**11. `smart_thief.pl` (Smart Thief - Code 'I')**
- **Behavior**: Uses a PDDL planner for intelligent pathfinding, prioritizing finding and stealing treasures on the map.
- **Integration**: Invokes an external Python script via `process_create` to solve PDDL problems.

### 🧠 PDDL Planning Module (pddl/)
This module implements a simple STRIPS planner integration for controlling the behavior of the `Smart Thief`.

**12. `domain.pddl`**
- **Function**: Defines the Grid World domain, including `location` type, `at`, `connected` predicates, and `move` action.

**13. `pddl_solver.py`**
- **Function**: Python adapter for PDDL solver using pyperplan.
- **Algorithm**: Implemented based on the `pyperplan` 2.1 library, responsible for parsing PDDL files, instantiating (Grounding) the problem, and using the A* algorithm (hFF heuristic) to find the optimal path, finally outputting `(move from to)` actions.
**Extremely slow, designed to meet assignment requirements. It is recommended to replace it with the PDDL solver below that does not call pyperplan (solver_without_pyperplan_record), just copy and replace, same algorithm, millisecond response**.

### 🎒 Item & Equipment System (items/)
**14. `items/item_manager.pl` (Item Manager)**
- **Function**: Responsible for randomly generating equipment and treasures at the start of the game, and handling player pickup logic.
- **Mechanisms**: 
  - **Spawn Strategy**: 
    - **Healthy Package (H)**: Spawned first, based on `health_spawn_area` configuration.
    - **Equipment (S/K)**: Spawned subsequently, based on `equipment_spawn_area` configuration, automatically avoiding healthy packages and enemies.
    - **Treasure (O/G/V)**: Spawned last, avoiding all existing entities, and will not spawn within the Boss room restricted area `[24,14]-[36,22]`.
  - **Pickup Logic**: Triggered when the player overlaps; equipment increases ATK, treasures increase Score.

**15. `items/equipments/sword.pl` (Sword - Code 'S')**
- **Effect**: Player ATK +10 upon pickup.

**16. `items/equipments/knife.pl` (Knife - Code 'K')**
- **Effect**: Player ATK +5 upon pickup.

**17. `items/tools/heathy_package.pl` (Healthy Package - Code 'H')**
- **Effect**: Immediately restores 40 HP upon pickup.

**18. `items/treasures/diamond.pl` (Diamond - Code 'O')**
- **Effect**: Score +1000 upon pickup.

**19. `items/treasures/gold.pl` (Gold - Code 'G')**
- **Effect**: Score +500 upon pickup.

**20. `items/treasures/silver.pl` (Silver - Code 'V')**
- **Effect**: Score +200 upon pickup.

### 🗺️ Map Configuration (maps/)
**21. `level1.pl` (Level Configuration)**
- **Function**: Pure data file, defining map boundaries, wall layout, spawn point, exit location, and special areas.
- **Configuration Items**:
  - `map_segment`: Wall segments.
  - `health_spawn_area`: Random spawn area for healthy packages.
  - `equipment_spawn_area`: Random spawn area for equipment.

## 📊 Game Stats & Config Reference

For convenient debugging and balance adjustment, the following lists all core game values and their definition locations in the source code.

### 🧑 Player Initial Stats
- **Initial HP**: 100
  - 📍 `game_engine.pl:346` (`assert(health(100))`)
- **Initial ATK**: 15
  - 📍 `game_engine.pl:347` (`assert(player_atk(15))`)

### 🎒 Item Stats
- **Equipment**
  - **Sword (S)**: ATK +10
    - 📍 `items/equipments/sword.pl:5` (`player_atk(CurrentAtk), NewAtk is CurrentAtk + 10`)
  - **Knife (K)**: ATK +5
    - 📍 `items/equipments/knife.pl:5` (`player_atk(CurrentAtk), NewAtk is CurrentAtk + 5`)
- **Consumables**
  - **Healthy Package (H)**: HP +40
    - 📍 `items/tools/heathy_package.pl:7` (`increase_health(40)`)
- **Treasure (Score)**
  - **Diamond (O)**: +1000 Score
    - 📍 `items/treasures/diamond.pl:1` (`treasure_value(diamond, 1000, 'Diamond')`)
  - **Gold (G)**: +500 Score
    - 📍 `items/treasures/gold.pl:1` (`treasure_value(gold, 500, 'Gold')`)
  - **Silver (V)**: +200 Score
    - 📍 `items/treasures/silver.pl:1` (`treasure_value(silver, 200, 'Silver')`)

### 🤖 Enemy Stats
| Enemy Code | Name | Initial ATK | Initial Stun | Definition File |
| :--- | :--- | :--- | :--- | :--- |
| **C** | BFS Chaser | 15 | 0 | `enemies/bfs_chaser.pl:12` |
| **R** | Random Walker | 10 | 0 | `enemies/random_walker.pl:10` |
| **B** | Hidden Bee | 20 | 0 | `enemies/hidden_bee.pl:10` |
| **T** | Timid Watched | 100 | 0 | `enemies/timid_watched.pl:10` |
| **I** | Smart Thief | 15 (Cannot trigger combat) | 0 | `enemies/smart_thief.pl:18` |

### ⚙️ Game Mechanic Stats
- **Dynamic Difficulty (Scaling)**
  - **Trigger Cycle**: Every 10 turns
  - **Increase Amount**: Enemy ATK +10%
  - 📍 `scaling_manager.pl:5-12` (`0 is Turn mod 10`, `NewAtk is round(Atk * 1.1)`)
- **Spike Damage**
  - **Damage Value**: 5 HP
  - 📍 `game_engine.pl:390` (`decrease_health(5)`)
