diff --git a//dev/null b/scripts/defensive_line_diagrams.py
index 0000000000000000000000000000000000000000..2b0fa7ad8c6b88ea689a8365cf0dfc4c5167dd86 100644
--- a//dev/null
+++ b/scripts/defensive_line_diagrams.py
@@ -0,0 +1,108 @@
+import os
+import pandas as pd
+import matplotlib.pyplot as plt
+from defensive_line_technique import OffensiveLine, technique_for_player
+
+BALL_SNAP_FILE = os.path.join('data', 'ball_snap.csv.gz')
+PASS_RUSH_FILE = os.path.join('data', 'pass_rush.csv.gz')
+
+
+def load_play_data(num_plays: int = 5):
+    """Return a list of (OffensiveLine, defenders, label) for ``num_plays``."""
+    ball = pd.read_csv(BALL_SNAP_FILE, compression='gzip')
+    cols = [
+        'gameId', 'playId', 'team', 'x', 'y', 'event',
+        'defensiveTeam', 'officialPosition', 'displayName'
+    ]
+    rush = pd.read_csv(PASS_RUSH_FILE, compression='gzip', usecols=cols)
+
+    rush_snaps = rush[rush['event'] == 'ball_snap']
+    plays = rush_snaps[['gameId', 'playId', 'defensiveTeam']].drop_duplicates()
+
+    play_data = []
+    for _, row in plays.iterrows():
+        gid, pid, def_team = row['gameId'], row['playId'], row['defensiveTeam']
+        bs_play = ball[(ball['gameId'] == gid) & (ball['playId'] == pid)]
+        if bs_play.empty:
+            continue
+        offense = bs_play[bs_play['team'] != def_team]
+        defense = rush_snaps[(rush_snaps['gameId'] == gid) & (rush_snaps['playId'] == pid)]
+        if offense.empty or defense.empty:
+            continue
+
+        center = offense[offense['officialPosition'] == 'C']
+        guards = offense[offense['officialPosition'] == 'G']
+        tackles = offense[offense['officialPosition'] == 'T']
+        if center.empty or len(guards) < 2 or len(tackles) < 2:
+            continue
+        center = center.iloc[0]
+        center_y = center['y']
+        guards = guards.sort_values('y')
+        tackles = tackles.sort_values('y')
+        lg = guards.iloc[0]
+        rg = guards.iloc[-1]
+        lt = tackles.iloc[0]
+        rt = tackles.iloc[-1]
+        tes = offense[offense['officialPosition'] == 'TE']
+        te_left = tes[tes['y'] < center_y].sort_values('y').iloc[0] if not tes[tes['y'] < center_y].empty else None
+        te_right = tes[tes['y'] > center_y].sort_values('y').iloc[0] if not tes[tes['y'] > center_y].empty else None
+
+        ol = OffensiveLine(
+            C=(center['x'], center['y']),
+            LG=(lg['x'], lg['y']),
+            RG=(rg['x'], rg['y']),
+            LT=(lt['x'], lt['y']),
+            RT=(rt['x'], rt['y']),
+            TE_L=(te_left['x'], te_left['y']) if te_left is not None else None,
+            TE_R=(te_right['x'], te_right['y']) if te_right is not None else None,
+        )
+
+        defenders = defense[['x', 'y']].values.tolist()
+        label = f"{gid}_{pid}"
+        play_data.append((ol, defenders, label))
+        if len(play_data) >= num_plays:
+            break
+    return play_data
+
+
+def plot_play(ol: OffensiveLine, defenders, idx, label: str):
+    fig, ax = plt.subplots(figsize=(6, 3))
+
+    # offensive players
+    offense = [
+        (ol.C, 'C'), (ol.LG, 'LG'), (ol.RG, 'RG'),
+        (ol.LT, 'LT'), (ol.RT, 'RT')
+    ]
+    if ol.TE_L is not None:
+        offense.append((ol.TE_L, 'TE'))
+    if ol.TE_R is not None and ol.TE_R != ol.TE_L:
+        offense.append((ol.TE_R, 'TE'))
+    for (x, y), lab in offense:
+        ax.scatter(y, x, marker='s', color='blue')
+        ax.text(y, x + 0.15, lab, ha='center', va='bottom', fontsize=8, color='white', weight='bold')
+
+    # defenders
+    for x_d, y_d in defenders:
+        tech = technique_for_player((x_d, y_d), ol)
+        ax.scatter(y_d, x_d, marker='o', color='red')
+        ax.text(y_d, x_d - 0.15, str(tech), ha='center', va='top', fontsize=8, color='red')
+
+    center_x, center_y = ol.C
+    ax.set_xlim(center_y - 8, center_y + 8)
+    ax.set_ylim(center_x - 3, center_x + 3)
+    ax.set_xticks([])
+    ax.set_yticks([])
+    ax.set_title(f'Play {idx} ({label})')
+    ax.set_aspect('equal')
+    fig.tight_layout()
+    # Save diagrams inside the data directory so they are easy to locate
+    out_dir = os.path.join('data', 'figures')
+    os.makedirs(out_dir, exist_ok=True)
+    fig.savefig(os.path.join(out_dir, f'play_{idx}.png'))
+    plt.close(fig)
+
+
+if __name__ == '__main__':
+    plays = load_play_data(num_plays=5)
+    for i, (ol, defenders, label) in enumerate(plays, start=1):
+        plot_play(ol, defenders, i, label)
