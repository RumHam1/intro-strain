"""Compute defensive line technique based on player alignment.

The script defines helpers to estimate the defensive technique number for a
player relative to offensive line positions. Techniques follow a common
football numbering system:

0  - head up on the center
1  - shade between the center and guard
2  - head up on the guard
3  - outside shade of the guard
4  - head up on the tackle
5  - outside shade of the tackle
6  - inside shade of the tight end
7  - head up on the tight end
8  - outside shade of the tight end
9  - very wide outside of the tight end

If an offense does not deploy a tight end, technique 6/7/8/9 are measured
relative to where a tight end would align one gap outside the tackle.
"""

from dataclasses import dataclass
from typing import Tuple, Optional

Coordinate = Tuple[float, float]

@dataclass
class OffensiveLine:
    C: Coordinate
    LG: Coordinate
    RG: Coordinate
    LT: Coordinate
    RT: Coordinate
    TE_L: Optional[Coordinate] = None
    TE_R: Optional[Coordinate] = None


def _default_te(tackle: Coordinate, guard: Coordinate, side: str) -> Coordinate:
    """Return a hypothetical tight end position if none is provided."""
    x, y_tackle = tackle
    gap = abs(y_tackle - guard[1])
    if side == "left":
        return (x, y_tackle - gap)
    else:
        return (x, y_tackle + gap)


def technique_for_player(player: Coordinate, ol: OffensiveLine) -> int:
    """Return the technique number for a defender at ``player``.

    Parameters
    ----------
    player : Coordinate
        (x, y) of the defensive player at the snap.
    ol : OffensiveLine
        Positions of the offensive linemen and optional tight ends.
    """
    x, y = player
    center_y = ol.C[1]

    # Determine which side of the formation the defender is on.
    if y < center_y:
        side = "left"
        guard_y = ol.LG[1]
        tackle_y = ol.LT[1]
        te = ol.TE_L or _default_te(ol.LT, ol.LG, side)
        te_y = te[1]
    else:
        side = "right"
        guard_y = ol.RG[1]
        tackle_y = ol.RT[1]
        te = ol.TE_R or _default_te(ol.RT, ol.RG, side)
        te_y = te[1]

    gap_cg = abs(guard_y - center_y)
    gap_gt = abs(tackle_y - guard_y)
    gap_tt = abs(te_y - tackle_y)
    tolerance = 0.3  # yards within "head up"

    def near(value: float, target: float) -> bool:
        return abs(value - target) <= tolerance

    if near(y, center_y):
        return 0
    if y < center_y:
        if y > center_y - gap_cg:
            return 1
    else:
        if y < center_y + gap_cg:
            return 1

    if near(y, guard_y):
        return 2
    if center_y < y < guard_y or guard_y < y < center_y:
        # inside shade of guard
        return 2

    if guard_y < y < tackle_y or tackle_y < y < guard_y:
        return 3
    if near(y, tackle_y):
        return 4

    if tackle_y < y < te_y or te_y < y < tackle_y:
        return 5
    if near(y, te_y):
        return 7

    # Outside the TE area
    if side == "left":
        if y > te_y:
            return 6
        elif te_y - gap_tt <= y <= te_y:
            return 8
        else:
            return 9
    else:
        if y < te_y:
            return 6
        elif te_y <= y <= te_y + gap_tt:
            return 8
        else:
            return 9



