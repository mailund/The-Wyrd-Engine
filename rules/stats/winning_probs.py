import itertools
from fractions import Fraction

# Possible skill values
skills = [0, 1, 2]
combos = [(A, D) for A in skills for D in skills]

# Pre-compute Fudge 4dF distribution: sum of 4 dice each in {-1,0,+1}
fudge_outcomes = [sum(outcome) for outcome in itertools.product([-1, 0, 1], repeat=4)]
total = len(fudge_outcomes)  # 3^4 = 81 outcomes
fudge_dist = {}
for result in fudge_outcomes:
    fudge_dist[result] = fudge_dist.get(result, 0) + 1
# Convert counts to probabilities (as Fraction for exactness)
for result in fudge_dist:
    fudge_dist[result] = Fraction(fudge_dist[result], total)
# fudge_dist now maps sums -4..4 to probabilities (e.g., P(sum=0)=19/81, etc.)


def damage_distribution(skill_diff):
    """Return probability distribution of damage dealt given attacker skill_diff = attack - defense."""
    dist = {}
    for fudge_sum, p in fudge_dist.items():
        margin = skill_diff + fudge_sum
        dmg = margin if margin > 0 else 0  # only positive margin yields damage
        dist[dmg] = dist.get(dmg, Fraction(0, 1)) + p
    return dist


# Pre-compute damage distributions for all relevant differences
diff_values = [-3, -2, -1, 0, 1, 2, 3]
damage_dist = {d: damage_distribution(d) for d in diff_values}


# Function to solve P1 win probability for given skills
def compute_p1_win_prob(P1_attack, P1_defense, P2_attack, P2_defense):
    d1 = P1_attack - P2_defense  # P1's attack skill difference
    d2 = P2_attack - P1_defense  # P2's attack skill difference

    # Handle cases where one or both players can never deal damage
    if d1 <= -4 and d2 <= -4:
        return 0.0  # Neither can ever damage the other
    if d1 <= -4 < d2:
        return 0.0  # P1 can’t deal damage, P2 can -> P1 never wins
    if d2 <= -4 < d1:
        return 1.0  # P2 can’t deal damage, P1 can -> P1 always wins

    # Use linear system solving for absorbing Markov chain
    # State space: (p1_damage, p2_damage, turn) with p1_damage, p2_damage in 0..6
    states = []
    state_index = {}
    for p1_d in range(7):
        for p2_d in range(7):
            if p1_d < 7 and p2_d < 7:  # (both <7 to be non-terminal)
                # Two turn phases: 1 for P1's attack, 2 for P2's attack
                states.append((p1_d, p2_d, 1))
                states.append((p1_d, p2_d, 2))
    state_index = {state: idx for idx, state in enumerate(states)}
    N = len(states)  # number of transient states

    # Initialize linear system: A * x = b
    # We'll use Fractions for exact arithmetic
    A = [[Fraction(0) for _ in range(N)] for __ in range(N)]
    b = [Fraction(0) for _ in range(N)]

    for state, idx in state_index.items():
        p1_d, p2_d, turn = state
        A[idx][idx] = Fraction(1)  # left-hand side P(state)
        if turn == 1:  # P1's attack turn
            for dmg, prob in damage_dist[d1].items():
                if p2_d + dmg >= 7:
                    # P2 would reach 7+ damage -> P1 wins
                    b[idx] += prob * 1  # contributes 1 * prob to RHS
                else:
                    # Transition to P2's turn state
                    next_state = (p1_d, p2_d + dmg, 2)
                    j = state_index[next_state]
                    A[idx][j] -= prob  # move transition term to LHS
        else:  # turn == 2, P2's attack
            for dmg, prob in damage_dist[d2].items():
                if p1_d + dmg >= 7:
                    # P1 reaches 7 -> P1 loses (0 contribution to win prob)
                    # (No addition to b since contributes 0)
                    continue
                else:
                    # Transition to P1's turn
                    next_state = (p1_d + dmg, p2_d, 1)
                    j = state_index[next_state]
                    A[idx][j] -= prob

    # Solve the linear system A x = b for x (P1 win probabilities for each state)
    # We only need x for the initial state (0,0,P1_turn)
    # Use Gaussian elimination (since N is moderate)
    x = [Fraction(0) for _ in range(N)]
    # Convert A to row echelon form and apply to b
    for i in range(N):
        # Find pivot
        if A[i][i] == 0:
            # swap with a row below that has non-zero in column i
            for k in range(i + 1, N):
                if A[k][i] != 0:
                    A[i], A[k] = A[k], A[i]
                    b[i], b[k] = b[k], b[i]
                    break
        pivot = A[i][i]
        if pivot == 0:
            continue  # (should not happen for a properly absorbing chain)
        # Normalize pivot row
        for j in range(i, N):
            A[i][j] /= pivot
        b[i] /= pivot
        # Eliminate column i in other rows
        for r in range(N):
            if r != i and A[r][i] != 0:
                factor = A[r][i]
                for c in range(i, N):
                    A[r][c] -= factor * A[i][c]
                b[r] -= factor * b[i]
    # After elimination, the system is solved; b contains the solution.
    x = b  # (each b[i] is now P1 win probability for state i)
    return float(
        x[state_index[(0, 0, 1)]]
    )  # probability that P1 wins from (0,0) with P1 to move


# Compute and print the probability matrix
prob_matrix = [
    [compute_p1_win_prob(P1A, P1D, P2A, P2D) for (P2A, P2D) in combos]
    for (P1A, P1D) in combos
]
print(r"\begin{DndTable}{l" + "r" * len(combos) + "}")
print(
    "&",
    "&".join(f" \\textbf{{P2({A},{D})}} " for A, D in combos),
    r"\\",
)
for i, (P1A, P1D) in enumerate(combos):
    row = [f"{100*prob_matrix[i][j]:>5.1f}\\%" for j in range(len(combos))]
    print(f"\\textbf{{P1({P1A},{P1D}):}} &", " & ".join(row), r"\\")
print(r"\end{DndTable}")
