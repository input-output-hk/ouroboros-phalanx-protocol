# Re-import necessary libraries after execution state reset
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from decimal import Decimal, getcontext
import ace_tools as tools

# Set high precision for calculations
getcontext().prec = 50

# Define function for computing N_CPU using high precision calculations
def compute_N_CPU_high_precision(rho, T_eval, w_A):
    if rho == 0:
        return Decimal('NaN')  # Avoid division by zero

    rho_dec = Decimal(rho)
    T_eval_dec = Decimal(T_eval)
    w_A_dec = Decimal(w_A)

    term_1 = (Decimal(2)**rho_dec * Decimal("5e-5") * (Decimal("11e-3") + (Decimal("20") * T_eval_dec))) / (Decimal(2) * rho_dec - Decimal(1))
    term_2 = (Decimal(2)**(rho_dec/2) * Decimal("2e-7") * w_A_dec).sqrt() / (Decimal(2) * rho_dec - Decimal(1))
    
    return term_1 + term_2

# Define rho values for graph
rho_values = np.arange(8, 257, 8)  # From 8 to 256 in steps of 8

# Define the four feasibility square cases
feasibility_cases = [
    {"T_eval": 0, "w_A": 3600, "label": "The Flash Grind Case"},
    {"T_eval": 0, "w_A": 432000, "label": "The Marathon Grind Case"},
    {"T_eval": 1, "w_A": 3600, "label": "The Brute Force Blitz Case"},
    {"T_eval": 1, "w_A": 432000, "label": "The Endgame Grind Case"}
]

# Compute N_CPU values per use case
use_case_data = []

for case in feasibility_cases:
    for rho in rho_values:
        N_CPU = compute_N_CPU_high_precision(int(rho), int(case["T_eval"]), int(case["w_A"]))
        use_case_data.append((rho, case["label"], float(N_CPU)))

# Convert to DataFrame for structured output
df_use_case_data = pd.DataFrame(use_case_data, columns=["rho", "Use Case", "N_CPU"])

# Display results grouped by use case
tools.display_dataframe_to_user(name="Feasibility Square - Use Case Data", dataframe=df_use_case_data)
