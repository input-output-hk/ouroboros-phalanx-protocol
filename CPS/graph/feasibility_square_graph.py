import numpy as np
import matplotlib.pyplot as plt
from decimal import Decimal, getcontext

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

# Define rho values for full range (from 4 to 256)
rho_values = np.arange(4, 257, 8)  # Starts from 4 instead of 8

# Define the four randomness manipulation scenarios
manipulation_scenarios = [
    {"T_eval": 0, "w_A": 3600, "label": "Quick Strike"},
    {"T_eval": 0, "w_A": 432000, "label": "Long Pull"},
    {"T_eval": 1, "w_A": 3600, "label": "Sudden Surge"},
    {"T_eval": 1, "w_A": 432000, "label": "Total Deluge"}
]

# Plot the graph for the four scenarios
plt.figure(figsize=(10, 6))

for case in manipulation_scenarios:
    N_CPU_values = np.array([compute_N_CPU_high_precision(int(rho), int(case["T_eval"]), int(case["w_A"])) for rho in rho_values], dtype=float)
    
    # Handle cases where log values might be undefined or negative
    N_CPU_values = np.nan_to_num(N_CPU_values, nan=1e-20, posinf=1e20, neginf=1e-20)  # Avoid issues with log(0)
    
    plt.plot(rho_values, np.log10(N_CPU_values), label=case["label"])

# Set labels and title
plt.xlabel(r"$\rho$ (Grinding Depth)", fontsize=12)
plt.ylabel(r"$\log_{10}(N_{\mathrm{CPU}})$", fontsize=12)
# plt.title("CPU Requirements for Randomness Manipulation Scenarios", fontsize=14)
plt.legend()
plt.grid(True, which="both", linestyle="--", linewidth=0.5)

# Show the plot
plt.show()