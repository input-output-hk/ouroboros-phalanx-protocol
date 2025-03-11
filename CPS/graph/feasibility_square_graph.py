import numpy as np
import matplotlib
import matplotlib.pyplot as plt
from matplotlib.patches import Patch

# Debugging: Confirm script start
print("Starting script execution...")

# Define the range for rho (Grinding Depth), starting at 0 to include exactly 0
rho = np.linspace(0, 256, 1000)  # 1000 points for smooth curve, starts at 0, ends at 256

# Debugging: Confirm rho range
print(f"Rho range: min={rho.min()}, max={rho.max()}")

# Constants from Cardano mainnet parameters
f = 0.05
base_term = 5e-9  # 5 * 10^-9

# Define N_CPU functions for each scenario, 
def ant_glance(rho):
    wa = 3600
    teval = 0
    # Add small epsilon to avoid division by zero at rho = 0, and ensure minimum value
    n_cpu = 0.05 * 2**rho * (base_term + (wa * 1e-6) / (rho + 1e-10))
    return n_cpu

def ant_patrol(rho):
    wa = 432000
    teval = 0
    n_cpu = 0.05 * 2**rho * (base_term + (wa * 1e-6) / (rho + 1e-10))
    return n_cpu

def owl_stare(rho):
    wa = 3600
    teval = 1
    n_cpu = 0.05 * 2**rho * (base_term + (wa * 1e-6 + teval) / (rho + 1e-10))
    return n_cpu

def owl_survey(rho):
    wa = 432000
    teval = 1
    n_cpu = 0.05 * 2**rho * (base_term + (wa * 1e-6 + teval) / (rho + 1e-10))
    return n_cpu

# Calculate log10(N_CPU) for each scenario
log_ant_glance = np.log10(ant_glance(rho))
log_ant_patrol = np.log10(ant_patrol(rho))
log_owl_stare = np.log10(owl_stare(rho))
log_owl_survey = np.log10(owl_survey(rho))

# Debugging: Check for NaNs or infinities and print ranges
print(f"Log Ant Glance: min={log_ant_glance.min()}, max={log_ant_glance.max()}")
print(f"Log Ant Patrol: min={log_ant_patrol.min()}, max={log_ant_patrol.max()}")
print(f"Log Owl Stare: min={log_owl_stare.min()}, max={log_owl_stare.max()}")
print(f"Log Owl Survey: min={log_owl_survey.min()}, max={log_owl_survey.max()}")

# Create the plot with improved aesthetics
plt.figure(figsize=(12, 7))
plt.plot(rho, log_ant_glance, label='Ant Glance', color='blue', linewidth=2)
plt.plot(rho, log_ant_patrol, label='Ant Patrol', color='orange', linewidth=2)
plt.plot(rho, log_owl_stare, label='Owl Stare', color='green', linewidth=2)
plt.plot(rho, log_owl_survey, label='Owl Survey', color='red', linewidth=2)

# Add feasibility threshold regions as horizontal spans based on log10(N_CPU)
plt.axhspan(-10, 4, color='green', alpha=0.1)  # Feasible
plt.axhspan(4, 9, color='yellow', alpha=0.1)   # Expensive
plt.axhspan(9, 13, color='orange', alpha=0.1)  # Borderline
plt.axhspan(13, 90, color='red', alpha=0.1)    # Infeasible

# Removed transition point labels as requested

# Set axis limits to ensure full range is visible
plt.xlim(0, 256)  # X-axis from 0 to 256
# Y-axis limits not explicitly set in your version, so we'll infer from data
y_max = max(log_owl_survey.max(), log_owl_stare.max(), log_ant_patrol.max(), log_ant_glance.max()) + 5  # Increased buffer
plt.ylim(-5, y_max)  # Y-axis starts at 0 to match your intent

# Add labels and title with larger font
plt.xlabel('$\\rho$ (Grinding Depth)', fontsize=14)
plt.ylabel('$\\log_{10}(N_{\\text{CPU}})$', fontsize=14)
plt.title('Behavior of $N_{\\text{CPU}}$ Across Scenarios with Feasibility Thresholds', fontsize=16)
plt.grid(True, linestyle='--', alpha=0.7)

# Add annotation for the delta at rho = 50
rho_idx = np.argmin(np.abs(rho - 50))
delta_log_ncpu = log_owl_survey[rho_idx] - log_ant_glance[rho_idx]
mid_y = log_owl_survey[rho_idx] - (delta_log_ncpu / 2)
# Add the delta label in purple, slightly offset to the right
plt.text(53, mid_y - 3.5, f'$\\Delta \\log_{{10}}(N_{{CPU}}) \\approx {delta_log_ncpu:.1f}$',
         fontsize=12, color='purple', bbox=dict(facecolor='white', alpha=0.8, edgecolor='none'),
         verticalalignment='center')

# Draw a smaller double-headed arrow in purple
plt.annotate('', xy=(50, log_owl_survey[rho_idx]), xytext=(50, log_ant_glance[rho_idx]),
             arrowprops=dict(arrowstyle='<->', color='purple', lw=0.5, shrinkA=15, shrinkB=15))

# Create a custom legend with all labels, improved spacing, placed at bottom right
legend_elements = [
    plt.Line2D([0], [0], color='blue', lw=2, label='Ant Glance'),
    plt.Line2D([0], [0], color='orange', lw=2, label='Ant Patrol'),
    plt.Line2D([0], [0], color='green', lw=2, label='Owl Stare'),
    plt.Line2D([0], [0], color='red', lw=2, label='Owl Survey'),
    Patch(facecolor='green', alpha=0.1, label='Feasible'),  # Removed interval
    Patch(facecolor='yellow', alpha=0.1, label='Expensive'),  # Removed interval
    Patch(facecolor='orange', alpha=0.1, label='Borderline'),  # Removed interval
    Patch(facecolor='red', alpha=0.1, label='Infeasible')  # Removed interval
]
plt.legend(handles=legend_elements, fontsize=10, loc='lower right', 
           bbox_to_anchor=(1, 0), ncol=2, handletextpad=0.5, columnspacing=1.5)

# Debugging: Confirm legend added
print("Legend added, saving figure...")

# Adjust layout to prevent overlap, with manual padding
plt.subplots_adjust(left=0.1, right=0.95, top=0.9, bottom=0.2)

# Save the plot and handle potential errors
try:
    plt.savefig('/Users/nhenin/dev/innovation/ouroboros-anti-grinding-design/CPS/graph/grinding_depth_scenarios_with_delta.png', dpi=300, bbox_inches='tight')
    print("Figure saved successfully.")
    plt.show()
except Exception as e:
    print(f"Error saving figure: {e}")

# Debugging: Confirm script end
print("Script execution completed.")