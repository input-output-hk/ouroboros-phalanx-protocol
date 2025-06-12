import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import Patch

# Define the range for rho (Grinding Depth), starting at 1 to avoid underflow
rho = np.linspace(1, 256, 1000)  # 1000 points for smooth curve, starts at 1, ends at 256

# Define f and cost per CPU-hour
f = 0.05
cost_per_cpu_hour = 0.01  # $0.01 per CPU-hour

# Compute w_O in seconds for each rho
w_O = 20 * (2 * rho - 1)  # w_O = (2 * rho - 1) / f, f = 0.05
w_O_hours = w_O / 3600  # Convert to hours for cost calculation

# Define N_CPU functions for all scenarios
def ant_glance_praos(rho):
    return 5e-10 * 2**(rho - 2) + 1.8e-4 * 2**(rho - 1) / rho

def ant_glance_phalanx(rho):
    return 5e-10 * 2**(rho - 2) + 1.8e-4 * 2**(rho - 1) / rho + 180 * 2**(rho - 1) / rho

def ant_patrol_praos(rho):
    return 5e-10 * 2**(rho - 2) + 2.16e-2 * 2**(rho - 1) / rho

def ant_patrol_phalanx(rho):
    return 5e-10 * 2**(rho - 2) + 2.16e-2 * 2**(rho - 1) / rho + 180 * 2**(rho - 1) / rho

def owl_stare_praos(rho):
    return 5e-10 * 2**(rho - 2) + (1.8e-4 + 5.02e-2) * 2**(rho - 1) / rho

def owl_stare_phalanx(rho):
    return 5e-10 * 2**(rho - 2) + (1.8e-4 + 5.02e-2) * 2**(rho - 1) / rho + 180 * 2**(rho - 1) / rho

def owl_survey_praos(rho):
    return 5e-10 * 2**(rho - 2) + (2.16e-2 + 5.02e-2) * 2**(rho - 1) / rho

def owl_survey_phalanx(rho):
    return 5e-10 * 2**(rho - 2) + (2.16e-2 + 5.02e-2) * 2**(rho - 1) / rho + 180 * 2**(rho - 1) / rho

# Compute N_CPU for all scenarios
n_cpu_ant_glance_praos = ant_glance_praos(rho)
n_cpu_ant_glance_phalanx = ant_glance_phalanx(rho)
n_cpu_ant_patrol_praos = ant_patrol_praos(rho)
n_cpu_ant_patrol_phalanx = ant_patrol_phalanx(rho)
n_cpu_owl_stare_praos = owl_stare_praos(rho)
n_cpu_owl_stare_phalanx = owl_stare_phalanx(rho)
n_cpu_owl_survey_praos = owl_survey_praos(rho)
n_cpu_owl_survey_phalanx = owl_survey_phalanx(rho)

# Compute cost in USD for all scenarios
cost_ant_glance_praos = n_cpu_ant_glance_praos * cost_per_cpu_hour * w_O_hours
cost_ant_glance_phalanx = n_cpu_ant_glance_phalanx * cost_per_cpu_hour * w_O_hours
cost_ant_patrol_praos = n_cpu_ant_patrol_praos * cost_per_cpu_hour * w_O_hours
cost_ant_patrol_phalanx = n_cpu_ant_patrol_phalanx * cost_per_cpu_hour * w_O_hours
cost_owl_stare_praos = n_cpu_owl_stare_praos * cost_per_cpu_hour * w_O_hours
cost_owl_stare_phalanx = n_cpu_owl_stare_phalanx * cost_per_cpu_hour * w_O_hours
cost_owl_survey_praos = n_cpu_owl_survey_praos * cost_per_cpu_hour * w_O_hours
cost_owl_survey_phalanx = n_cpu_owl_survey_phalanx * cost_per_cpu_hour * w_O_hours

# Calculate log10(Cost) for all scenarios, adding epsilon to avoid log of zero
epsilon = 1e-100  # Small positive value to prevent log(0)
log_cost_ant_glance_praos = np.log10(np.maximum(cost_ant_glance_praos, epsilon))
log_cost_ant_glance_phalanx = np.log10(np.maximum(cost_ant_glance_phalanx, epsilon))
log_cost_ant_patrol_praos = np.log10(np.maximum(cost_ant_patrol_praos, epsilon))
log_cost_ant_patrol_phalanx = np.log10(np.maximum(cost_ant_patrol_phalanx, epsilon))
log_cost_owl_stare_praos = np.log10(np.maximum(cost_owl_stare_praos, epsilon))
log_cost_owl_stare_phalanx = np.log10(np.maximum(cost_owl_stare_phalanx, epsilon))
log_cost_owl_survey_praos = np.log10(np.maximum(cost_owl_survey_praos, epsilon))
log_cost_owl_survey_phalanx = np.log10(np.maximum(cost_owl_survey_phalanx, epsilon))

# Create the plot with improved aesthetics
plt.figure(figsize=(12, 7))
# Plot Praos scenarios with solid lines
plt.plot(rho, log_cost_ant_glance_praos, label='Ant Glance Praos', color='blue', linewidth=2)
plt.plot(rho, log_cost_ant_patrol_praos, label='Ant Patrol Praos', color='orange', linewidth=2)
plt.plot(rho, log_cost_owl_stare_praos, label='Owl Stare Praos', color='green', linewidth=2)
plt.plot(rho, log_cost_owl_survey_praos, label='Owl Survey Praos', color='red', linewidth=2)

# Plot Phalanx scenarios with dashed lines
plt.plot(rho, log_cost_ant_glance_phalanx, label='Ant Glance Phalanx', color='blue', linestyle='--', linewidth=2)
plt.plot(rho, log_cost_ant_patrol_phalanx, label='Ant Patrol Phalanx', color='orange', linestyle='--', linewidth=2)
plt.plot(rho, log_cost_owl_stare_phalanx, label='Owl Stare Phalanx', color='green', linestyle='--', linewidth=2)
plt.plot(rho, log_cost_owl_survey_phalanx, label='Owl Survey Phalanx', color='red', linestyle='--', linewidth=2)

# Add feasibility threshold layers as horizontal spans based on log10(Cost USD)
plt.axhspan(-10, 4, color='green', alpha=0.1, label='Trivial')         # Trivial (< $10,000)
plt.axhspan(4, 6, color='yellow', alpha=0.1, label='Feasible')          # Feasible ($10,000 to $1M)
plt.axhspan(6, 9, color='#FFA07A', alpha=0.1, label='Possible')         # Possible ($1M to $1B)
plt.axhspan(9, 12, color='#FF6347', alpha=0.1, label='Borderline Infeasible')  # Borderline Infeasible ($1B to $1T)
plt.axhspan(12, 90, color='red', alpha=0.1, label='Infeasible')         # Infeasible (> $1T)

# Add labels and title with larger font
plt.xlabel('$\\rho$ (Grinding Depth)', fontsize=14)
plt.ylabel('$\\log_{10}(\\text{Cost (USD)})$', fontsize=14)
plt.title('Cost of Grinding Attacks: Praos vs Phalanx Scenarios', fontsize=16)
plt.grid(True, linestyle='--', alpha=0.7)

# Set axis limits
plt.xlim(0, 256)  # X-axis from 0 to 256
valid_log_costs = np.concatenate([
    log_cost_ant_glance_praos[np.isfinite(log_cost_ant_glance_praos)],
    log_cost_ant_glance_phalanx[np.isfinite(log_cost_ant_glance_phalanx)],
    log_cost_ant_patrol_praos[np.isfinite(log_cost_ant_patrol_praos)],
    log_cost_ant_patrol_phalanx[np.isfinite(log_cost_ant_patrol_phalanx)],
    log_cost_owl_stare_praos[np.isfinite(log_cost_owl_stare_praos)],
    log_cost_owl_stare_phalanx[np.isfinite(log_cost_owl_stare_phalanx)],
    log_cost_owl_survey_praos[np.isfinite(log_cost_owl_survey_praos)],
    log_cost_owl_survey_phalanx[np.isfinite(log_cost_owl_survey_phalanx)]
])
y_max = np.max(valid_log_costs) + 5 if valid_log_costs.size > 0 else 90
plt.ylim(-5, y_max)  # Y-axis starts at -5

# Add annotations for deltas
# # Delta at rho = 50 (Phalanx Owl Survey vs Phalanx Ant Glance)
# rho_idx_50 = np.argmin(np.abs(rho - 50))
# delta_log_cost_50 = log_cost_owl_survey_phalanx[rho_idx_50] - log_cost_ant_glance_phalanx[rho_idx_50]
# mid_y_50 = log_cost_owl_survey_phalanx[rho_idx_50] - (delta_log_cost_50 / 2)
# plt.annotate('', xy=(50, log_cost_owl_survey_phalanx[rho_idx_50]), xytext=(50, log_cost_ant_glance_phalanx[rho_idx_50]),
#              arrowprops=dict(arrowstyle='<->', color='black', lw=1))
# plt.text(53, mid_y_50, f'$\\Delta \\approx {delta_log_cost_50:.1f}$', fontsize=12, color='black',
#          bbox=dict(facecolor='white', alpha=0.8, edgecolor='none'), verticalalignment='center')

# Delta at rho = 100 (Phalanx Owl Survey vs Praos Owl Survey)
rho_idx_100 = np.argmin(np.abs(rho - 100))
delta_log_cost_100 = log_cost_owl_survey_phalanx[rho_idx_100] - log_cost_owl_survey_praos[rho_idx_100]
mid_y_100 = log_cost_owl_survey_phalanx[rho_idx_100] - (delta_log_cost_100 / 2)
plt.annotate('', xy=(100, log_cost_owl_survey_phalanx[rho_idx_100]), xytext=(100, log_cost_owl_survey_praos[rho_idx_100]),
             arrowprops=dict(arrowstyle='<->', color='black', lw=1))
plt.text(103, mid_y_100, f'$\\Delta \\approx {delta_log_cost_100:.1f}$', fontsize=12, color='black',
         bbox=dict(facecolor='white', alpha=0.8, edgecolor='none'), verticalalignment='center')

# Delta at rho = 150 (Phalanx Owl Survey vs Praos Ant Glance)
rho_idx_150 = np.argmin(np.abs(rho - 150))
delta_log_cost_150 = log_cost_owl_survey_phalanx[rho_idx_150] - log_cost_ant_glance_praos[rho_idx_150]
mid_y_150 = log_cost_owl_survey_phalanx[rho_idx_150] - (delta_log_cost_150 / 2)
plt.annotate('', xy=(150, log_cost_owl_survey_phalanx[rho_idx_150]), xytext=(150, log_cost_ant_glance_praos[rho_idx_150]),
             arrowprops=dict(arrowstyle='<->', color='black', lw=1))
plt.text(153, mid_y_150, f'$\\Delta \\approx {delta_log_cost_150:.1f}$', fontsize=12, color='black',
         bbox=dict(facecolor='white', alpha=0.8, edgecolor='none'), verticalalignment='center')

# Create a custom legend
legend_elements = [
    plt.Line2D([0], [0], color='blue', lw=2, label='Ant Glance Praos'),
    plt.Line2D([0], [0], color='blue', lw=2, linestyle='--', label='Ant Glance Phalanx'),
    plt.Line2D([0], [0], color='orange', lw=2, label='Ant Patrol Praos'),
    plt.Line2D([0], [0], color='orange', lw=2, linestyle='--', label='Ant Patrol Phalanx'),
    plt.Line2D([0], [0], color='green', lw=2, label='Owl Stare Praos'),
    plt.Line2D([0], [0], color='green', lw=2, linestyle='--', label='Owl Stare Phalanx'),
    plt.Line2D([0], [0], color='red', lw=2, label='Owl Survey Praos'),
    plt.Line2D([0], [0], color='red', lw=2, linestyle='--', label='Owl Survey Phalanx'),
    Patch(facecolor='green', alpha=0.1, label='Trivial'),
    Patch(facecolor='yellow', alpha=0.1, label='Feasible'),
    Patch(facecolor='#FFA07A', alpha=0.1, label='Possible'),
    Patch(facecolor='#FF6347', alpha=0.1, label='Borderline Infeasible'),
    Patch(facecolor='red', alpha=0.1, label='Infeasible')
]
plt.legend(handles=legend_elements, fontsize=10, loc='lower right',
           bbox_to_anchor=(1, 0), ncol=2, handletextpad=0.5, columnspacing=1.5)

# Adjust layout
plt.subplots_adjust(left=0.1, right=0.95, top=0.9, bottom=0.2)

# Save the plot
plt.savefig('grinding_depth_scenarios_cost_praos_vs_phalanx.png', dpi=300, bbox_inches='tight')
plt.show()