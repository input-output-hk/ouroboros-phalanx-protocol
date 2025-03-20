import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import Patch

# Define the range for rho
rho = np.linspace(1, 256, 1000)

# Define constants
f = 0.05
cost_per_cpu_hour = 0.01

# Compute w_O in seconds and hours
w_O = 20 * (2 * rho - 1)
w_O_hours = w_O / 3600

# N_CPU functions from CIP Section 4 (unchanged)
def ant_glance_praos(rho):
    return 5e-10 * 2**(rho - 1) + 1.8e-11 * 2**(rho - 1)

def ant_patrol_praos(rho):
    return 5e-10 * 2**(rho - 1) + 2.16e-9 * 2**(rho - 1)

def owl_stare_praos(rho):
    return 5e-10 * 2**(rho - 1) + 1.8e-11 * 2**(rho - 1) + 5e-2 * 2**(rho - 1) / rho

def owl_survey_praos(rho):
    return 5e-10 * 2**(rho - 1) + 2.16e-9 * 2**(rho - 1) + 5e-2 * 2**(rho - 1) / rho

def ant_glance_phi_min(rho):
    return 5e-10 * 2**(rho - 1) + 1.8e-11 * 2**(rho - 1) + 4.32e4 * 2**rho

def ant_patrol_phi_min(rho):
    return 5e-10 * 2**(rho - 1) + 2.16e-9 * 2**(rho - 1) + 4.32e4 * 2**rho

def owl_stare_phi_min(rho):
    return 5e-10 * 2**(rho - 1) + 1.8e-11 * 2**(rho - 1) + 5e-2 * 2**(rho - 1) / rho + 4.32e4 * 2**rho

def owl_survey_phi_min(rho):
    return 5e-10 * 2**(rho - 1) + 2.16e-9 * 2**(rho - 1) + 5e-2 * 2**(rho - 1) / rho + 4.32e4 * 2**rho

def ant_glance_phi_max(rho):
    return 5e-10 * 2**(rho - 1) + 1.8e-11 * 2**(rho - 1) + 4.32e5 * 2**rho

def ant_patrol_phi_max(rho):
    return 5e-10 * 2**(rho - 1) + 2.16e-9 * 2**(rho - 1) + 4.32e5 * 2**rho

def owl_stare_phi_max(rho):
    return 5e-10 * 2**(rho - 1) + 1.8e-11 * 2**(rho - 1) + 5e-2 * 2**(rho - 1) / rho + 4.32e5 * 2**rho

def owl_survey_phi_max(rho):
    return 5e-10 * 2**(rho - 1) + 2.16e-9 * 2**(rho - 1) + 5e-2 * 2**(rho - 1) / rho + 4.32e5 * 2**rho

# Compute N_CPU
n_cpu = {
    'ant_glance_praos': ant_glance_praos(rho),
    'ant_patrol_praos': ant_patrol_praos(rho),
    'owl_stare_praos': owl_stare_praos(rho),
    'owl_survey_praos': owl_survey_praos(rho),
    'ant_glance_phi_min': ant_glance_phi_min(rho),
    'ant_patrol_phi_min': ant_patrol_phi_min(rho),
    'owl_stare_phi_min': owl_stare_phi_min(rho),
    'owl_survey_phi_min': owl_survey_phi_min(rho),
    'ant_glance_phi_max': ant_glance_phi_max(rho),
    'ant_patrol_phi_max': ant_patrol_phi_max(rho),
    'owl_stare_phi_max': owl_stare_phi_max(rho),
    'owl_survey_phi_max': owl_survey_phi_max(rho)
}

# Compute cost in USD, clipping to avoid overflow
max_float = 1e308
cost = {key: np.clip(n_cpu[key] * cost_per_cpu_hour * w_O_hours, 0, max_float) for key in n_cpu}

# Calculate log10(Cost)
epsilon = 1e-100
log_cost = {key: np.log10(np.maximum(cost[key], epsilon)) for key in cost}

# Debug: Print values at ρ = 50
rho_idx = np.argmin(np.abs(rho - 50))
for key in log_cost:
    print(f"{key} at ρ=50: log10(Cost) = {log_cost[key][rho_idx]:.2f}")

# Toggle flags for each curve (True = show, False = hide)
toggles = {
    'ant_glance_praos': False,
    'ant_patrol_praos': False,
    'owl_stare_praos': False,
    'owl_survey_praos': False,
    'ant_glance_phi_min': True,
    'ant_patrol_phi_min': False,
    'owl_stare_phi_min': False,
    'owl_survey_phi_min': False,
    'ant_glance_phi_max': True,
    'ant_patrol_phi_max': False,
    'owl_stare_phi_max': False,
    'owl_survey_phi_max': False
}

# Create the plot
plt.figure(figsize=(14, 8))

# Define styles
styles = {
    'ant_glance_praos': ('blue', '-', 2, 'Ant Glance Praos'),
    'ant_patrol_praos': ('orange', '-', 2, 'Ant Patrol Praos'),
    'owl_stare_praos': ('green', '-', 2, 'Owl Stare Praos'),
    'owl_survey_praos': ('red', '-', 2, 'Owl Survey Praos'),
    'ant_glance_phi_min': ('blue', '--', 2, 'Ant Glance $\\Phi_{\\text{min}}$'),
    'ant_patrol_phi_min': ('orange', '--', 2, 'Ant Patrol $\\Phi_{\\text{min}}$'),
    'owl_stare_phi_min': ('green', '--', 2, 'Owl Stare $\\Phi_{\\text{min}}$'),
    'owl_survey_phi_min': ('red', '--', 2, 'Owl Survey $\\Phi_{\\text{min}}$'),
    'ant_glance_phi_max': ('blue', ':', 2, 'Ant Glance $\\Phi_{\\text{max}}$'),
    'ant_patrol_phi_max': ('orange', ':', 2, 'Ant Patrol $\\Phi_{\\text{max}}$'),
    'owl_stare_phi_max': ('green', ':', 2, 'Owl Stare $\\Phi_{\\text{max}}$'),
    'owl_survey_phi_max': ('red', ':', 2, 'Owl Survey $\\Phi_{\\text{max}}$')
}

# Plot curves based on toggles
legend_elements = []
for key, (color, linestyle, lw, label) in styles.items():
    if toggles[key]:
        plt.plot(rho, log_cost[key], label=label, color=color, linestyle=linestyle, linewidth=lw)
        legend_elements.append(plt.Line2D([0], [0], color=color, lw=lw, linestyle=linestyle, label=label))

# Add feasibility threshold layers
plt.axhspan(0, 4, color='green', alpha=0.1)
plt.axhspan(4, 6, color='yellow', alpha=0.1)
plt.axhspan(6, 9, color='#FFA07A', alpha=0.1)
plt.axhspan(9, 12, color='#FF6347', alpha=0.1)
plt.axhspan(12, 80, color='red', alpha=0.1)

# Labels and title
plt.xlabel('$\\rho$ (Grinding Depth)', fontsize=14)
plt.ylabel('$\\log_{10}(\\text{Cost (USD)})$', fontsize=14)
plt.title('Cost of Grinding Attacks: Praos vs Phalanx Scenarios (ρ up to 256)', fontsize=16)
plt.grid(True, linestyle='--', alpha=0.7)

# Set axis limits
plt.xlim(0, 256)
valid_log_costs = np.concatenate([log_cost[key] for key in log_cost if toggles[key]])
valid_log_costs = valid_log_costs[np.isfinite(valid_log_costs)]
y_max = np.max(valid_log_costs) + 5 if valid_log_costs.size > 0 else 80
plt.ylim(0, y_max)

# Add annotation for delta at ρ = 50 (if both curves are enabled)
if toggles['owl_survey_phi_max'] and toggles['ant_glance_praos']:
    delta_log_cost = log_cost['owl_survey_phi_max'][rho_idx] - log_cost['ant_glance_praos'][rho_idx]
    mid_y = log_cost['ant_glance_praos'][rho_idx] + (delta_log_cost / 2)
    plt.annotate('', xy=(50, log_cost['owl_survey_phi_max'][rho_idx]), xytext=(50, log_cost['ant_glance_praos'][rho_idx]),
                 arrowprops=dict(arrowstyle='<->', color='purple', lw=1))
    plt.text(53, mid_y, f'$\\Delta \\log_{{10}}(\\text{{Cost}}) \\approx {delta_log_cost:.1f}$',
             fontsize=12, color='purple', bbox=dict(facecolor='white', alpha=0.8))

# Custom legend with feasibility layers
legend_elements += [
    Patch(facecolor='green', alpha=0.1, label='Trivial (< $10K)'),
    Patch(facecolor='yellow', alpha=0.1, label='Feasible ($10K-$1M)'),
    Patch(facecolor='#FFA07A', alpha=0.1, label='Possible ($1M-$1B)'),
    Patch(facecolor='#FF6347', alpha=0.1, label='Borderline Infeasible ($1B-$1T)'),
    Patch(facecolor='red', alpha=0.1, label='Infeasible (> $1T)')
]
plt.legend(handles=legend_elements, fontsize=10, loc='lower right', bbox_to_anchor=(1, 0), ncol=2)

# Adjust layout
plt.subplots_adjust(left=0.1, right=0.95, top=0.9, bottom=0.2)

# Save the plot
plt.savefig('grinding_depth_scenarios_cost_phalanx_individual_toggle.png', dpi=300, bbox_inches='tight')
plt.show()