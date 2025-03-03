import matplotlib.pyplot as plt

# Define figure and axis
fig, ax = plt.subplots(figsize=(8, 8))

# Define quadrant labels and their colors
quadrants = [
    ("The Flash Grind Case\n(Fast but Limited)", (0.25, 0.75), "#FFDDC1"),  # Light Orange
    ("The Marathon Grind Case\n(Extended but Optimized)", (0.25, 0.25), "#D4E157"),  # Light Green
    ("The Brute Force Blitz Case\n(Expensive but Quick)", (0.75, 0.75), "#64B5F6"),  # Light Blue
    ("The Endgame Grind Case\n(Worst-Case Scenario)", (0.75, 0.25), "#F06292")  # Light Pink
]

# Draw the square grid
ax.axhline(0.5, color="black", linewidth=3)  # Thicker grid lines
ax.axvline(0.5, color="black", linewidth=3)

# Add quadrant labels with improved aesthetics
for label, position, color in quadrants:
    ax.text(
        position[0], position[1], label, 
        fontsize=12, ha="center", va="center",
        bbox=dict(facecolor=color, edgecolor="black", boxstyle="round,pad=0.5")
    )

# Set axis labels
ax.set_xticks([0.25, 0.75])
ax.set_yticks([0.25, 0.75])
ax.set_xticklabels(["Low $w_A$", "High $w_A$"], fontsize=14, fontweight="bold")
ax.set_yticklabels(["High $T_{\text{eval}}$", "Low $T_{\text{eval}}$"], fontsize=14, fontweight="bold")

# Remove additional axis ticks and grid
ax.set_xlim(0, 1)
ax.set_ylim(0, 1)
ax.set_xticks([])
ax.set_yticks([])
ax.grid(False)

# Set title
ax.set_title("📊 The Feasibility Square - Attack Scenarios", fontsize=16, fontweight="bold", pad=20)

# Show plot
plt.show()
