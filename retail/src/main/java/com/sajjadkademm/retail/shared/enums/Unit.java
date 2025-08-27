package com.sajjadkademm.retail.shared.enums;

/**
 * Common measurement units for retail inventory items
 */
public enum Unit {
    // Count-based
    PIECES("pieces"),
    PAIRS("pairs"),
    SETS("sets"),
    BOXES("boxes"),
    PACKS("packs"),

    // Weight
    GRAMS("grams"),
    KILOGRAMS("kg"),
    POUNDS("lbs"),

    // Volume
    MILLILITERS("ml"),
    LITERS("liters"),

    // Containers
    BOTTLES("bottles"),
    CANS("cans"),
    BAGS("bags");

    private final String displayName;

    Unit(String displayName) {
        this.displayName = displayName;
    }

    public String getDisplayName() {
        return displayName;
    }

    @Override
    public String toString() {
        return displayName;
    }
}