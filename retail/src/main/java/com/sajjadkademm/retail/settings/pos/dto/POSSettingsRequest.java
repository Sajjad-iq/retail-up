package com.sajjadkademm.retail.settings.pos.dto;

import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class POSSettingsRequest {

    // Payment Settings
    @NotNull(message = "Cash payment enabled is required")
    private Boolean cashPaymentEnabled;

    @NotNull(message = "Card payment enabled is required")
    private Boolean cardPaymentEnabled;

    @Pattern(regexp = "^(automatic|manual)$", message = "Change calculation method must be automatic or manual")
    @NotNull(message = "Change calculation method is required")
    private String changeCalculationMethod;

    @NotNull(message = "Allow partial payments is required")
    private Boolean allowPartialPayments;

    @NotNull(message = "Require exact change is required")
    private Boolean requireExactChange;

    // Receipt Settings
    @NotNull(message = "Auto print receipts is required")
    private Boolean autoPrintReceipts;

    @NotNull(message = "Receipt template HTML is required")
    private String receiptTemplateHtml;

    @NotNull(message = "Receipt header template HTML is required")
    private String receiptHeaderTemplateHtml;

    @NotNull(message = "Receipt footer template HTML is required")
    private String receiptFooterTemplateHtml;

    @Min(value = 58, message = "Receipt paper width must be at least 58mm")
    @Max(value = 112, message = "Receipt paper width cannot exceed 112mm")
    @NotNull(message = "Receipt paper width is required")
    private Integer receiptPaperWidthMm;

    // Transaction Settings
    @NotNull(message = "Hold transactions enabled is required")
    private Boolean holdTransactionsEnabled;

    @NotNull(message = "Returns enabled is required")
    private Boolean returnsEnabled;

    @NotNull(message = "Discounts enabled is required")
    private Boolean discountsEnabled;

    // Customer Settings
    @NotNull(message = "Customer lookup enabled is required")
    private Boolean customerLookupEnabled;

    @NotNull(message = "Require customer info is required")
    private Boolean requireCustomerInfo;

    // Display Settings
    @NotNull(message = "Show product images is required")
    private Boolean showProductImages;

    @NotNull(message = "Show stock levels is required")
    private Boolean showStockLevels;

}