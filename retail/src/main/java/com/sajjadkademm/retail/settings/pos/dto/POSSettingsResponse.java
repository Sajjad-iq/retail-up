package com.sajjadkademm.retail.settings.pos.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class POSSettingsResponse {

    private String id;
    private String organizationId;

    // Payment Settings
    private Boolean cashPaymentEnabled;
    private Boolean cardPaymentEnabled;
    private String changeCalculationMethod;
    private Boolean allowPartialPayments;
    private Boolean requireExactChange;

    // Receipt Settings
    private Boolean autoPrintReceipts;
    private String receiptTemplateHtml;
    private String receiptHeaderTemplateHtml;
    private String receiptFooterTemplateHtml;
    private Integer receiptPaperWidthMm;

    // Transaction Settings
    private Boolean holdTransactionsEnabled;
    private Boolean returnsEnabled;
    private Boolean discountsEnabled;

    // Customer Settings
    private Boolean customerLookupEnabled;
    private Boolean requireCustomerInfo;

    // Display Settings
    private Boolean showProductImages;
    private Boolean showStockLevels;

    // Audit Fields
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
}