package com.sajjadkademm.retail.domain.settings.model;

import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import jakarta.persistence.Id;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Column;
import jakarta.persistence.OneToOne;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.FetchType;
import jakarta.persistence.ForeignKey;
import jakarta.persistence.Index;
import java.time.LocalDateTime;

import com.sajjadkademm.retail.domain.organization.model.Organization;

import jakarta.validation.constraints.NotNull;

import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UpdateTimestamp;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.Builder;

@Entity
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Table(name = "pos_settings", indexes = {
        @Index(name = "idx_pos_settings_organization_id", columnList = "organization_id")
})
public class POSSetting {
    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private String id;

    @Column(name = "organization_id", nullable = false)
    @NotNull(message = "Organization ID is required")
    private String organizationId;

    // Payment Settings
    @Column(name = "cash_payment_enabled", nullable = false)
    private Boolean cashPaymentEnabled = true;

    @Column(name = "card_payment_enabled", nullable = false)
    private Boolean cardPaymentEnabled = true;

    @Column(name = "change_calculation_method", nullable = false)
    private String changeCalculationMethod = "automatic"; // automatic, manual

    @Column(name = "allow_partial_payments", nullable = false)
    private Boolean allowPartialPayments = false;

    @Column(name = "require_exact_change", nullable = false)
    private Boolean requireExactChange = false;

    // Receipt Settings
    @Column(name = "auto_print_receipts", nullable = false)
    private Boolean autoPrintReceipts = true;

    @Column(name = "receipt_template_html", columnDefinition = "TEXT", nullable = true)
    private String receiptTemplateHtml = "templates/receipt-main.html";

    @Column(name = "receipt_header_template_html", columnDefinition = "TEXT", nullable = true)
    private String receiptHeaderTemplateHtml = "templates/receipt-header.html";

    @Column(name = "receipt_footer_template_html", columnDefinition = "TEXT", nullable = true)
    private String receiptFooterTemplateHtml = "templates/receipt-footer.html";

    @Column(name = "receipt_paper_width_mm", nullable = false)
    private Integer receiptPaperWidthMm = 80;

    // Transaction Settings
    @Column(name = "hold_transactions_enabled", nullable = false)
    private Boolean holdTransactionsEnabled = true;

    @Column(name = "returns_enabled", nullable = false)
    private Boolean returnsEnabled = true;

    @Column(name = "discounts_enabled", nullable = false)
    private Boolean discountsEnabled = true;

    // Customer Settings
    @Column(name = "customer_lookup_enabled", nullable = false)
    private Boolean customerLookupEnabled = true;

    @Column(name = "require_customer_info", nullable = false)
    private Boolean requireCustomerInfo = false;

    // Display Settings
    @Column(name = "show_product_images", nullable = false)
    private Boolean showProductImages = true;

    @Column(name = "show_stock_levels", nullable = false)
    private Boolean showStockLevels = true;

    // Audit Fields
    @CreationTimestamp
    @Column(name = "created_at", nullable = false)
    private LocalDateTime createdAt;

    @UpdateTimestamp
    @Column(name = "updated_at", nullable = false)
    private LocalDateTime updatedAt;

    @OneToOne(fetch = FetchType.EAGER)
    @JoinColumn(name = "organization_id", insertable = false, updatable = false, foreignKey = @ForeignKey(name = "fk_pos_settings_organization"))
    private Organization organization;
}