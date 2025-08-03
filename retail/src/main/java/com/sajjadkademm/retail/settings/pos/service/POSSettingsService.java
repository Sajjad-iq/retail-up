package com.sajjadkademm.retail.settings.pos.service;

import com.sajjadkademm.retail.exceptions.NotFoundException;
import com.sajjadkademm.retail.exceptions.BadRequestException;
import com.sajjadkademm.retail.settings.pos.entity.POSSetting;
import com.sajjadkademm.retail.settings.pos.repository.POSSettingRepository;
import com.sajjadkademm.retail.settings.pos.dto.POSSettingsRequest;
import com.sajjadkademm.retail.settings.pos.dto.POSSettingsResponse;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class POSSettingsService {

        private final POSSettingRepository posSettingRepository;

        /**
         * Get POS settings for an organization
         */
        public POSSettingsResponse getPOSSettings(String organizationId) {
                POSSetting setting = posSettingRepository.findByOrganizationId(organizationId)
                                .orElseThrow(() -> new NotFoundException(
                                                "POS settings not found for organization: " + organizationId));

                return mapToResponse(setting);
        }

        /**
         * Update POS settings
         */
        public POSSettingsResponse updatePOSSettings(String organizationId, POSSettingsRequest request, String userId) {
                POSSetting setting = posSettingRepository.findByOrganizationId(organizationId)
                                .orElseThrow(() -> new NotFoundException(
                                                "POS settings not found for organization: " + organizationId));

                setting.setCashPaymentEnabled(request.getCashPaymentEnabled());
                setting.setCardPaymentEnabled(request.getCardPaymentEnabled());
                setting.setChangeCalculationMethod(request.getChangeCalculationMethod());
                setting.setAllowPartialPayments(request.getAllowPartialPayments());
                setting.setRequireExactChange(request.getRequireExactChange());
                setting.setAutoPrintReceipts(request.getAutoPrintReceipts());
                setting.setReceiptTemplateHtml(request.getReceiptTemplateHtml());
                setting.setReceiptHeaderTemplateHtml(request.getReceiptHeaderTemplateHtml());
                setting.setReceiptFooterTemplateHtml(request.getReceiptFooterTemplateHtml());
                setting.setReceiptPaperWidthMm(request.getReceiptPaperWidthMm());
                setting.setHoldTransactionsEnabled(request.getHoldTransactionsEnabled());
                setting.setReturnsEnabled(request.getReturnsEnabled());
                setting.setDiscountsEnabled(request.getDiscountsEnabled());
                setting.setCustomerLookupEnabled(request.getCustomerLookupEnabled());
                setting.setRequireCustomerInfo(request.getRequireCustomerInfo());
                setting.setShowProductImages(request.getShowProductImages());
                setting.setShowStockLevels(request.getShowStockLevels());
                setting.setUpdatedBy(userId);

                POSSetting updatedSetting = posSettingRepository.save(setting);
                return mapToResponse(updatedSetting);
        }

        /**
         * Reset POS settings to defaults
         */
        public POSSettingsResponse resetToDefaults(String organizationId) {
                POSSetting setting = posSettingRepository.findByOrganizationId(organizationId)
                                .orElseThrow(() -> new NotFoundException(
                                                "POS settings not found for organization: " + organizationId));

                POSSetting defaultSettings = createDefaultPOSSettings(organizationId);
                defaultSettings.setId(setting.getId());
                defaultSettings.setCreatedAt(setting.getCreatedAt());

                POSSetting savedSetting = posSettingRepository.save(defaultSettings);
                return mapToResponse(savedSetting);
        }

        /**
         * Create default POS settings
         */
        public POSSetting createDefaultPOSSettings(String organizationId) {
                return POSSetting.builder()
                                .organizationId(organizationId)
                                // Payment Settings
                                .cashPaymentEnabled(true)
                                .cardPaymentEnabled(true)
                                .changeCalculationMethod("automatic")
                                .allowPartialPayments(false)
                                .requireExactChange(false)
                                // Receipt Settings
                                .autoPrintReceipts(true)
                                .receiptTemplateHtml("templates/receipt-main.html")
                                .receiptHeaderTemplateHtml("templates/receipt-header.html")
                                .receiptFooterTemplateHtml("templates/receipt-footer.html")
                                .receiptPaperWidthMm(80)
                                // Transaction Settings
                                .holdTransactionsEnabled(true)
                                .returnsEnabled(true)
                                .discountsEnabled(true)
                                // Customer Settings
                                .customerLookupEnabled(true)
                                .requireCustomerInfo(false)
                                // Display Settings
                                .showProductImages(true)
                                .showStockLevels(true)
                                .build();
        }

        /**
         * Create and save default POS settings for a new organization
         */
        public POSSetting createAndSaveDefaultPOSSettings(String organizationId, String createdBy) {
                try {
                        POSSetting defaultSettings = createDefaultPOSSettings(organizationId);
                        defaultSettings.setUpdatedBy(createdBy);
                        POSSetting savedSettings = posSettingRepository.save(defaultSettings);
                        return savedSettings;
                } catch (Exception e) {
                        throw new BadRequestException("Failed to create default POS settings: " + e.getMessage(), e);
                }
        }

        /**
         * Map POSSetting entity to POSSettingsResponse DTO
         */
        private POSSettingsResponse mapToResponse(POSSetting setting) {
                return POSSettingsResponse.builder()
                                .id(setting.getId())
                                .organizationId(setting.getOrganizationId())
                                // Payment Settings
                                .cashPaymentEnabled(setting.getCashPaymentEnabled())
                                .cardPaymentEnabled(setting.getCardPaymentEnabled())
                                .changeCalculationMethod(setting.getChangeCalculationMethod())
                                .allowPartialPayments(setting.getAllowPartialPayments())
                                .requireExactChange(setting.getRequireExactChange())
                                // Receipt Settings
                                .autoPrintReceipts(setting.getAutoPrintReceipts())
                                .receiptTemplateHtml(setting.getReceiptTemplateHtml())
                                .receiptHeaderTemplateHtml(setting.getReceiptHeaderTemplateHtml())
                                .receiptFooterTemplateHtml(setting.getReceiptFooterTemplateHtml())
                                .receiptPaperWidthMm(setting.getReceiptPaperWidthMm())
                                // Transaction Settings
                                .holdTransactionsEnabled(setting.getHoldTransactionsEnabled())
                                .returnsEnabled(setting.getReturnsEnabled())
                                .discountsEnabled(setting.getDiscountsEnabled())
                                // Customer Settings
                                .customerLookupEnabled(setting.getCustomerLookupEnabled())
                                .requireCustomerInfo(setting.getRequireCustomerInfo())
                                // Display Settings
                                .showProductImages(setting.getShowProductImages())
                                .showStockLevels(setting.getShowStockLevels())
                                // Audit Fields
                                .updatedBy(setting.getUpdatedBy())
                                .createdAt(setting.getCreatedAt())
                                .updatedAt(setting.getUpdatedAt())
                                .build();
        }
}