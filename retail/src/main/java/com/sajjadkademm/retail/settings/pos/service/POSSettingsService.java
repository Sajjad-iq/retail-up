package com.sajjadkademm.retail.settings.pos.service;

import com.sajjadkademm.retail.exceptions.NotFoundException;
import com.sajjadkademm.retail.exceptions.BadRequestException;
import com.sajjadkademm.retail.settings.pos.entity.POSSetting;
import com.sajjadkademm.retail.settings.pos.repository.POSSettingRepository;
import com.sajjadkademm.retail.settings.pos.dto.POSSettingsRequest;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class POSSettingsService {

        private final POSSettingRepository posSettingRepository;

        /**
         * Get POS settings for an organization
         */
        public POSSetting getPOSSettings(String organizationId) {
                return posSettingRepository.findByOrganizationId(organizationId)
                                .orElseThrow(() -> new NotFoundException(
                                                "POS settings not found for organization: " + organizationId));
        }

        /**
         * Update POS settings
         */
        public POSSetting updatePOSSettings(String organizationId, POSSettingsRequest request) {
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

                return posSettingRepository.save(setting);
        }

        /**
         * Reset POS settings to defaults
         */
        public POSSetting resetToDefaults(String organizationId) {
                POSSetting setting = posSettingRepository.findByOrganizationId(organizationId)
                                .orElseThrow(() -> new NotFoundException(
                                                "POS settings not found for organization: " + organizationId));

                POSSetting defaultSettings = createDefaultPOSSettings(organizationId);
                defaultSettings.setId(setting.getId());
                defaultSettings.setCreatedAt(setting.getCreatedAt());

                return posSettingRepository.save(defaultSettings);
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
                        return posSettingRepository.save(defaultSettings);
                } catch (Exception e) {
                        throw new BadRequestException("Failed to create default POS settings: " + e.getMessage(), e);
                }
        }
}