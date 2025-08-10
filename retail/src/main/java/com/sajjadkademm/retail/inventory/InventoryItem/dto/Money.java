package com.sajjadkademm.retail.inventory.InventoryItem.dto;

import java.math.BigDecimal;

import com.sajjadkademm.retail.utils.dto.Currency;
import jakarta.persistence.Embeddable;
import jakarta.persistence.Enumerated;
import jakarta.persistence.EnumType;
import jakarta.validation.constraints.DecimalMin;
import jakarta.validation.constraints.NotNull;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;


@Embeddable
@Data
@NoArgsConstructor
@AllArgsConstructor
public  class Money {
    @DecimalMin(value = "0.0", message = "Amount cannot be negative")
    private BigDecimal amount;

    @Enumerated(EnumType.STRING)
    @NotNull
    private Currency currency;
}
