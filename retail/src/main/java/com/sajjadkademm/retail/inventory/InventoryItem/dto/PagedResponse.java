package com.sajjadkademm.retail.inventory.InventoryItem.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * Generic paged response wrapper for API responses
 * 
 * @param <T> The type of content in the page
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class PagedResponse<T> {
    
    /**
     * The content of the current page
     */
    private List<T> content;
    
    /**
     * Current page number (0-based)
     */
    private int page;
    
    /**
     * Number of items per page
     */
    private int size;
    
    /**
     * Total number of elements across all pages
     */
    private long totalElements;
    
    /**
     * Total number of pages
     */
    private int totalPages;
    
    /**
     * Whether this is the first page
     */
    private boolean first;
    
    /**
     * Whether this is the last page
     */
    private boolean last;
    
    /**
     * Whether there is a next page
     */
    private boolean hasNext;
    
    /**
     * Whether there is a previous page
     */
    private boolean hasPrevious;
    
    /**
     * Number of elements in the current page
     */
    private int numberOfElements;
    
    /**
     * Whether the page is empty
     */
    private boolean empty;
}