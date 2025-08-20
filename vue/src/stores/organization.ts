import { defineStore } from 'pinia'
import { ref, computed } from 'vue'
import type { OrganizationResponse } from '@/services/organizationService'
import { toast } from 'vue-sonner'

const ORGANIZATION_STORAGE_KEY = 'selected_organization'

export const useOrganizationStore = defineStore('organization', () => {
    // State
    const selectedOrganization = ref<OrganizationResponse | null>(null)

    // Computed
    const isOrganizationSelected = computed(() => !!selectedOrganization.value)

    // Getter
    const getSelectedOrganization = (): OrganizationResponse | null => {
        return selectedOrganization.value
    }

    // Setter
    const setSelectedOrganization = (organization: OrganizationResponse | null) => {
        selectedOrganization.value = organization

        if (organization) {
            // Save to localStorage
            localStorage.setItem(ORGANIZATION_STORAGE_KEY, JSON.stringify(organization))
        } else {
            // Remove from localStorage
            localStorage.removeItem(ORGANIZATION_STORAGE_KEY)
        }
    }

    // Initialize from localStorage
    const initializeFromStorage = () => {
        try {
            const stored = localStorage.getItem(ORGANIZATION_STORAGE_KEY)
            if (stored) {
                const org = JSON.parse(stored) as OrganizationResponse
                selectedOrganization.value = org
            }
        } catch (err) {
            toast.error('Error parsing stored organization')
            localStorage.removeItem(ORGANIZATION_STORAGE_KEY)
        }
    }

    // Clear selection
    const clearSelection = () => {
        selectedOrganization.value = null
        localStorage.removeItem(ORGANIZATION_STORAGE_KEY)
    }

    return {
        // State
        selectedOrganization,

        // Computed
        isOrganizationSelected,

        // Methods
        getSelectedOrganization,
        setSelectedOrganization,
        initializeFromStorage,
        clearSelection
    }
})
