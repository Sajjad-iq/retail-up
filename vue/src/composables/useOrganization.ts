import { computed, ref, onMounted } from 'vue'
import { useAuthStore } from '@/stores/auth'
import { useOrganizationStore } from '@/stores/organization'
import { organizationService } from '@/services/organizationService'
import { toast } from 'vue-sonner'
import type { OrganizationResponse, CreateOrganizationRequest } from '@/services/organizationService'

export function useOrganization() {
    const authStore = useAuthStore()
    const organizationStore = useOrganizationStore()
    const isLoading = ref(false)
    const organizations = ref<OrganizationResponse[]>([])
    const error = ref<string | null>(null)

    // Computed properties
    const hasOrganizations = computed(() => organizations.value.length > 0)
    const isOrganizationSelected = computed(() => organizationStore.isOrganizationSelected)
    const selectedOrganization = computed(() => organizationStore.selectedOrganization)

    // Get organizations for the current user
    const fetchUserOrganizations = async () => {
        if (!authStore.user?.id) {
            const errorMsg = 'User not authenticated'
            error.value = errorMsg
            toast.error(errorMsg)
            return false
        }

        isLoading.value = true
        error.value = null

        try {
            // For now, we'll get all organizations since the backend doesn't have a specific endpoint
            // In the future, this should be: `/organizations/user/${authStore.user.id}`
            const result = await organizationService.getOrganizations()

            if (result.success && result.data) {
                organizations.value = result.data
                return true
            } else {
                const errorMsg = result.error || 'Failed to fetch organizations'
                error.value = errorMsg
                toast.error(errorMsg)
                return false
            }
        } catch (err) {
            const errorMsg = 'An error occurred while fetching organizations'
            error.value = errorMsg
            toast.error(errorMsg)
            return false
        } finally {
            isLoading.value = false
        }
    }

    // Select an organization
    const selectOrganization = (organization: OrganizationResponse) => {
        organizationStore.setSelectedOrganization(organization)
        authStore.setOrganization(organization)
    }

    // Get the selected organization from localStorage
    const getStoredOrganization = (): OrganizationResponse | null => {
        try {
            const stored = localStorage.getItem('selected_organization')
            if (stored) {
                const org = JSON.parse(stored) as OrganizationResponse
                // Validate that the stored organization exists in the user's organizations
                if (organizations.value.some(o => o.id === org.id)) {
                    return org
                }
            }
        } catch (err) {
            toast.error('Error parsing stored organization')
        }
        return null
    }

    // Initialize organization state
    const initialize = async () => {
        // First fetch organizations
        const success = await fetchUserOrganizations()

        if (success) {
            // Try to restore from localStorage
            const storedOrg = getStoredOrganization()
            if (storedOrg) {
                selectOrganization(storedOrg)
            }
        }
    }

    // Clear selected organization
    const clearSelection = () => {
        organizationStore.setSelectedOrganization(null)
        authStore.setOrganization(null)

        toast.info('Organization selection cleared')
    }

    // Create a new organization
    const createOrganization = async (orgData: Omit<CreateOrganizationRequest, 'userId'>) => {
        if (!authStore.user?.id) {
            const errorMsg = 'User not authenticated'
            error.value = errorMsg
            toast.error(errorMsg)
            return false
        }

        isLoading.value = true
        error.value = null

        try {
            const result = await organizationService.createOrganization({
                userId: authStore.user.id,
                ...orgData
            })

            if (result.success && result.data) {
                // Add the new organization to the list
                organizations.value.push(result.data)

                // Optionally select the new organization
                selectOrganization(result.data)

                toast.success(`Organization "${result.data.name}" created successfully`)
                return true
            } else {
                const errorMsg = result.error || 'Failed to create organization'
                error.value = errorMsg
                toast.error(errorMsg)
                return false
            }
        } catch (err) {
            const errorMsg = 'An error occurred while creating organization'
            error.value = errorMsg
            toast.error(errorMsg)
            return false
        } finally {
            isLoading.value = false
        }
    }

    // Get organization by ID
    const getOrganizationById = async (id: string): Promise<OrganizationResponse | null> => {
        try {
            const result = await organizationService.getOrganization(id)
            if (result.success && result.data) {
                return result.data
            } else {
                toast.error(result.error || 'Failed to fetch organization')
                return null
            }
        } catch (err) {
            toast.error('An error occurred while fetching organization')
            return null
        }
    }

    // Update organization
    const updateOrganization = async (id: string, userId: string, updateData: any): Promise<boolean> => {
        isLoading.value = true
        error.value = null

        try {
            const result = await organizationService.updateOrganization(id, userId, updateData)

            if (result.success && result.data) {
                // Update the organization in the list
                const index = organizations.value.findIndex(org => org.id === id)
                if (index !== -1) {
                    organizations.value[index] = result.data
                }

                // Update selected organization if it's the one being updated
                if (selectedOrganization.value?.id === id) {
                    organizationStore.setSelectedOrganization(result.data)
                    authStore.setOrganization(result.data)
                }

                toast.success(`Organization "${result.data.name}" updated successfully`)
                return true
            } else {
                const errorMsg = result.error || 'Failed to update organization'
                error.value = errorMsg
                toast.error(errorMsg)
                return false
            }
        } catch (err) {
            const errorMsg = 'An error occurred while updating organization'
            error.value = errorMsg
            toast.error(errorMsg)
            return false
        } finally {
            isLoading.value = false
        }
    }

    // Auto-initialize when composable is used
    onMounted(() => {
        if (authStore.isAuthenticated) {
            initialize()
        }
    })

    return {
        // State
        organizations,
        selectedOrganization,
        isLoading,
        error,

        // Computed
        hasOrganizations,
        isOrganizationSelected,

        // Methods
        fetchUserOrganizations,
        selectOrganization,
        createOrganization,
        clearSelection,
        initialize,
        getOrganizationById,
        updateOrganization
    }
}
