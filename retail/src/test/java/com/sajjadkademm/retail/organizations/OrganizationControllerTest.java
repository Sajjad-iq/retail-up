package com.sajjadkademm.retail.organizations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.sajjadkademm.retail.organizations.dto.CreateOrganizationRequest;
import com.sajjadkademm.retail.organizations.dto.OrganizationResponse;
import com.sajjadkademm.retail.organizations.dto.OrganizationStatus;
import com.sajjadkademm.retail.organizations.dto.UpdateOrganizationRequest;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.List;

import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

@WebMvcTest(OrganizationController.class)
class OrganizationControllerTest {

        @Autowired
        private MockMvc mockMvc;

        @MockBean
        private OrganizationService organizationService;

        @Autowired
        private ObjectMapper objectMapper;

        private OrganizationResponse sampleOrganization;
        private CreateOrganizationRequest createRequest;
        private UpdateOrganizationRequest updateRequest;

        @BeforeEach
        void setUp() {
                sampleOrganization = OrganizationResponse.builder()
                                .id("org-123")
                                .name("Test Organization")
                                .description("A test organization")
                                .city("Test City")
                                .state("Test State")
                                .country("Test Country")
                                .postalCode("12345")
                                .taxId("TAX123")
                                .status(OrganizationStatus.ACTIVE)
                                .createdAt(LocalDateTime.now())
                                .updatedAt(LocalDateTime.now())
                                .createdBy("user-123")
                                .build();

                createRequest = new CreateOrganizationRequest();
                createRequest.setName("Test Organization");
                createRequest.setDescription("A test organization");
                createRequest.setStatus(OrganizationStatus.ACTIVE);

                updateRequest = new UpdateOrganizationRequest();
                updateRequest.setName("Updated Organization");
                updateRequest.setDescription("Updated description");
        }

        @Test
        void createOrganization_Success() throws Exception {
                when(organizationService.createOrganization(any(CreateOrganizationRequest.class), anyString()))
                                .thenReturn(sampleOrganization);

                mockMvc.perform(post("/api/organizations")
                                .contentType(MediaType.APPLICATION_JSON)
                                .header("User-ID", "user-123")
                                .content(objectMapper.writeValueAsString(createRequest)))
                                .andExpect(status().isOk())
                                .andExpect(jsonPath("$.id").value("org-123"))
                                .andExpect(jsonPath("$.name").value("Test Organization"));

                verify(organizationService).createOrganization(any(CreateOrganizationRequest.class), eq("user-123"));
        }

        @Test
        void getOrganizationById_Success() throws Exception {
                when(organizationService.getOrganizationById("org-123"))
                                .thenReturn(sampleOrganization);

                mockMvc.perform(get("/api/organizations/org-123"))
                                .andExpect(status().isOk())
                                .andExpect(jsonPath("$.id").value("org-123"))
                                .andExpect(jsonPath("$.name").value("Test Organization"));

                verify(organizationService).getOrganizationById("org-123");
        }

        @Test
        void getAllOrganizations_Success() throws Exception {
                List<OrganizationResponse> organizations = Arrays.asList(sampleOrganization);
                when(organizationService.getAllOrganizations()).thenReturn(organizations);

                mockMvc.perform(get("/api/organizations"))
                                .andExpect(status().isOk())
                                .andExpect(jsonPath("$[0].id").value("org-123"))
                                .andExpect(jsonPath("$[0].name").value("Test Organization"));

                verify(organizationService).getAllOrganizations();
        }

        @Test
        void getOrganizationsByStatus_Success() throws Exception {
                List<OrganizationResponse> organizations = Arrays.asList(sampleOrganization);
                when(organizationService.getOrganizationsByStatus(OrganizationStatus.ACTIVE))
                                .thenReturn(organizations);

                mockMvc.perform(get("/api/organizations/status/ACTIVE"))
                                .andExpect(status().isOk())
                                .andExpect(jsonPath("$[0].status").value("ACTIVE"));

                verify(organizationService).getOrganizationsByStatus(OrganizationStatus.ACTIVE);
        }

        @Test
        void searchOrganizations_Success() throws Exception {
                List<OrganizationResponse> organizations = Arrays.asList(sampleOrganization);
                when(organizationService.searchOrganizations("test"))
                                .thenReturn(organizations);

                mockMvc.perform(get("/api/organizations/search")
                                .param("q", "test"))
                                .andExpect(status().isOk())
                                .andExpect(jsonPath("$[0].name").value("Test Organization"));

                verify(organizationService).searchOrganizations("test");
        }

        @Test
        void updateOrganization_Success() throws Exception {
                OrganizationResponse updatedOrganization = sampleOrganization.toBuilder()
                                .name("Updated Organization")
                                .build();

                when(organizationService.updateOrganization(eq("org-123"), any(UpdateOrganizationRequest.class),
                                anyString()))
                                .thenReturn(updatedOrganization);

                mockMvc.perform(put("/api/organizations/org-123")
                                .contentType(MediaType.APPLICATION_JSON)
                                .header("User-ID", "user-123")
                                .content(objectMapper.writeValueAsString(updateRequest)))
                                .andExpect(status().isOk())
                                .andExpect(jsonPath("$.name").value("Updated Organization"));

                verify(organizationService).updateOrganization(eq("org-123"), any(UpdateOrganizationRequest.class),
                                eq("user-123"));
        }

        @Test
        void deleteOrganization_Success() throws Exception {
                doNothing().when(organizationService).deleteOrganization("org-123");

                mockMvc.perform(delete("/api/organizations/org-123"))
                                .andExpect(status().isNoContent());

                verify(organizationService).deleteOrganization("org-123");
        }

        @Test
        void activateOrganization_Success() throws Exception {
                when(organizationService.activateOrganization("org-123"))
                                .thenReturn(sampleOrganization);

                mockMvc.perform(patch("/api/organizations/org-123/activate"))
                                .andExpect(status().isOk())
                                .andExpect(jsonPath("$.status").value("ACTIVE"));

                verify(organizationService).activateOrganization("org-123");
        }

        @Test
        void deactivateOrganization_Success() throws Exception {
                OrganizationResponse deactivatedOrganization = sampleOrganization.toBuilder()
                                .status(OrganizationStatus.INACTIVE)
                                .build();

                when(organizationService.deactivateOrganization("org-123"))
                                .thenReturn(deactivatedOrganization);

                mockMvc.perform(patch("/api/organizations/org-123/deactivate"))
                                .andExpect(status().isOk())
                                .andExpect(jsonPath("$.status").value("INACTIVE"));

                verify(organizationService).deactivateOrganization("org-123");
        }

        @Test
        void getOrganizationStatistics_Success() throws Exception {
                OrganizationService.OrganizationStatistics statistics = OrganizationService.OrganizationStatistics
                                .builder()
                                .totalOrganizations(10)
                                .activeOrganizations(8)
                                .inactiveOrganizations(1)
                                .suspendedOrganizations(1)
                                .pendingOrganizations(0)
                                .build();

                when(organizationService.getOrganizationStatistics()).thenReturn(statistics);

                mockMvc.perform(get("/api/organizations/statistics"))
                                .andExpect(status().isOk())
                                .andExpect(jsonPath("$.totalOrganizations").value(10))
                                .andExpect(jsonPath("$.activeOrganizations").value(8));

                verify(organizationService).getOrganizationStatistics();
        }

        @Test
        void createOrganization_ValidationError() throws Exception {
                CreateOrganizationRequest invalidRequest = new CreateOrganizationRequest();
                invalidRequest.setName(""); // Invalid: empty name

                mockMvc.perform(post("/api/organizations")
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(objectMapper.writeValueAsString(invalidRequest)))
                                .andExpect(status().isBadRequest());

                verify(organizationService, never()).createOrganization(any(), any());
        }

        @Test
        void updateOrganization_ValidationError() throws Exception {
                UpdateOrganizationRequest invalidRequest = new UpdateOrganizationRequest();
                invalidRequest.setName(""); // Invalid: empty name

                mockMvc.perform(put("/api/organizations/org-123")
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(objectMapper.writeValueAsString(invalidRequest)))
                                .andExpect(status().isBadRequest());

                verify(organizationService, never()).updateOrganization(any(), any(), any());
        }
}