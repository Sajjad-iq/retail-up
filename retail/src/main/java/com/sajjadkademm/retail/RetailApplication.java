package com.sajjadkademm.retail;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;

/**
 * Main Spring Boot application class for the Retail Management System.
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
@SpringBootApplication
@RestController
@Tag(name = "Health Check", description = "Application health and status endpoints")
public class RetailApplication {

	public static void main(String[] args) {
		SpringApplication.run(RetailApplication.class, args);
	}

	/**
	 * Health check endpoint
	 */
	@Operation(summary = "Health Check", description = "Check if the application is running and healthy", operationId = "healthCheck")
	@ApiResponses(value = {
			@ApiResponse(responseCode = "200", description = "Application is healthy and running", content = @io.swagger.v3.oas.annotations.media.Content(mediaType = "text/plain", examples = @io.swagger.v3.oas.annotations.media.ExampleObject(name = "Healthy Response", value = "OK")))
	})
	@GetMapping("/health")
	public String health() {
		return "OK";
	}
}
