resource "google_compute_global_address" "default" {
  project      = "nixpkgs-graph-explorer"
  name         = "nixpkgs-graph-explorer-address"
  ip_version   = "IPV4"
  address_type = "EXTERNAL"
}

resource "google_compute_global_forwarding_rule" "https" {
  name       = "lb-https-rule"
  target     = google_compute_target_https_proxy.default.self_link
  ip_address = google_compute_global_address.default.address
  port_range = "443"
  depends_on = [google_compute_global_address.default]
}

resource "google_compute_target_https_proxy" "default" {
  name             = "lb-https-proxy"
  url_map          = google_compute_url_map.default.self_link
  ssl_certificates = google_compute_ssl_certificate.certificate.*.self_link
}

resource "google_compute_url_map" "default" {
  name        = "lb-url-map"
  description = "url map for lb"

  default_service = google_compute_backend_service.default.id

  host_rule {
    hosts        = ["*"]
    path_matcher = "allpaths"
  }

  path_matcher {
    name            = "allpaths"
    default_service = google_compute_backend_service.default.id

    path_rule {
      paths   = ["/*"]
      service = google_compute_backend_service.default.id
    }
  }
}

resource "google_compute_backend_service" "default" {
  name        = "backend-service"
  port_name   = "http"
  protocol    = "HTTP"
  timeout_sec = 10
  enable_cdn  = false

  backend {
    group = google_compute_instance_group.default.self_link
  }

  health_checks = [google_compute_health_check.default.self_link]

  depends_on = [google_compute_instance_group.default]

}


resource "google_compute_health_check" "default" {
  name    = "lb-http-hc"

  http_health_check {
    port         = 5000
    request_path = "/"
  }

  check_interval_sec = 5
  timeout_sec        = 5
}

# ------------------------------------------------------------------------------
# IF DNS ENTRY REQUESTED, CREATE A RECORD POINTING TO THE PUBLIC IP OF THE CLB
# ------------------------------------------------------------------------------

resource "google_dns_record_set" "dns" {
  name = "nixpkgs-graph.app."
  type = "A"
  ttl  = 300

  managed_zone = google_dns_managed_zone.default.name

  rrdatas = [google_compute_global_address.default.address]
}

resource "google_dns_managed_zone" "default" {
  name     = "default-zone"
  dns_name = "nixpkgs-graph.app."
}
