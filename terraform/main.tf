provider "google" {
  project = "nixpkgs-graph-explorer"
  region  = "us-west1"
  zone    = "us-west1-a"
}

resource "google_compute_network" "vpc_network" {
  project                 = "nixpkgs-graph-explorer"
  name                    = "nixpkgs-graph-explorer-network"
  auto_create_subnetworks = false
  mtu                     = 1460
}

resource "google_compute_subnetwork" "default" {
  name          = "nixpkgs-graph-explorer-subnet"
  ip_cidr_range = "192.168.1.0/24"
  region        = "us-west1"

  stack_type       = "IPV4_IPV6"
  ipv6_access_type = "EXTERNAL"
  
  network       = google_compute_network.vpc_network.id
}

resource "google_compute_disk" "default" {
  name = "compute-disk"
  size = "10"
}

resource "google_compute_instance_group" "default" {
  name      = "nixpkgs-graph-explorer-instance-group"
  zone      = "us-west1-a"
  instances = [google_compute_instance.default.self_link]

  # lifecycle {
  #   create_before_destroy = true
  # }

  named_port {
    name = "http"
    port = 5000
  }
}

# Create a single Compute Engine instance
resource "google_compute_instance" "default" {
  name         = "nixpkgs-graph-explorer-vm-1"
  machine_type = "e2-medium"
  zone         = "us-west1-a"
  tags         = ["ssh", "ssh-ipv6", "http-server", "https-server", "firewall-web"]

  boot_disk {
    initialize_params {
      image = "ubuntu-os-pro-cloud/ubuntu-pro-2004-lts"
    }
  }

  network_interface {
    network = "default"

    access_config {
      # Include this section to give the VM an external IP address
    }
  }

  scheduling {
    preemptible        = true
    automatic_restart  = false
    provisioning_model = "SPOT"
  }

  metadata_startup_script = "${file("./install.docker.sh")}"

  lifecycle {
    ignore_changes = [attached_disk]
  }

  service_account {
    # Google recommends custom service accounts that have cloud-platform scope and permissions granted via IAM Roles.
    email  = "cloud-storage-bucket@nixpkgs-graph-explorer.iam.gserviceaccount.com"
    scopes = ["cloud-platform"]
  }
}

# connect compute & disk
resource "google_compute_attached_disk" "default" {
  disk     = google_compute_disk.default.id
  instance = google_compute_instance.default.id
}

resource "google_compute_firewall" "ssh" {
  name = "allow-ssh"
  allow {
    ports    = ["22"]
    protocol = "tcp"
  }
  direction     = "INGRESS"
  network       = "default"
  priority      = 1000
  source_ranges = ["176.158.157.138/32"]
  target_tags   = ["ssh"]
}

resource "google_compute_firewall" "ssh-ipv6" {
  name = "allow-ssh-ipv6"
  allow {
    ports    = ["22"]
    protocol = "tcp"
  }
  direction     = "INGRESS"
  network       = "default"
  priority      = 1000
  source_ranges = ["2001:171b:c9b5:6230:cca9:6c33:ef44:3097/128"]
  target_tags   = ["ssh-ipv6"]
}

resource "google_compute_firewall" "web" {
  name    = "firewall-web"
  network = "default"
  allow {
    protocol = "tcp"
    ports    = ["5000"]
  }
  target_tags   = ["firewall-web"]
  source_ranges = ["0.0.0.0/0"]
}
