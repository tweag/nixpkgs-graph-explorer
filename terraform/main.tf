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
  network       = google_compute_network.vpc_network.id
}

resource "google_compute_address" "static" {
  name   = "ipv4-address"
  region = "us-west1"
}

# Create a single Compute Engine instance
resource "google_compute_instance" "instance_with_ip" {
  name         = "nixpkgs-graph-explorer-vm-1"
  machine_type = "e2-small"
  zone         = "us-west1-a"
  tags         = ["ssh"]

  boot_disk {
    initialize_params {
      image = "ubuntu-os-pro-cloud/ubuntu-pro-2004-lts"
    }
  }

  network_interface {
    network = "default"

    access_config {
      # Include this section to give the VM an external IP address
      nat_ip = google_compute_address.static.address
    }
  }

  scheduling {
    preemptible       = true
    automatic_restart = false
  }

  metadata_startup_script = "${file("./install.docker.sh")}"
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
  source_ranges = ["0.0.0.0/0"]
  target_tags   = ["ssh"]
}

resource "google_compute_firewall" "postgresql" {
  name    = "firewall-postgresql"
  network = "default"
  allow {
    protocol = "tcp"
    ports    = ["5432"]
  }
  target_tags   = ["firewall-postgresql"]
  source_ranges = ["0.0.0.0/0"]
}

resource "google_compute_firewall" "gremlin" {
  name    = "firewall-gremlin"
  network = "default"
  allow {
    protocol = "tcp"
    ports    = ["8182"]
  }
  target_tags   = ["firewall-gremlin"]
  source_ranges = ["0.0.0.0/0"]
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