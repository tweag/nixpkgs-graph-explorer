# Update the apt package index and install packages to allow apt to use a repository over HTTPS
sudo apt update
sudo apt install apt-transport-https ca-certificates curl software-properties-common

# Add Docker’s official GPG key
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add -

# The following command is to set up the stable repository
sudo add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/ubuntu `lsb_release -cs` stable"

# Update the apt package index, and install the latest version of Docker Engine and containerd, or go to the next step to install a specific version
sudo apt update
sudo apt install -y docker-ce

# Install the proper version of docker-compose
curl -O -J -L https://github.com/docker/compose/releases/download/v2.11.2/docker-compose-linux-x86_64
chmod +x docker-compose-linux-x86_64
sudo mv ./docker-compose-linux-x86_64 /usr/bin/docker-compose
sudo apt update

# Install Nix
sh <(curl -L https://nixos.org/nix/install) --no-daemon
 . /home/\$USER/.nix-profile/etc/profile.d/nix.sh

# Get the application source code from the Google Cloud Storage bucket.
mkdir /nixpkgs-graph-explorer
sudo gsutil -m cp -r gs://codebase-nixpkgs-graph-explorer/nixpkgs-graph-explorer/* /nixpkgs-graph-explorer/
chmod -R 777 /nixpkgs-graph-explorer
