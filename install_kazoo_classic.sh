#!/bin/bash

# Set GitHub repository information
REPO_OWNER="kazoo-classic"
REPO_NAME="kazoo"

# Create temp directory
TEMP_DIR=$(mktemp -d)
cd "$TEMP_DIR" || exit 1

echo "Fetching latest release information..."
# Get latest release info using GitHub API
LATEST_RELEASE=$(curl -s "https://api.github.com/repos/$REPO_OWNER/$REPO_NAME/releases/latest")
RELEASE_TAG=$(echo "$LATEST_RELEASE" | grep -o '"tag_name": "[^"]*' | cut -d'"' -f4)

echo "Latest release: $RELEASE_TAG"

# Function to download assets - fixed version
download_assets() {
  echo "Downloading RPM packages..."
  
  # Extract download URLs more reliably
  DOWNLOAD_URLS=$(echo "$LATEST_RELEASE" | grep -o '"browser_download_url": "[^"]*' | cut -d'"' -f4)
  
  # Check if we got any URLs
  if [ -z "$DOWNLOAD_URLS" ]; then
    echo "ERROR: Failed to find download URLs in the GitHub API response"
    echo "API Response excerpt:"
    echo "$LATEST_RELEASE" | head -n 30
    exit 1
  fi
  
  # Download each file
  echo "$DOWNLOAD_URLS" | while read -r url; do
    filename=$(basename "$url")
    echo "Downloading: $filename"
    curl -L -o "$filename" "$url"
    if [ ! -f "$filename" ]; then
      echo "ERROR: Failed to download $filename"
    fi
  done
  
  # Verify downloads
  echo "Downloaded files:"
  ls -la
}

# Function to install packages in specified order
install_packages() {
  echo "Installing EPEL repository..."
  dnf install -y epel-release
  
  # Install packages in correct order
  echo "Installing packages in the required order..."
  
  # 1. Install erlang-otp first
  if [ -f "erlang-otp-"*".rpm" ]; then
    echo "Installing Erlang OTP..."
    dnf install -y ./erlang-otp-*.rpm || { echo "Failed to install Erlang OTP"; exit 1; }
  else
    echo "ERROR: Erlang OTP package not found. Files in directory:"
    ls -la
    exit 1
  fi
  
  # 2. Install rebar next
  if [ -f "rebar-"*".rpm" ]; then
    echo "Installing Rebar..."
    dnf install -y ./rebar-*.rpm || { echo "Failed to install Rebar"; exit 1; }
  else
    echo "ERROR: Rebar package not found. Aborting."
    exit 1
  fi
  
  # 3. Install elixir
  if [ -f "elixir-"*".rpm" ]; then
    echo "Installing Elixir..."
    dnf install -y ./elixir-*.rpm || { echo "Failed to install Elixir"; exit 1; }
  else
    echo "ERROR: Elixir package not found. Aborting."
    exit 1
  fi
  
  # 4. Finally install kazoo-classic
  if [ -f "kazoo-classic-"*".rpm" ]; then
    echo "Installing Kazoo..."
    dnf install -y ./kazoo-classic-*.rpm || { echo "Failed to install Kazoo"; exit 1; }
  else
    echo "ERROR: Kazoo package not found. Aborting."
    exit 1
  fi
}

# Main execution
download_assets
install_packages

echo "Installation complete! Kazoo has been installed successfully."
cd - > /dev/null
rm -rf "$TEMP_DIR"