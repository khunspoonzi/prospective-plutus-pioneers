# Homework

The following steps were carried out on a Linux-based operating system.

## Clone the Plutus Repo

1. Open a new terminal window
2. Navigate to the directory that will house the Plutus repo:

    `cd /path/where/i/want/plutus/`

3. Clone the Plutus repo from GitHub:

    `git clone git@github.com:input-output-hk/plutus.git`

4. Navigate into Plutus repo:

    `cd plutus/`

5. Checkout the lecture-related tag:

    `git checkout ea0ca4e9f9821a9dbfc5255fa0f42b6f2b3887c4`

## Install Nix

1. Open a new terminal window
2. Download and install Nix:

    `sudo curl -L https://nixos.org/nix/install | sh`

    **Note:** Nix will place its /nix/store directory -- which can grow very large -- in your root directory.
3. If you are low on disk space in your root partition, try to mount your Nix store elsewhere:
    - Navigate to a directory on a partition or drive with a lot of space:

        `cd /path/to/spacious/dir/`

    - Make a new Nix store in your spacious directory:

        `mkdir -p nix/store/`

    - Copy installation files to new Nix store:

        `sudo cp -a /nix/store/. /path/to/spacious/dir/nix/store/`

    - Remove installation files from old Nix store in root directory:

        `sudo rm -rf /nix/store/*`

    - Mount your spacious Nix store to your root Nix store.

        `sudo mount --bind /path/to/spacious/dir/nix/store /nix/store`

    More info [here](https://forum.holochain.org/t/install-nix-to-custom-location/4221)


**Note:** Follow the instructions outlined [here](https://github.com/NixOS/nix/issues/1402) should you need to uninstall Nix.

## Set Up IOHK Binary Caches

1. Open a new terminal window
2. If /etc/nix/nix.conf does not exist:
   - Navigate to /etc/ directory:

       `cd /etc/`

   - Create a nix/nix.conf file:

       `sudo mkdir nix/ && sudo touch nix/nix.conf && cd nix/`

3. Add the following lines to your nix.conf file (e.g. using `sudo gedit nix.conf`):
```
substituters        = https://hydra.iohk.io https://iohk.cachix.org https://cache.nixos.org/
trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
```


## Enter a Nix Shell

1. Open a new terminal window
2. Navigate to the Plutus repository:

    `cd /path/to/plutus/`

3. If you use Fish command line:
    - Invoke bash since Nix does not like Fish:

        `bash -cl fish`

    **Note:** See this [discussion](https://github.com/NixOS/nix/issues/1512) for more info on actually getting Nix to work with Fish.

4. Enter a Nix shell:

    `nix-shell`

5. Wait for Nix shell to initialize

    **Note:** This can take 30 - 40 minutes and will consume more than 10 GB in your nix/store/ directory
