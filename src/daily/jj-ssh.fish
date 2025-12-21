# title       = Setting up jjvcs collaboration between local machines via ssh
# pubDate     = 2025-12-21
# tags        = jjvcs, ssh, workflow
# description =

# | Introduction
# 
# This blog provides step-by-step instruxions for using [jjvcs] to collaborate between two laptops (|host| and |guest|) over [ssh], without
# relying on github, codeberg, or similar hosting services. Essentially this approach should apply to any number of local machines.
# 
# Note that this blog is targeted at macos. It relies on [mDNS] to announces a machine's hostname and ip via multicast dns on every
# network it joins, so other machines can automatically resolve `.local` hostnames without manual dns configuration. In other words, 
# the approach works immediately on any wifi network without updating ip addresses.
# 
# | Setup hostname
# 
# This step allows us to use `hostname.local` addresses that automatically resolve on any local network without configuration,
# so we needn't reconfigure ip addresses on new networks.
#
# || Setup hostname (host) 

# verify your local hostname
scutil --get LocalHostName

# This returns something like MacBook (without the .local suffix).

# set a memorable hostname if desired
sudo scutil --set LocalHostName host

# This machine will now be reachable at `host.local` on any local network.

# || Verify mDNS is working (guest) 

ping host.local

# | Configure ssh access 
# 
# || Ensure the ssh server is running (host)

# check if ssh is enabled
sudo systemsetup -getremotelogin
# if not enabled, enable it
sudo systemsetup -setremotelogin on

# || Generate an ssh key (guest)

ssh-keygen -t ed25519 -C "your_email@example.com"

# || Copy guest's public key to host (guest)

# substitute username with the output of `whoami`
ssh-copy-id username@host.local

# || Test the connection (guest)
ssh username@host.local

# Note that you don't need to ssh into the host in the following steps.

# | Repo workflow
#
# || Initialize the jj repo (host)
# 
# Let's assume that we put all of our projects under `~/dev`. We create a new project `jjssh` here:

cd ~/dev && mkdir jjssh && jj git init --colocate

# We make some dummy changes:

echo "initial content" >readme

jj ci -m "first commit" && jj bookmark s main -r @-

# So far so good. But unfortunately, we can't directly use this repo for ssh-based collaboration. Instead, we need a bare git repo.
# 
# || Create a bare repo (host)
# 
# We need to create a bare repo that will serve as the remote. We will put all our bare repos under `~/dev-remote`.

mkdir -p ~/dev-remote && cd ~/dev-remote

git init --bare jjssh.git

# || Push jj repo to bare repo (host)

cd ~/dev/jjssh && jj git remote add origin ~/dev-remote/jjssh.git

jj bookmark t main@origin && jj git push -b main

# || Clone the bare repo (guest)

cd ~/dev && jj git clone --colocate ssh://username@host.local/~/dev-remote/jjssh.git jjssh

# || Make commits (guest)

cd ~/dev/jjssh

jj new main@origin

echo "changes from guest" >>readme

jj ci -m "changes from guest"

jj bookmark s guest -r @- && jj bookmark t guest@origin

jj git push -b guest

# || Fetch commits (host)

cd ~/dev/jjssh && jj git fetch

# Everything should be fine.

# | A jj-init script to do them all
