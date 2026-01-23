# title       = Setting up jjvcs collaboration between local machines via ssh
# pubDate     = 2025-12-22
# tags        = fish, jjvcs, ssh, workflow, 2025
# description = about how i collaborate between 2 ⟅laptops|identities|personae⟆ using jjvcs & ssh without hosting services.

# | Introduxion
#
# This blog provides instruxions for using [jjvcs](https://www.jj-vcs.dev/latest/) to collaborate between two laptops (|host| and |guest|)
# over [ssh](https://en.wikipedia.org/wiki/Secure_Shell), without relying on github or similar hosting services. Essentially this approach
# should apply to any number of local machines.
#
# Note that this blog is targeted at macos. It relies on [mDNS](https://en.wikipedia.org/wiki/Multicast_DNS) to announces a machine's
# hostname and ip via multicast dns on every network it joins, so other machines can automatically resolve `.local` hostnames without
# manual dns configuration.
#
# We use |(host/guest)| to indicate the commands in this section shall be run on the host/guest laptop.
#
# | The ssh part
#
# || Ensure the ssh server is running (host)

# check if ssh is enabled
sudo systemsetup -getremotelogin
# if not enabled, enable it
# sudo systemsetup -setremotelogin on

# || Sudo set hostname (host)
#
# The formation of this section shall allow us to use `hostname.local` addresses that automatically resolve on any local network without manual configuration,
# so we needn't reconfigure ip addresses on new networks.

# verify your local hostname
scutil --get LocalHostName

# This returns something like MacBook (without the .local suffix).

# We can set a memorable hostname:

sudo scutil --set LocalHostName host

# This machine will now be reachable at `host.local` on any local network.

# || Verify mDNS is working (guest)

ping host.local

# || Generate an ssh key (guest)

ssh-keygen -t ed25519 -C "your_email@example.com"

# || Copy guest's public key to host (guest)

# substitute username with the output of `whoami`
ssh-copy-id username@host.local

# || Test the connection (guest)

ssh username@host.local

# Note that you don't need to ssh into the host in the following steps.

# | The jj part
#
# || Initialize a jj repo (host)
#
# Let's assume that we put all of our projects under `~/dev`. We create a new project `jjssh` here:

cd ~/dev && mkdir jjssh && jj git init --colocate

# We make some dummy changes:

echo "initial content" >readme

# And commit them:

jj ci -m "first commit" && jj b s main -r @-

# So far so good. But unfortunately, we can't directly use this repo for ssh-based collaboration. Instead, we need a bare git repo.
#
# || Create a bare repo (host)
#
# We need to create a bare repo that will serve as the remote. We will put all our bare repos under `~/remotes`.

mkdir -p ~/remotes && cd ~/remotes && git init --bare jjssh.git

# || Push jj repo to bare repo (host)
#
# We can now add the bare repo as origin push the bookmark to it:

cd ~/dev/jjssh && jj git remote add origin ~/remotes/jjssh.git
jj b t main --remote origin && jj git push -b main --remote origin

# The workflow should henceforth be analogous to that in normal jj.

# || Clone the bare repo (guest)

cd ~/dev && jj git clone --colocate ssh://username@host.local/~/remotes/jjssh.git jjssh

# || Make changes and push the bookmark (guest)

cd ~/dev/jjssh && jj new main@origin
echo "changes from guest" >>readme && jj ci -m "changes from guest"
jj b s guest -r @- && jj b t guest --remote origin && jj git push -b guest --remote origin

# || Fetch commits (host)

cd ~/dev/jjssh && jj git fetch --remote origin

# | My shortcut, a jjinit function to automate the project setup

function jjinit
    set project_name $argv[1]
    set project_dir ~/dev/$project_name
    set remote_dir ~/remotes/$project_name.git

    mkdir -p $project_dir
    cd $project_dir

    jj git init --colocate

    mkdir -p ~/remotes
    git init --bare $remote_dir

    jj git remote add origin $remote_dir
end
