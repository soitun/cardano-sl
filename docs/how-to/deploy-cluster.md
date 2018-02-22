# How to deploy cluster, prepare installers, propose an update

## Table of contents
  * [How to deploy developer cluster](#how-to-deploy-developer-cluster)
    + [Getting access to staging jumpserver](#getting-access-to-staging-jumpserver)
    + [Preparing branch and configuration](#preparing-branch-and-configuration)
    + [Creation of tmux session](#creation-of-tmux-session)
    + [Deployment and recommendations](#deployment-and-recommendations)

## How to deploy developer cluster

### Getting access to staging jumpserver
First of all you should make sure you have access to `staging` jumpserver via SSH. 
You can check it trying to connect to cluster `ssh staging@35.156.156.28`.
If you don't have access you should create ssh key add it to you ssh agent and then 
follow [this](https://github.com/input-output-hk/iohk-ops#getting-ssh-access) instruction.
It may take DevOps some time to give you access, ping them periodically.

### Preparing branch and configuration
The next step is you need to prepare branch and configuration for deploy.
For example you want to deploy release branch with smaller `k`, 
then you must create new branch based on this release one and modify `devnet` configuration in `lib/configuration.yaml` in this branch.
Sometimes you will need to create your own configuration, however,
most likely it will be enough to modify existed `devnet` configuration.
Then you should push your branch on github.

### Creation of tmux session
After it you should connect to `staging` jumpserver and create tmux session for the issue within which you are going to deploy cluster:
`tmux new -s issue-id` (for instance `tmux new -s csl228`). 
It should be done for the two reasons: to keep state of `nix-shell` between reconnections to `staging` jumpserver
and if something goes wrong (and it will go) you can ask devops to help you attaching to this session.

To attach to this session when you reconnect to `staging` server, you can use `tmux a -t issue-id`.

### Deployment and recommendations
The next steps are described [here](https://github.com/input-output-hk/internal-documentation/wiki/Developer-clusters-HOWTO).
Some notes about this instruction:
* `cardano-sl` git revision is most likely HEAD commit of your branch
* it's better to ask DevOps which `iohk-ops` branch you should use before deploy
* it's better specify `--bump-system-start-held-by` option for `io deploy`. 
The passed value depends on how long will deployment take: 
if `cardano-sl` is compiling from scratch, then it probably should be 15-20 mins, otherwise it's enough 5-10 mins. 
If deploy won't be done utill system start then the first slots will be without blocks and system most probably will die.
* it's better to obtain logs from one of node after you deployed the system to make sure deployment was successful.
