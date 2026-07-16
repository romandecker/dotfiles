# End-to-end test of the Linux bootstrap path: install chezmoi from scratch and
# run `chezmoi init --apply` against a copy of this repo, exactly as a fresh
# machine would. Build via test/run-test.sh (it stages ./workspace first).
FROM ubuntu:24.04

RUN apt-get update \
    && apt-get install -y curl sudo git ca-certificates zsh \
    && rm -rf /var/lib/apt/lists/*

RUN useradd -ms /bin/bash alice && usermod -aG sudo alice
COPY sudoers /etc/sudoers
COPY --chown=alice:alice workspace /home/alice/dotfiles-src

USER alice
WORKDIR /home/alice
ENV PATH="/home/alice/.local/bin:${PATH}"

# Install the chezmoi binary.
RUN sh -c "$(curl -fsLS get.chezmoi.io)" -- -b /home/alice/.local/bin

# Full bootstrap: reads .chezmoiroot, generates config (symlink mode), installs
# apt packages, fetches externals, links dotfiles, runs post-apply scripts.
RUN chezmoi init --apply --source=/home/alice/dotfiles-src

CMD ["/bin/bash"]
