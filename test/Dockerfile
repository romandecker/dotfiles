FROM ubuntu

RUN apt-get update
RUN apt-get install sudo
RUN useradd -ms /bin/bash alice
RUN usermod -aG sudo alice
RUN echo "alice\nalice" | passwd alice

COPY workspace /home/alice/.dotfiles
COPY sudoers /etc/sudoers
WORKDIR /home/alice
RUN chown -R alice:alice /home/alice/.dotfiles
