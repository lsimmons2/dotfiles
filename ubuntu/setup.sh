#not a script yet, just list of commands used


#emacs
sudo -s
apt-get install emacs24
apt-get install git
apt-get install
exit

#nvm
curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.32.1/install.sh | bash


#allow node listening on ports < 1024
sudo setcap cap_net_bind_service=+ep /home/ubuntu/.nvm/versions/node/v6.2.2/bin/node

#disable default nginx
sudo update-rc.d -f nginx disable


sudo su -c "env PATH=$PATH:/home/ubuntu/.nvm/versions/node/v6.2.2/bin pm2 startup ubuntu -u ubuntu --hp /home/ubuntu"
pm2 save


gulp prod && NODE_ENV=prod pm2 start dist/server/app.js
