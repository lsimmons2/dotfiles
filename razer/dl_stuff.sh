#install nvidia container toolkit - at nvidia-docker github repo
distribution=$(. /etc/os-release;echo $ID$VERSION_ID)
echo $distribution
curl -s -L https://nvidia.github.io/nvidia-docker/gpgkey | sudo apt-key add -
curl -s -L https://nvidia.github.io/nvidia-docker/$distribution/nvidia-docker.list | sudo tee /etc/apt/sources.list.d/nvidia-docker.list
echo "added repos"

sudo apt-get update && sudo apt-get install -y nvidia-container-toolkit
echo "restarting docker"
sudo systemctl restart docker
echo "restarted docker"


https://git-scm.com/book/en/v2/Getting-Started-Getting-Help
