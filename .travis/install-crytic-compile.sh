sudo add-apt-repository ppa:jonathonf/python-3.6 -y
sudo apt-get update
sudo apt-get install python3.5 python3.6 python3-setuptools -y

git clone https://github.com/crytic/crytic-compile --depth 1
cd crytic-compile
python3.6 setup.py install --user
cd ..
