#!bin/bash

if [ -f /etc/lsb-release ]; then
	apt-get install -y libmagick++-dev  
else
	yum install ImageMagick-c++-devel
fi


