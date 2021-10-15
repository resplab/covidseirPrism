FROM opencpu/base
RUN add-apt-repository ppa:marutter/rrutter4.0
RUN add-apt-repository ppa:c2d4u.team/c2d4u4.0+
RUN apt update
RUN apt install -y r-cran-rstan
RUN apt-get install -y libv8-dev  
RUN R -e 'install.packages("rredis")'
RUN R -e 'install.packages("rstantools")'
RUN R -e 'remotes::install_github("aminadibi/covidseir")'
RUN R -e 'remotes::install_github("resplab/covidseirPrism")'
RUN echo  "{\"timelimit.post\":18000}" >  /etc/opencpu/server.conf.d/timeoverride.conf
#RUN sed -i "s|\"timelimit.post\": 90|\"timelimit.post\": 18000|" /etc/opencpu/server.conf
RUN echo "opencpu:opencpu" | chpasswd
