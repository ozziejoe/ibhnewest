FROM wahiduzzaman007/ubuntu-focal-r:4.3.1

# copy the app to the image
RUN mkdir /app
COPY "app/." /app

# install dependencies
RUN apt-get update && apt-get install -y libxml2-dev libglpk-dev libglpk40 libsodium-dev

# install all necessary packages with renv
WORKDIR /app
ENV RENV_PATHS_LIBRARY renv/library
RUN R -e "renv::restore()"

EXPOSE 3838
CMD ["R", "-e", "shiny::runApp('/app', port = 3838, host = '0.0.0.0')"]