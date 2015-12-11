# R image with dev libs &  packages for GIS
# =========================================

FROM rocker/hadleyverse

MAINTAINER "Maarten Hermans" maarten@mhermans.net

# apt-get packages
# ----------------
RUN apt-get update && apt-get install -y --no-install-recommends \
    tk-dev \
    libgdal-dev \ 
    libimage-exiftool-perl \
    libproj-dev \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/ \
  && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

# R packages
# ----------
RUN install2.r --error --deps TRUE \
    rgdal \
    lubridate \
    plotKML \
  && rm -rf /tmp/downloaded_packages/

# R Github repo's 
# ----------------
RUN installGithub.r rstudio/leaflet \
&& rm -rf /tmp/downloaded_packages/

WORKDIR /home/rstudio/
