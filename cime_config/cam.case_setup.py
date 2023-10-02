#! /usr/bin/env bash

# This script is run from CIME when calling case.setup

cam_root = $1
case_root = $2
cam_config = case.get_value('CAM_CONFIG_OPTS')

# GEOS-Chem only: copy config files to case
if '-chem geoschem' in cam_config:
   geoschem_config_src = os.path.join(cam_root, '/src/chemistry/geoschem/geoschem_src/run/CESM')
   if not os.path.isdir(geoschem_config_src):
      raise SystemExit("ERROR: Did not find path to GEOS-Chem source code at {:s}".format(geoschem_src))
   for fileName in ['species_database.yml', 'geoschem_config.yml', 'HISTORY.rc', 'HEMCO_Config.rc', 'HEMCO_Diagn.rc']:
       source_file = os.path.join(cam_root, geoschem_config_src, fileName)
       target_file = os.path.join(case_root, fileName)
       if not os.path.exists(target_file):
	  logger.info("CAM namelist one-time copy of GEOS-Chem run directory files: source_file %s target_file %s ",source_file, target_file)
	  shutil.copy(source_file, target_file)	   			         

