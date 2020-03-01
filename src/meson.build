subdir('utils')
subdir('rng')
if mpi_enabled
  subdir('vegas/mpi')
else
  subdir('vegas/serial')
endif

vamp2 = library('vamp2', link_with : [vegas, utils, rng])
