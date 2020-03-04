subdir('utils')
subdir('rng')
if mpi_enabled
  subdir('vegas/mpi')
  vamp2 = library('vamp2', link_with : [vegas,
                                        load_balancer,
                                        utils,
                                        rng])
else
  subdir('vegas/serial')
  vamp2 = library('vamp2', link_with : [vegas,
                                        utils,
                                        rng])
endif
