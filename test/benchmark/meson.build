benchmark_utils = library('benchmark_utils', ['benchmark_handler.f90',
                                              'benchmark_func.f90'],
                          link_with : [utils,
                                       signal,
                                       rng,
                                       vegas,
                                       load_balancer],
                         dependencies : mpi)

benchmark_1 = executable('benchmark_1', 'benchmark_1.f90',
                         link_with : [utils,
                                      test_utils,
                                      signal,
                                      rng,
                                      load_balancer,
                                      vegas,
                                      benchmark_utils],
                         dependencies : mpi,
                         link_language : 'fortran')
