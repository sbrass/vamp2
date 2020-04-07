benchmark_utils = library('benchmark_utils', ['benchmark_handler.f90',
                              'benchmark_func.f90'],
                    link_with : [utils,
                                 rng,
                                 vegas,
                                 load_balancer,
                                 signal],
                    link_language: 'fortran')

benchmark_1 = executable('benchmark_1', 'benchmark_1.f90',
                         link_with : [utils,
                                      rng,
                                      load_balancer,
                                      benchmark_utils])
