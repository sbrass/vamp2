unit_test = find_program('unit_test.py')

test_utils = library('test_utils', 'test_utils.f90',
                    link_with : utils)

signal_1 = executable('signal_1', ['signal_1.f90'],
                      link_with : signal)
queue_1 = executable('queue_1', 'queue_1.f90',
                     link_with : utils)
array_list_1 = executable('array_list_1', 'array_list_1.f90',
                          link_with : utils)
iterator_1 = executable('iterator_1', 'iterator_1.f90',
                        link_with : utils)
binary_tree_1 = executable('binary_tree_1', 'binary_tree_1.f90',
                           link_with : utils)
# logging_1 = executable('logging_1', 'logging_1.f90',
#                        link_with : utils)

test('Signal.', signal_1)
test('Simple queue.', queue_1)
test('Array List.', array_list_1)
test('Iterator.', iterator_1)
test('Binary tree.', binary_tree_1, should_fail : true)

if mpi_enabled
  result_handler = library('result_handler', 'result_handler.f90',
                           link_with : load_balancer)

  mpi_caller_1 = executable('mpi_caller_1', 'mpi_caller_1.f90',
                            link_with : [load_balancer,
                                         signal,
                                         test_utils,
                                         utils],
                            include_directories : inc,
                            link_language : 'fortran')
  mpi_caller_2 = executable('mpi_caller_2', 'mpi_caller_2.f90',
                            link_with : [load_balancer,
                                         result_handler,
                                         signal,
                                         test_utils,
                                         utils],
                            link_language : 'fortran')
  mpi_caller_3 = executable('mpi_caller_3', 'mpi_caller_3.f90',
                            link_with : [load_balancer,
                                         result_handler,
                                         signal,
                                         test_utils,
                                         utils],
                            link_language : 'fortran')
  balancer_1 = executable('balancer_1', 'balancer_1.f90',
                          link_with : [load_balancer,
                                       utils,
                                       test_utils,
                                       signal],
                          link_language : 'fortran')
  balancer_2 = executable('balancer_2', 'balancer_2.f90',
                          link_with : [load_balancer,
                                       utils,
                                       test_utils,
                                       signal],
                          link_language : 'fortran')
  channel_1 = executable('channel_1', 'channel_1.f90',
                         link_with : [load_balancer, test_utils])
  channel_2 = executable('channel_2', 'channel_2.f90',
                         link_with : [load_balancer, test_utils])
  mpi_simple_1 = executable('mpi_simple_1', 'mpi_simple_1.f90',
                            link_with : [load_balancer,
                                         utils,
                                         test_utils,
                                         signal],
                            link_language : 'fortran')
  mpi_simple_2 = executable('mpi_simple_2', 'mpi_simple_2.f90',
                            link_with : [load_balancer,
                                         utils,
                                         test_utils,
                                         signal],
                            link_language : 'fortran')
  mpi_simple_3 = executable('mpi_simple_3', 'mpi_simple_3.f90',
                            link_with : [load_balancer,
                                         utils,
                                         result_handler,
                                         test_utils,
                                         signal],
                            link_language : 'fortran')
  mpi_switch = executable('mpi_switch', 'mpi_switch.f90',
                          link_with : [load_balancer,
                                       utils,
                                       rng,
                                       result_handler,
                                       test_utils,
                                       signal],
                          link_language : 'fortran')

  test('Request balancer initialization.', balancer_1)
  test('Request balancer add partition.', balancer_2)
  test('Basic interface and channel balancing.', channel_1)
  test('Apply channel balancing.', channel_2)
  test('Simple Request.', mpi_simple_1)
  test('Simple Request (iterated).', mpi_simple_2)

  vegas_mpi_ut = executable('vegas_mpi_ut', 'vegas_mpi_ut.f90',
                            link_with : [utils, rng, vegas])
  # test('VEGAS (MPI).', unit_test, args : [vegas_mpi_ut, ref_vegas_mpi])
  vamp2_mpi_ut = executable('vamp2_mpi_ut', 'vamp2_mpi_ut.f90',
                            link_with : [utils,
                                         rng,
                                         vegas,
                                         load_balancer,
                                         test_utils,
                                         signal],
                            link_language : 'fortran')
  # test('VAMP2 (MPI).', unit_test, args : [vamp2_mpi_ut, ref_vamp2_mpi])
else
  vegas_ut = executable('vegas_ut', ['vegas_ut.f90',
                                     'vegas_uti.f90'],
                        link_with : [utils,
                                     rng,
                                     test_utils,
                                     vegas])
  test('VEGAS.', unit_test, args : [vegas_ut, ref_vegas])

  vamp2_ut = executable('vamp2_ut', ['vamp2_ut.f90',
                                     'vamp2_uti.f90'],
                        link_with : [utils,
                                     rng,
                                     test_utils,
                                     vegas,
                                     signal],
                        link_language : 'fortran')
  test('VAMP2.', unit_test, args : [vamp2_ut, ref_vamp2])
endif

# mpi_comm_dup = executable('mpi_comm_dup', 'mpi_comm_dup.f90')

# ice_gfortran = executable('ice-gfortran', 'ice-gfortran.f90')
