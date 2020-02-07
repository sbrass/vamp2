load_balancer = library('request', ['partition.f90',
                                    'resources.f90',
                                    'request_balancer.f90',
                                    'request_callback.f90',
                                    'request.f90',
                                    'request_simple.f90',
                                    'request_caller.f90',
                                    'request_state.f90',
                                    'channel_balancer.f90'],
                       link_with : utils)

vegas = library('vegas', ['vegas.f90',
                          'vamp2.f90'],
                link_with : [utils, rng, load_balancer])
