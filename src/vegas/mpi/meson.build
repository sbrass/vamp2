load_balancer = library('request', ['balancer.f90',
                                    'balancer_simple.f90',
                                    'channel_balancer.f90',
                                    'request_callback.f90',
                                    'request.f90',
                                    'request_simple.f90',
                                    'request_caller.f90',
                                    'request_state.f90'],
                       link_with : utils)

vegas = library('vegas', ['vegas.f90',
                          'vamp2.f90'],
                link_with : [utils, rng, load_balancer])
