vegas_ut = executable('vegas_ut', ['vegas_ut.f90',
                                   'vegas_uti.f90'],
                      link_with : [utils, rng, vegas])
test('VEGAS.', vegas_ut)

vamp2_ut = executable('vamp2_ut', ['vamp2_ut.f90',
                                   'vamp2_uti.f90'],
                      link_with : [utils, rng, vegas])
test('VAMP2.', vamp2_ut)
