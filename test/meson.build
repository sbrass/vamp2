unit_test = find_program('unit_test.py')

vegas_ut = executable('vegas_ut', ['vegas_ut.f90',
                                   'vegas_uti.f90'],
                      link_with : [utils, rng, vegas])
test('VEGAS.', unit_test, args : [vegas_ut, ref_vegas])

vamp2_ut = executable('vamp2_ut', ['vamp2_ut.f90',
                                   'vamp2_uti.f90'],
                      link_with : [utils, rng, vegas])
test('VAMP2.', unit_test, args : [vamp2_ut, ref_vamp2])
