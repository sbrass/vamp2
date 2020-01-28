project('vamp2', 'fortran',
  version : '0.1',
  default_options : ['warning_level=3'])

utils = library('utils', ['src/constants.f90',
                          'src/kinds.f90',
                          'src/io_units.f90',
                          'src/diagnostics.f90',
                          'src/format_defs.f90',
                          'src/format_utils.f90',
                          'src/numeric_utils.f90'])

rng = library('rng', ['src/rng_base.f90',
                      'src/rng_stream.f90'],
              link_with : utils)

cp = find_program('cp')
gen = generator(cp,
                output  : '@BASENAME@.f90',
                arguments : ['@INPUT@', '@OUTPUT@'])
vegas_src = gen.process('src/vegas.f90_serial')
vamp2_src = gen.process('src/vamp2.f90_serial')

vegas = library('vegas', [vegas_src, vamp2_src], link_with : [utils, rng])

vegas_ut = executable('vegas_ut', ['test/vegas_ut.f90', 'test/vegas_uti.f90'], link_with : [utils, rng, vegas])
test('VEGAS.', vegas_ut)
vamp2_ut = executable('vamp2_ut', ['test/vamp2_ut.f90', 'test/vamp2_uti.f90'], link_with : [utils, rng, vegas])
test('VAMP2.', vamp2_ut)
