project('vamp2', ['fortran', 'c'],
  version : '0.1',
  default_options : ['debug=true', 'optimization=0', 'warning_level=3'],
  license : 'GPLv2')

# cp = find_program('cp')
# gen = generator(cp,
#                 output  : '@BASENAME@.f90',
#                 arguments : ['@INPUT@', '@OUTPUT@'])
# vegas_src = gen.process('src/vegas.f90_serial')
# vamp2_src = gen.process('src/vamp2.f90_serial')

add_project_arguments('-cpp', language : 'fortran')
add_project_arguments('-fbacktrace', language : 'fortran')

subdir('include')
subdir('src')
subdir('share')
subdir('test')
