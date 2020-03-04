project('vamp2', ['fortran', 'c'],
  version : '0.1',
  default_options : ['buildtype=debug',
                     'debug=true',
                     'optimization=0',
                     'warning_level=3'],
  license : 'GPLv2')

add_project_arguments('-cpp', language : 'fortran')
add_project_arguments('-fbacktrace', language : 'fortran')
if get_option('buildtype') == 'debug'
  message('add compiler option -fcheck=all')
  add_project_arguments('-fcheck=all', language : 'fortran')
endif

fortran_compiler = meson.get_compiler('fortran')

code = '''
program main
   implicit none
   print *, "Hello World!"
end program
'''
result = fortran_compiler.run(code, name : 'Basic Fortran Program')

code = '''
       module test
         use, intrinsic :: iso_fortran_env
       end module test
       '''
if not fortran_compiler.compiles(code, name : 'Fortran has module iso_fortran_env')
  error('Module iso_fortran_env not found.')
endif

## Polymorphism Check
## Finalization Check
code = '''
module test_final
  implicit none
  type :: test_t
    integer, pointer :: a => null ()
   contains
    final :: test_finalize
  end type test_t
contains
  subroutine test_finalize (obj)
    type(test_t), intent(inout) :: obj
    nullify (obj%a)
  end subroutine test_finalize
end module test_final
'''
result = fortran_compiler.compiles(code, name : 'Fortran Derived-Type Finalization Procedure')
if not result
  error('Fortran compiler does not support finalization procedure for derived types.')
endif


## Test MPI_F08
ompi = dependency('ompi', version : '>=2.0.0')
if get_option('mpi').enabled()
  if not ompi.found()
    error('OpenMPI not found.')
  endif
  code = '''
         module test
           use mpi_f08
         end module test
         '''
  if not fortran_compiler.compiles(code, name : 'OpenMPI has support for mpi_f08 module')
    error('Module mpi_f08 not found.')
  endif
  mpi_enabled = true
else
  mpi_enabled = false
endif

subdir('include')
subdir('src')
subdir('share')
subdir('test')
