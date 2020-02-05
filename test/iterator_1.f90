program main
  use, intrinsic :: iso_fortran_env, only: ERROR_UNIT
  use iterator

  implicit none

  type(iterator_t) :: iter

  call iter%init (1, 10)
  call iter%write (ERROR_UNIT)
  do while (iter%is_iterable ())
     write (ERROR_UNIT, "(A,1X,I3)") "NDX", iter%next ()
  end do

  call iter%init (10, 1, -1)
  call iter%write (ERROR_UNIT)
  do while (iter%is_iterable ())
     write (ERROR_UNIT, "(A,1X,I3)") "NDX", iter%next ()
  end do
  write (ERROR_UNIT, "(A,1X,I3)") "INVALID NDX", iter%next ()

  call iter%init (1, 10)
  call iter%write (ERROR_UNIT)
  do while (iter%is_iterable ())
     call iter%next_step ()
     write (ERROR_UNIT, "(A)") "STEP."
  end do
end program main
