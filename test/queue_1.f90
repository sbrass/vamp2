program main
  use iso_fortran_env, only: ERROR_UNIT

  use queue

  implicit none

  type(queue_t) :: queue

  integer :: i, j

  call queue%write (ERROR_UNIT)
  do i = 1, 15
     call queue%enqueue (i)
  end do
  call queue%write (ERROR_UNIT)
  do i = 1, 15
     j = queue%dequeue ()
     write (ERROR_UNIT, "(A,1X,I2,1X,I2)") "Dequeue", i, j
  end do
  call queue%write (ERROR_UNIT)
end program main
