program day1
  !use stdlib_sorting, only: sort
  implicit none
  integer, allocatable :: array1(:), array2(:)

  integer :: i, rivit, ios
  character(len = 100) :: rivi
  character(len = 10) :: tiedosto

  tiedosto = "paiva1.txt"

  OPEN(UNIT=10, FILE=tiedosto, STATUS="OLD", ACTION="READ")
  rivit = 0

  do
    read(10, '(A)', iostat = ios) rivi
    if(ios /= 0) exit
    rivit = rivit + 1
  end do
  close(10)
  print *, rivit

  allocate(array1(rivit), array2(rivit))

  OPEN(UNIT=10, FILE=tiedosto, STATUS="OLD", ACTION="READ")
  do i = 1, rivit
    read(10, *, iostat = ios) array1(i), array2(i)
    if(ios /= 0) then
      print *, "error rivillä ", i
      stop
    end if
  end do
  close(10)

  PRINT *, "Array 1:"
  PRINT *, array1
  PRINT *, "Array 2:"
  PRINT *, array2


  deallocate(array1)
  deallocate(array2)

end program day1
