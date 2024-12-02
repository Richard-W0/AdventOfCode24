program day1
  USE ISO_FORTRAN_ENV, ONLY: INT32
  implicit none
  integer, allocatable :: array1(:), array2(:), tulos(:)

  integer :: i, rivit, ios, n, j, temp
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

  allocate(array1(rivit), array2(rivit), tulos(rivit))

  n = rivit

  OPEN(UNIT=10, FILE=tiedosto, STATUS="OLD", ACTION="READ")
  do i = 1, rivit
    read(10,'(i6, 1x, i6)', iostat = ios) array1(i), array2(i)
    if(ios /= 0) then
      print *, "error rivillä ", i
      stop
    end if
  end do
  close(10)

  do i = 1, n - 1
    do j = 1, n - i
      if (array1(j) > array1(j + 1)) then
      temp = array1(j)
      array1(j) = array1(j + 1)
      array1(j + 1) = temp
      end if
    end do
  end do

  do i = 1, n - 1
    do j = 1, n - i
      if (array2(j) > array2(j + 1)) then
      temp = array2(j)
      array2(j) = array2(j + 1)
      array2(j + 1) = temp
      end if
    end do
end do

  do i = 1, n
    tulos(i) = abs(array1(i) - array2(i))
  end do

  print *, sum(tulos)

  deallocate(array1)
  deallocate(array2)
  deallocate(tulos)

end program day1
