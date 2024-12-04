program day4
  implicit none
  integer :: ios, i, fileLen
  character(len=10) :: tiedosto
  character(len = :), allocatable :: temp
  character(len=1), allocatable :: buffer(:)

  file = "paiva4.txt"

  open(unit=10, file=file, status="old", access="stream", action="read", iostat=ios)
  if (ios /= 0) stop

  inquire(unit=10, size=fileLen) !not exactly sure how this works, but it does
  allocate(buffer(fileLen))

  read(10) buffer
  close(10)

  temp = ""
  do i = 1, size(buffer)
    temp = trim(temp) // buffer(i)
  end do
  print *, temp
  

end program day4
