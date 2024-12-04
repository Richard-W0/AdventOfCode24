program day3
  implicit none
  integer :: ios, total, x, y, alotus, lopetus, tiedostoPituus, i
  character(len = 10) :: tiedosto
  character(len = :), allocatable :: temp
  character(len=1), allocatable :: buffer(:)


  tiedosto = "paiva3.txt"

  open(unit=10, file=tiedosto, status="old", access="stream", action="read", iostat=ios)
  if (ios /= 0) then
    print *, "Error reading file"
    stop  
  end if

  inquire(unit=10, size=tiedostoPituus) !not entirely sure how this works but it works

  allocate(buffer(tiedostoPituus))

  read(10) buffer
  close(10)

  temp=""

  do i = 1, size(buffer)
    temp = trim(adjustl(temp)) // buffer(i)
  end do

  total = 0
  alotus = 1

 do
    alotus = index(temp(alotus:), "mul(")
    if (alotus == 0) exit 
    alotus = alotus + 4 !skip the "mul("

    read(temp(alotus:), '(I3)', iostat=ios) x
    if (ios /= 0) then
      print *, "Error read 1", alotus
      exit
    end if
    print *, "x=",x, "alotus =", alotus

    lopetus = index(temp(alotus:), ",")
    if (lopetus == 0) exit

    alotus = alotus + lopetus

    read(temp(alotus:), '(I3)', iostat=ios) y
    if (ios /= 0) then
      print *, "Error read 2"
      exit
    end if
    print *,"y=", y, "lopetus =", lopetus

    lopetus = index(temp(alotus:), ")")
    if (lopetus == 0) exit

    total = total + (x * y)
    print *, total

    alotus = alotus + lopetus
    print *, "alotus + lopetus =", alotus
  end do

  print *, total
end program day3
