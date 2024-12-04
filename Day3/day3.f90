program day3
  implicit none
  integer :: ios, total, x, y, alotus, lopetus, tiedostoPituus, i
  character(len = 10) :: tiedosto
  character(len = :), allocatable :: temp
  character(len=1), allocatable :: buffer(:)

  total = 0
  alotus = 1

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

 do
    print *, "loopin alussa alotus on", alotus
    alotus = index(temp(alotus:), "mul(")
    
    print *, "indexoinnin jälkeen alotus on", alotus
    print *, temp(alotus:alotus+5)
    if (alotus == 0) exit 
    alotus = alotus + 4 !skip the "mul("

    read(temp(alotus:), '(I3)', iostat=ios) x
    if (ios /= 0) then
      print *, "Error read 1", alotus, temp(alotus:alotus)
      print *, temp(1:alotus)
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
    temp = temp(alotus:)
    print *, "alotus + lopetus =", alotus
  end do

  print *, total
end program day3
