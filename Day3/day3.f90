program day3
  implicit none
  integer :: ios, total, x, y, alotus, lopetus, riviPituus
  character(len = 10) :: tiedosto
  character(len = :), allocatable :: temp, rivi


  tiedosto = "paiva3.txt"

  open(unit=10, file=tiedosto, status="old", action="read", iostat=ios)
  if (ios /= 0) then
    print *, "Error reading file"
    stop  
  end if

  temp = ""

  do
    riviPituus = 0
    read(10, '(A)', iostat=ios) riviPituus

    if (ios /= 0) exit
    allocate(character(len=riviPituus) :: rivi)

    read(10, '(A)', iostat=ios) rivi
    if (ios /= 0) exit 
    print *, trim(rivi)

    temp = trim(temp) // trim(rivi) !string concatenation or something, idk this thing is weird
    print *, temp
    deallocate(rivi)
  end do
  close(10)

  total = 0
  alotus = 1

 do
    alotus = index(temp(alotus:), "mul(")
    if (alotus == 0) exit 
    alotus = alotus + 4 !skip the "mul("

    read(temp(alotus:), '(I3)', iostat=ios) x
    if (ios /= 0) then
      alotus = alotus + 1
      cycle
    end if
    print *, x

    lopetus = index(temp(alotus:), ",")
    if (lopetus == 0) then
      alotus = alotus + 1
      cycle
    end if

    alotus = alotus + lopetus

    read(temp(alotus:), '(I3)', iostat=ios) y
    if (ios /= 0) then
      alotus = alotus + 1
      cycle
    end if
    print *, y

    lopetus = index(temp(alotus:), ")")
    if (lopetus == 0) then
      alotus = alotus + 1
      cycle
    end if

    total = total + (x * y)
    print *, total

    alotus = alotus + lopetus
  end do

  print *, total
end program day3
