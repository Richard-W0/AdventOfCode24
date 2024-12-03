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

    temp = trim(temp) // trim(rivi) !string concatenation or something, idk this thing is weird
    print *, temp
    deallocate(rivi)
  end do
  close(10)

  total = 0
  alotus = 1

  


end program day3
