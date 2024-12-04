program day3
  implicit none
  integer :: ios, total, x, y, alotus, lopetus, tiedostoPituus, i, kerto, pilkku
  logical :: enabled
  character(len = 10) :: tiedosto
  character(len = :), allocatable :: temp, ehdokas
  character(len=1), allocatable :: buffer(:)

  total = 0
  alotus = 1
  enabled = .true.

  tiedosto = "paiva3.txt"

  open(unit=10, file=tiedosto, status="old", access="stream", action="read", iostat=ios)
  if (ios /= 0) stop

  inquire(unit=10, size=tiedostoPituus) !not exactly sure how this works, but it does
  allocate(buffer(tiedostoPituus))

  read(10) buffer
  close(10)

  temp = ""
  do i = 1, size(buffer)
    temp = trim(temp) // buffer(i)
  end do

  do
    kerto = index(temp(alotus:), "mul(")
    if (kerto == 0) exit
    kerto = alotus + kerto - 1

    if (index(temp(alotus:), "do()") > 0 .and. index(temp(alotus:), "do()") < kerto) enabled = .true.
    if (index(temp(alotus:), "don't()") > 0 .and. index(temp(alotus:), "don't()") < kerto) enabled = .false.

    lopetus = index(temp(kerto:), ")")
    if (lopetus > 12) then
      alotus = kerto + 4
      cycle
    end if
    lopetus = kerto + lopetus - 1

    ehdokas = temp(kerto:lopetus)
    if (ehdokas(1:4) /= "mul(") then
      alotus = kerto + 4
      cycle
    end if

    pilkku = index(ehdokas, ",")
    if (pilkku == 0) then
      alotus = kerto + 4
      cycle
    end if

    read(ehdokas(5:pilkku-1), *, iostat=ios) x
    if (ios /= 0) then
      alotus = lopetus + 1
      cycle
    end if
      
    read(ehdokas(pilkku+1:len(ehdokas)-1), *, iostat=ios) y
    if (ios /= 0) then
      alotus = lopetus + 1
      cycle
    end if

    if (enabled) total = total + (x * y)
    alotus = lopetus + 1
  end do

  print *, total
end program day3


