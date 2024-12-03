program day2
  implicit none
  integer, allocatable ::rivi(:), tempLista(:)
  integer :: ios, n, i, hyvatRivit
  character (len = 10) :: tiedosto
  character(len=100) :: buffer
  logical :: tulos

  !something to hopefully get the function calls to work
  interface
    logical function analysoi(level, n)
      integer, intent(in) :: level(:), n
    end function analysoi
  end interface

  tiedosto = "paiva2.txt"
  hyvatRivit = 0

  OPEN(UNIT=10, FILE=tiedosto, STATUS="OLD", ACTION="READ")

  do
    read(10, '(A)', iostat = ios) buffer
    if (ios < 0) exit
    if (ios > 0) then
      print *, "Error"
      exit
    end if

    buffer = trim(adjustl(buffer))
    allocate(tempLista(1000))
    tempLista = 0

    read(buffer, *, iostat = ios) (tempLista(i), i = 1, 1000)
    n = count(tempLista /= 0)

    if (n == 0) cycle

    allocate(rivi(n))

    read(buffer, *,  iostat = ios) (rivi(i), i = 1, n)
    if (ios /= 0) then
      print *, "Error rivin luvussa"
      deallocate(rivi)
      exit
    end if
    
    print *, "Rivi=", (rivi)

    tulos = analysoi(rivi, n)

    if (tulos) then
      hyvatRivit = hyvatRivit + 1 
    end if

    deallocate(rivi)
    deallocate(tempLista)

    print *, "Onko turvarivi? ", tulos

  end do

  print *, "Hyvät rivit =", hyvatRivit
  
  close(unit=10)

end program day2

logical function analysoi(level, n)
  implicit none
  integer, intent(in) :: level(:), n
  integer :: i
  logical :: laskeva, nouseva

  laskeva = .true.
  nouseva = .true.
  if (n < 2) then
    analysoi = .false.
    return
  end if

  do i = 1, n - 1
    if (i + 1 > n) then
      print *, "Error: Accessing out of bounds."
      analysoi = .false.
      return
    end if

    if (level(i) == 0) cycle 
    if(level(i+1) - level(i) < 1 .or. level(i+1) - level(i) >3) then
      analysoi = .false.
      return
    end if

    if (level(i+1) - level(i) <= 0) nouseva = .false.
    if (level(i+1) - level(i) >= 0) laskeva = .false.
  end do

  analysoi = laskeva .or. nouseva
end function analysoi
