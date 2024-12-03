program day2
  implicit none
  integer, allocatable ::rivi(:), tempLista(:)
  integer :: ios, n, i
  character (len = 10) :: tiedosto
  character(len=100) :: buffer

  tiedosto = "paiva2.txt"

  OPEN(UNIT=10, FILE=tiedosto, STATUS="OLD", ACTION="READ")

  do
    read(10, '(A)', iostat = ios) buffer
    if (ios < 0) exit
    if (ios > 0) then
      print *, "Error"
      exit
    end if

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
    deallocate(rivi)
    deallocate(tempLista)



  end do





end program day2
