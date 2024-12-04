program day3
  implicit none
  integer :: ios, total, x, y, start, endPos, fileLength, i, mulIndex, commaPos
  logical :: enabled
  character(len = 10) :: fileName
  character(len = :), allocatable :: temp, candidate
  character(len=1), allocatable :: buffer(:)

  total = 0
  start = 1
  enabled = .true.

  fileName = "paiva3.txt"

  open(unit=10, file=fileName, status="old", access="stream", action="read", iostat=ios)
  if (ios /= 0) stop

  inquire(unit=10, size=fileLength) !not exactly sure how this works, but it does
  allocate(buffer(fileLength))

  read(10) buffer
  close(10)

  temp = ""
  do i = 1, size(buffer)
    temp = trim(temp) // buffer(i)
  end do

  do
    mulIndex = index(temp(start:), "mul(")
    if (mulIndex == 0) exit
    mulIndex = start + mulIndex - 1

    if (index(temp(start:mulIndex-1), "do()") > 0) then
        enabled = .true.
    end if
    if (index(temp(start:mulIndex-1), "don't()") > 0) then
        enabled = .false.
    end if

    endPos = index(temp(mulIndex:), ")")
    if (endPos > 12) then
      start = mulIndex + 4
      cycle
    end if
    endPos = mulIndex + endPos - 1

    candidate = temp(mulIndex:endPos)
    if (candidate(1:4) /= "mul(") then
      start = mulIndex + 4
      cycle
    end if

    commaPos = index(candidate, ",")
    if (commaPos == 0) then
      start = mulIndex + 4
      cycle
    end if

    read(candidate(5:commaPos-1), *, iostat=ios) x
    if (ios /= 0) then
      start = endPos + 1
      cycle
    end if
      
    read(candidate(commaPos+1:len(candidate)-1), *, iostat=ios) y
    if (ios /= 0) then
      start = endPos + 1
      cycle
    end if

    print *, enabled

    if (enabled) total = total + (x * y)
    start = endPos + 1
  end do

  print *, total
end program day3

