program day4
    implicit none
    integer, parameter :: maxRows = 10000, maxCols = 10000
    character(len=1), dimension(maxRows, maxCols) :: grid
    integer :: numRows, numCols, totalCount
    character(len=4), parameter :: target = "XMAS"
    character(len=100) :: fileName
    integer :: i, j, k, dx, dy, newRow, newCol, ios
    character(len=1024) :: line
    logical :: match

    fileName = "paiva4.txt"

    !read the grid from the file
    numRows = 0
    numCols = 0
    open(unit=10, file=fileName, status="old", action="read", iostat=ios)
    if (ios /= 0) then
        print *, "Error opening file."
        stop
    end if

    i = 1
    do
        read(10, '(A)', iostat=ios) line
        if (ios /= 0) exit
        numCols = len_trim(line)
        do j = 1, numCols
            grid(i, j) = line(j:j)
        end do
        i = i + 1
    end do
    numRows = i - 1
    close(10)

    totalCount = 0
    do i = 1, numRows !iterate over the rows
        do j = 1, numCols !iterate columns
            do dx = -1, 1 !loop over possible x-directions (-1 for left, 0 for no movement, 1 for right)
                do dy = -1, 1 !same for y-directions -1 for down 1 for up
                    if (dx == 0 .and. dy == 0) cycle !skip if both are 0
                    match = .true. !assume match unless stated otherwise
                    do k = 0, len_trim(target) - 1 !loop through each character in "XMAS"
                        newRow = i + k * dx !calculate new row positions for x and y
                        newCol = j + k * dy
                        if (newRow < 1 .or. newRow > numRows .or. & !check if the new position is out of bounds or doesn't match the target character
                            newCol < 1 .or. newCol > numCols .or. &
                            grid(newRow, newCol) /= target(k + 1:k + 1)) then
                            match = .false. !if out of bounds or mismatch, this direction doesn't match "XMAS"
                            exit
                        end if
                    end do
                    if (match) totalCount = totalCount + 1
                end do
            end do
        end do
    end do

    print *, "Total occurrences of XMAS:", totalCount
end program day4
