program name
    implicit none
    call mainGaussJordan()
end program name

subroutine mainGaussJordan()
    implicit none

    integer n
    real, dimension(:,:), allocatable :: a
    real, dimension(:), allocatable :: x

    n = 5
    allocate(a(n,n),x(n))

    a(1,1) = 0
    a(1,2) = -1
    a(1,3) = 2
    a(1,4) = -3
    a(1,5) = 4
    a(1,6) = -38.5
    a(2,1) = 2
    a(2,2) = 3
    a(2,3) = -1
    a(2,4) = 5
    a(2,5) = -2
    a(2,6) = 32.4
    a(3,1) = -1
    a(3,2) = 3
    a(3,3) = 2
    a(3,4) = -5
    a(3,5) = 1
    a(3,6) = -17.9
    a(4,1) = 1
    a(4,2) = 2
    a(4,3) = 1
    a(4,4) = 2
    a(4,5) = 3
    a(4,6) = -13.9
    a(5,1) = -4
    a(5,2) = -6
    a(5,3) = -2
    a(5,4) = 8
    a(5,5) = -3
    a(5,6) = 4.9

    call GaussJordanPivoting(a,x,n)
    write(*,*)"Solution"
    call displayMatrix(x,n,1)
end subroutine mainGaussJordan

subroutine GaussJordanPivoting(a,x,n)
    implicit none
    integer, intent(in) :: n
    real, dimension(n) :: x
    real, dimension(n,n+1) :: a
    integer, external :: rowWithMaxValueInColumn

    integer rowCount, columnCount, factorCount
    real factor
    do rowCount = 1, n
        


end subroutine GaussJordanPivoting

subroutine displayMatrix(matrix, rows, columns)
    implicit none

    integer, intent(in)::rows, columns
    real, dimension(rows,columns) , intent(in):: matrix
    integer i,j
    do i = 1, rows
        do j = 1,columns
            write(*,10, advance = 'no')matrix(i,j)
        end do
        write(*,*)
    end do
    write(*,*)

    10 format(f7.2)
end subroutine displayMatrix
