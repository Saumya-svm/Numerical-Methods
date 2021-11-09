!! Efficent way to store data

program name
    implicit none
    call mainLUEfficient()
end program name

subroutine mainLUEfficient
    implicit none
    integer n
    real, dimension(:,:), allocatable :: a,lu
    real, dimension(:), allocatable :: b
    real, dimension(:), allocatable :: x, y

    integer rowCount
    n = 5
    allocate(a(n, n))
    allocate(b(n))
    allocate(x(n))
    allocate(y(n))
    allocate(lu(n,n))
    !! Row 1
    a(1,1) = 15
    a(1,2) = -1
    a(1,3) = 2
    a(1,4) = -3
    a(1,5) = 4
    b(1) = 8
    !! Row 2
    a(2,1) = 2
    a(2,2) = 23
    a(2,3) = -1
    a(2,4) = 5
    a(2,5) = -2
    b(2) = 82.4
    !! Row 3
    a(3,1) = -1
    a(3,2) = 3
    a(3,3) = 92
    a(3,4) = -5
    a(3,5) = 1

    b(3) = -764.9
    !! Row 4
    a(4,1) = 1
    a(4,2) = 2
    a(4,3) = 1
    a(4,4) = 27
    a(4,5) = 3
    b(4) = -8.9
    !! Row 5
    a(5,1) = -4
    a(5,2) = -6
    a(5,3) = -2
    a(5,4) = 8
    a(5,5) = 41
    b(5) = -201.9

    call LUget(a,n,lu)

end subroutine mainLUEfficient

subroutine LUget(a,n,lu)
    implicit none
    integer, intent(in) :: n
    integer , intent(in) :: a
    integer, intent(out) :: lu

    integer rowCount, columnCount, factorCount
    real factor
    lu = 0.0
    

end subroutine LUget