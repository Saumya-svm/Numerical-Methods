program name
    implicit none
    call mainThomas()
end program name

subroutine mainThomas
    implicit none
    integer n 
    real, dimension(:), allocatable :: l,d,u,b,x

    n = 5
    allocate(l(n),d(n),u(n),b(n),x(n))
    l = -1.0
    d = 3.0
    u = -1.0

    b(1) = 2.0
    b(2) = 1.0
    b(3) = 1.0
    b(4) = 1.0
    b(5) = 2.0
    call thomasDiagonal(l,d,u,b,x,n)
    call displayMatrix(x,n,1)

end subroutine mainThomas

subroutine thomasDiagonal(l,d,u,b,x,n)
    implicit none
    integer, intent(in) :: n
    real, dimension(n),intent(out) :: x
    real, dimension(n) :: l,d,u,b

    integer stepCount
    real factor
    do stepCount = 1,n-1
        factor = l(stepCount + 1)/d(stepCount)
        d(stepCount+1) = d(stepCount+1) - factor*u(stepCOunt)
        b(stepCount+1) = b(stepCount + 1)- factor*b(stepCount)
    end do
    x(n) = b(n) / d(n)
    do stepCount = (n - 1), 1, -1
    x(stepCount) = (b(stepCount) - (u(stepCount) * x(stepCount + 1))) /d(stepCount)
    end do
end subroutine thomasDiagonal


subroutine displayMatrix(matrix, rows,columns)
    implicit none

    integer , intent(in) :: rows,columns
    integer rowCount, columnCount
    real, dimension(rows,columns) , intent(in):: matrix

    do rowCount = 1, rows
        do columnCount= 1, columns
            write(*,10,advance = 'no')matrix(rowCount, columnCount)
        end do
        write(*,*)
    end do

    10 format(f7.2)
end subroutine displayMatrix
