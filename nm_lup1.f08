program name
    implicit none
    call mainLU()
end program name

subroutine mainLU()
    implicit none

    integer n
    real , allocatable,dimension(:) :: x,y,b
    real, allocatable :: a(:,:),l(:,:),u(:,:),lu(:,:)


    n = 5
    allocate(a(n,n),l(n,n),u(n,n),lu(n,n),x(n),y(n),b(n))

    a(1,1) = 15
    a(1,2) = -1
    a(1,3) = 2
    a(1,4) = -3
    a(1,5) = 4
    b(1) = 8

    a(2,1) = 2
    a(2,2) = 23
    a(2,3) = -1
    a(2,4) = 5
    a(2,5) = -2
    b(2) = 82.4


    a(3,1) = -1
    a(3,2) = 3
    a(3,3) = 92
    a(3,4) = -5
    a(3,5) = 1
    b(3) = -764.9

    a(4,1) = 1
    a(4,2) = 2
    a(4,3) = 1
    a(4,4) = 27
    a(4,5) = 3
    b(4) = -8.9

    a(5,1) = -4
    a(5,2) = -6
    a(5,3) = -2
    a(5,4) = 8
    a(5,5) = 41
    b(5) = -201.9



    call LUdec(a, n, l, u)
    write(*,*)'L:'
    call displayMatrix(l,n,n)
    write(*,*)'U:'
    call displayMatrix(u,n,n)
    call matrixProduct(l,u,n,n,n,n,lu)
    write(*,*)'LU:'
    call displayMatrix(lu,n,n)
    write(*,*)'A:'
    call displayMatrix(a,n,n)

    call forwardSub(l, b, n, y)
    write(*,*) "Intermediate solution x after forward substitution"
    call displayMatrix(y, n, 1)
    ! Step 3: Determine [x] using back-substitution. Equation = [U][x] = [y]
    call backSubstitution(u, y, n, x)
    write(*,*) "Solution x after back substitution"

    call displayMatrix(x, n, 1)



    write(*,*)"Solution"
    call displayMatrix(x,n,1)
end subroutine mainLU

subroutine LUdec(ain,n,l,u)
    implicit none

    integer, intent(in) :: n
    real, dimension(n,n), intent(in) :: ain
    real, dimension(n,n), intent(out) :: l
    real, dimension(n,n), intent(out) :: u
    integer rowCount,columnCount, factorCount
    real factor
    l = 0.0
    u = 0.0



    do rowCount = 1,n

        do columnCount = 1,rowCount
            factor = 0.0
            do factorCount = 1, columnCount-1
                factor = factor + l(rowCount, factorCount)*u(factorCount, columnCount)
            end do
            l(rowCount, columnCount) = ain(rowCount,columnCount) - factor
        end do

        u(rowCount, rowCount) = 1.0

        do columnCount = rowCount + 1,n
            factor = 0.0
            do factorCount = 1, rowCount-1
                factor = factor + l(rowCount, factorCount)*u(factorCount, columnCount)
            end do
            u(rowCount, columnCount) = (ain(rowCount,columnCount) - factor)/ain(rowCount,rowCount)
        end do
    end do 
end subroutine LUdec

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


subroutine matrixProduct(a,b,ra,ca,rb,cb,ab)
    implicit none
    integer, intent(in) :: ra,rb,ca,cb
    real,dimension(ra,cb),intent(out) :: ab
    real,dimension(ra,ca),intent(in) :: a
    real,dimension(rb,cb), intent(in) :: b

    integer rowCount,columnCount,factorCount
    real summation
    if (ca /= rb) then 
        stop 'Error'
    end if 
    do rowCount = 1 , ra 
        do columnCount = 1, cb
            summation = 0.0
            do factorCount = 1,rb
                summation = summation + (a(rowCount,factorCount) * b(factorCount,columnCount))
            end do
            ab(rowCount,columnCount) = summation
        end do 
    end do 
end subroutine matrixProduct

subroutine forwardSub(l,b,n,x)
    implicit none

    integer, intent(in) :: n
    real, dimension(n,n), intent(in) :: l
    real, dimension(n), intent(in) :: b
    ! Variables: Output arguments
    real, dimension(n), intent(out) :: x

    integer rowCount, stepCount
    real factor
    x(1) = b(1) / l(1,1)
    do rowCount = 2, n
        factor = 0.0
        do stepCount = 1, (rowCount - 1)
            factor = factor + (l(rowCount, stepCount) * x(stepCount))
        end do
        x(rowCount) = (b(rowCount) - factor) / l(rowCount, rowCount)
    end do
end subroutine forwardSub

subroutine backSubstitution(u,b,n,x)
    implicit none
    integer, intent(in)::n
    real, dimension(n,n), intent(in) :: u
    real, dimension(n), intent(in) :: b
    real, dimension(n), intent(out) :: x
    real factor
    integer rowCount, stepCount
    x(n) = b(n)/u(n,n)
    do rowCount = n-1,1,-1
        factor = 0.0
        do stepCount = rowCount + 1, n
            factor  = factor + u(rowCount,stepCount)*x(stepCount)
        end do 
        x(rowCount) = (b(rowCount) - factor) / u(rowCount, rowCount)
    end do 

end subroutine backSubstitution

