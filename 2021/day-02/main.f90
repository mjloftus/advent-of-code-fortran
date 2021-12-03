program day_02
    implicit none

    integer :: n, v, i, x, y1, y2, a
    character(len=10) :: c
    n = 0
    open(unit = 11, file = 'input.txt')
    do
        read(11, *, end=1)
        n = n + 1
    end do
    1 rewind(11)
    x = 0
    y1 = 0
    y2 = 0
    a = 0
    do i = 1, n
        read(11, *) c, v
        select case(c)
            case ("forward")
                x = x + v
                y2 = y2 + (v * a)
            case ("down")
                y1 = y1 + v
                a = a + v
            case ("up")
                y1 = y1 - v
                a = a - v
        end select
    end do
    write(*, *) "part 1: ", x * y1
    write(*, *) "part 2: ", x * y2
end program
