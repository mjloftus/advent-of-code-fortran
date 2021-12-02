module procedures
    implicit none
contains
    integer function increase_count(data, window_size)
        real, dimension(:) :: data
        integer :: l, window_size
        increase_count = 0
        do l = window_size + 1, size(data)
            if (data(l) > data(l - window_size)) then
                increase_count = increase_count + 1
            end if
        end do
        return
    end
end module procedures

program day_01
    use procedures
    implicit none

    real, dimension(:), allocatable :: x
    integer :: n
    open(unit = 1, file = 'input.txt')
    do
        read(1, *, end=10)
        n = n + 1
    end do
    10 rewind(1)
    allocate(x(n))
    read(1, *) x

    write(*, *) "part 1: ", increase_count(x, 1)
    write(*, *) "part 2: ", increase_count(x, 3)
end program
