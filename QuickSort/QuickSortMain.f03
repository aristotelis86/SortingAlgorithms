! Test QuickSort Module
program quicksort_test
use QuickSort_mod
implicit none
 
integer, parameter      :: k = 8
integer, dimension(k)   :: A_int, A_int_ord
real, dimension(k)      :: A_real, A_real_ord
real(8), dimension(k)   :: A_dp, A_dp_ord
integer                 :: i
 
A_int = [5, 3, 9, 13, 0, 7, 21, 3]
A_int_ord = A_int
call QuickSort(A_int_ord, k)

A_real = [9.81, 6.64, -273.15, 42., 1.013, 0.5, 28.28, 3.142]
A_real_ord = A_real
call QuickSort(A_real_ord, k)

A_dp = [9.81, 6.64, -273.15, 42., 1.013, 0.5, 28.28, 3.142]
A_dp_ord = A_dp
call QuickSort(A_dp_ord, k)

call showMe_int( A_int, A_int_ord, k )
call showMe_real( A_real, A_real_ord, k )
call showMe_dp( A_dp, A_dp_ord, k )

contains

subroutine showMe_int( A, Ao, n )
implicit none
integer, intent(in) :: n
integer, dimension(n), intent(in) :: A, Ao
integer :: i

write(*,*)
write(*,'(A)') '****************************'
write(*,'(A)') '           INTEGER          '
write(*,'(A)') '           -------          '
write(*,'(A,3X,A)') 'Not sorted', 'Sorted'
do i = 1, n
    write(*,'(3X,I4,5X,I4)') A(i), Ao(i)
end do
end subroutine showMe_int

subroutine showMe_real( A, Ao, n )
implicit none
integer, intent(in) :: n
real, dimension(n), intent(in) :: A, Ao
integer :: i

write(*,*)
write(*,'(A)') '****************************'
write(*,'(A)') '            REAL            '
write(*,'(A)') '            ----            '
write(*,'(A,5X,A)') 'Not sorted', 'Sorted'
do i = 1, n
    write(*,'(2X,F8.3,4X,F8.3)') A(i), Ao(i)
end do
end subroutine showMe_real

subroutine showMe_dp( A, Ao, n )
implicit none
integer, intent(in) :: n
real(8), dimension(n), intent(in) :: A, Ao
integer :: i

write(*,*)
write(*,'(A)') '****************************'
write(*,'(A)') '          REAL(8)           '
write(*,'(A)') '          -------           '
write(*,'(A,5X,A)') 'Not sorted', 'Sorted'
do i = 1, n
    write(*,'(2X,F8.3,4X,F8.3)') A(i), Ao(i)
end do
end subroutine showMe_dp


 
end program quicksort_test