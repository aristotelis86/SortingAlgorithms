module quicksort_mod
 
implicit none
private

public :: QuickSort

interface QuickSort
    procedure :: QSort_int, QSort_real, QSort_dp
end interface

 
contains

! ==================================================
! INTEGER 
recursive subroutine QSort_int( a, na )
implicit none
 
! DUMMY ARGUMENTS
integer, intent(in) :: na
integer, dimension(na), intent(inout) :: a
 
! LOCAL VARIABLES
integer :: left, right
real :: random
integer :: pivot
integer :: temp
integer :: marker
 
    if (na > 1) then
 
        call random_number(random)
        pivot = a(int(random*real(na-1))+1)   ! random pivor (not best performance, but avoids worst-case)
        left = 0
        right = na + 1
 
        do while (left < right)
            right = right - 1
            do while (a(right) > pivot)
                right = right - 1
            end do
            left = left + 1
            do while (a(left) < pivot)
                left = left + 1
            end do
            if (left < right) then
                temp = a(left)
                a(left) = a(right)
                a(right) = temp
            end if
        end do
 
        if (left == right) then
            marker = left + 1
        else
            marker = left
        end if
 
        call QSort_int(a(:marker-1), marker-1)
        call QSort_int(a(marker:), na-marker+1)
 
    end if
end subroutine QSort_int

! ==================================================
! REAL
recursive subroutine QSort_real( a, na )
implicit none
 
! DUMMY ARGUMENTS
integer, intent(in) :: na
real, dimension(na), intent(inout) :: a
 
! LOCAL VARIABLES
integer :: left, right
real :: random
real :: pivot
real :: temp
integer :: marker
 
    if (na > 1) then
 
        call random_number(random)
        pivot = a(int(random*real(na-1))+1)   ! random pivor (not best performance, but avoids worst-case)
        left = 0
        right = na + 1
 
        do while (left < right)
            right = right - 1
            do while (a(right) > pivot)
                right = right - 1
            end do
            left = left + 1
            do while (a(left) < pivot)
                left = left + 1
            end do
            if (left < right) then
                temp = a(left)
                a(left) = a(right)
                a(right) = temp
            end if
        end do
 
        if (left == right) then
            marker = left + 1
        else
            marker = left
        end if
 
        call QSort_real(a(:marker-1), marker-1)
        call QSort_real(a(marker:), na-marker+1)
 
    end if
end subroutine QSort_real

! ==================================================
! REAL
recursive subroutine QSort_dp( a, na )
implicit none
 
! DUMMY ARGUMENTS
integer, intent(in) :: na
real(8), dimension(na), intent(inout) :: a
 
! LOCAL VARIABLES
integer :: left, right
real :: random
real(8) :: pivot
real(8) :: temp
integer :: marker
 
    if (na > 1) then
 
        call random_number(random)
        pivot = a(int(random*real(na-1))+1)   ! random pivor (not best performance, but avoids worst-case)
        left = 0
        right = na + 1
 
        do while (left < right)
            right = right - 1
            do while (a(right) > pivot)
                right = right - 1
            end do
            left = left + 1
            do while (a(left) < pivot)
                left = left + 1
            end do
            if (left < right) then
                temp = a(left)
                a(left) = a(right)
                a(right) = temp
            end if
        end do
 
        if (left == right) then
            marker = left + 1
        else
            marker = left
        end if
 
        call QSort_dp(a(:marker-1), marker-1)
        call QSort_dp(a(marker:), na-marker+1)
 
    end if
end subroutine QSort_dp
 
end module quicksort_mod