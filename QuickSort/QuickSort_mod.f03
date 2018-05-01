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

! ==================================================
! AUGMENTED INTEGER
recursive subroutine QSort_int_aug( A, n, m, dim, id )
implicit none

! DUMMIES
integer, intent(in)              :: n, m, dim, id
integer, dimension(n,m), intent(inout) :: A

! LOCALS
integer :: left, right
real :: random
integer :: pivot
integer, allocatable, dimension(:,:) :: temp
integer :: marker

if (dim /= 1 .and. dim /= 2) then
   write(*,*)
   write(*,*) 'Error: Dimension given does not exist for this matrix'
   return
end if

if (dim == 1 .and. (id > n .or. id < 1)) then
   write(*,*)
   write(*,*) 'Error: Row ID is out of bounds.'
   return
end if

if (dim == 2 .and. (id > m .or. id < 1)) then
   write(*,*)
   write(*,*) 'Error: Column ID is out of bounds.'
   return
end if 

if (dim == 1 .and. m > 1) then
   call random_number(random)
   pivot = A(id, int(random*real(m-1))+1)   ! random pivor (not best performance, but avoids worst-case)
   left = 0
   right = m + 1
   
   if (allocated(temp)) deallocate(temp)
   allocate(temp(n,1))
   do while (left < right)
      right = right - 1
      do while (A(id,right) > pivot)
            right = right - 1
      end do
      left = left + 1
      do while (A(id,left) < pivot)
            left = left + 1
      end do
      if (left < right) then
            temp(:, 1) = A(:, left)
            A(:, left) = A(:, right)
            A(:, right) = temp(:,1)
      end if
   end do
 
   if (left == right) then
      marker = left + 1
   else
      marker = left
   end if
   
   call QSort_int_aug(A(:,:marker-1), n, marker-1, 1, id)
   call QSort_int_aug(A(:,marker:), n, m-marker+1, 1, id)
   
else if (dim == 2 .and. n > 1) then
   call random_number(random)
   pivot = A(int(random*real(n-1))+1, id)   ! random pivor (not best performance, but avoids worst-case)
   left = 0
   right = n + 1
   
   if (allocated(temp)) deallocate(temp)
   allocate(temp(1,m))
   do while (left < right)
      right = right - 1
      do while (A(right,id) > pivot)
            right = right - 1
      end do
      left = left + 1
      do while (A(left,id) < pivot)
            left = left + 1
      end do
      if (left < right) then
            temp(1, :) = A(left, :)
            A(left, :) = A(right, :)
            A(right, :) = temp(1, :)
      end if
   end do
 
   if (left == right) then
      marker = left + 1
   else
      marker = left
   end if
   
   call QSort_int_aug(A(:marker-1,:), marker-1, m, 2, id)
   call QSort_int_aug(A(marker:,:), n-marker+1, m, 2, id)
   
end if

end subroutine QSort_int_aug
 
end module quicksort_mod
