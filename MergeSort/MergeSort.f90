
!****************************************************************************
!
!  PROGRAM: MergeSort
!
!  PURPOSE:  Entry point for the console application to test MergeSort modules
!
!****************************************************************************

    program MergeSortMain

    use MergeSort_mod

    implicit none

    ! Variables
    integer, parameter    :: N = 11
    integer, dimension(N) :: A_int
    real, dimension(N)    :: A_real
    double precision, dimension(N) :: A_dp

    integer, dimension(N) :: A_int_ord
    real, dimension(N) :: A_real_ord
    double precision, dimension(N) :: A_dp_ord
    
    ! Body of MergeSortMain
    A_int = (/3, 5, 8, 7, 11, 1, 4, 2, 6, 10, 9/)
    A_real = (/3.2, 4.6, 3.5, 4.77, 6.0, 9.12, 3.51, 1.11, 0.03, 9.99, 12.76/)
    A_dp = (/2.92, 6.4, 5.1, 7.44, 11.0, 2.92, 1.53, 1.11, 0.03, 9.99, 2.376/)
    
    A_int_ord = MergeSort( A_int, N )
    A_real_ord = MergeSort( A_real, N )
    A_dp_ord = MergeSort( A_dp, N )
    
    
    call showMe_int(A_int, A_int_ord, N)
    call showMe_real(A_real, A_real_ord, N)
    call showMe_dp(A_dp, A_dp_ord, N)

    pause 
    contains

    subroutine showMe_int( K, K_ord, n )
      implicit none
      integer, intent(in) :: n
      integer, dimension(n), intent(in) :: K, K_ord
      integer :: i
      
      write(*,*)
      write(*,'(A19)') '***** INTEGER *****'
      write(*,'(A21)') 'Not Sorted -  Sorted'
      do i = 1, n
         write(*,'(5X,I3,3X,A,2X,I3)') K(i), ' -', K_ord(i)
      end do
    end subroutine

    subroutine showMe_real( K, K_ord, n )
      implicit none
      integer, intent(in) :: n
      real, dimension(n), intent(in) :: K, K_ord
      integer :: i
      
      write(*,*)
      write(*,'(A19)') '*****   REAL *****'
      write(*,'(A21)') 'Not Sorted -  Sorted'
      do i = 1, n
         write(*,'(2X,F6.3,3X,A,1X,F6.3)') K(i), ' -', K_ord(i)
      end do
    end subroutine

    subroutine showMe_dp( K, K_ord, n )
      implicit none
      integer, intent(in) :: n
      double precision, dimension(n), intent(in) :: K, K_ord
      integer :: i
      
      write(*,*)
      write(*,'(A28)') '***** DOUBLE PRECISION *****'
      write(*,'(A21)') 'Not Sorted -  Sorted'
      do i = 1, n
         write(*,'(2X,F6.3,3X,A,1X,F6.3)') K(i), ' -', K_ord(i)
      end do
    end subroutine

    end program MergeSortMain
    
    
   
      
      
   
   
