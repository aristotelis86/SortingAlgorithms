! *********************************************************
!                  MODULE: MERGESORT_MOD
!                  ---------------------
! MergeSort algorithm for sorting integer, real and double 
! precision arrays in ascending order. 
! *********************************************************
   
   
   
   module MergeSort_mod

   use Merge_mod
   
   implicit none
   private
   
   public :: MergeSort
   
   interface MergeSort
      procedure :: MergeSort_int, MergeSort_real, MergeSort_dp
   end interface
   
   
   contains
   
   ! ====================================================================
   ! INTEGER
   recursive function MergeSort_int( A, N ) result( T )
      implicit none
      integer, intent(in)                 :: N
      integer, dimension(N), intent(in)   :: A
      integer, dimension(N)               :: T
      
      integer           :: NL,NR
      
      if (N < 2) then
         T = A
         return
      end if
      
      NL = (N+1)/2
      NR = N - NL
      
      T = Merge( MergeSort_int( A(1), NL ), MergeSort_int( A(NL+1), NR ) )
   end function MergeSort_int
   
   ! ====================================================================
   ! REAL
   recursive function MergeSort_real( A, N ) result( T )
      implicit none
      integer, intent(in)                 :: N
      real, dimension(N), intent(in)      :: A
      real, dimension(N)                  :: T
      
      integer           :: NL,NR
      
      if (N < 2) then
         T = A
         return
      end if
      
      NL = (N+1)/2
      NR = N - NL
      
      T = Merge( MergeSort_real( A(1), NL ), MergeSort_real( A(NL+1), NR ) )
   end function MergeSort_real
   
   ! ====================================================================
   ! DOUBLE PRECISION
   recursive function MergeSort_dp( A, N ) result( T )
      implicit none
      integer, intent(in)                 :: N
      double precision, dimension(N), intent(in)      :: A
      double precision, dimension(N)                  :: T
      
      integer           :: NL,NR
      
      if (N < 2) then
         T = A
         return
      end if
      
      NL = (N+1)/2
      NR = N - NL
      
      T = Merge( MergeSort_dp( A(1), NL ), MergeSort_dp( A(NL+1), NR ) )
   end function MergeSort_dp
   
   
   end module
   
   
   