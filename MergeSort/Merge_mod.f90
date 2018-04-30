! Module to handle merging of the arrays.
   
   module Merge_mod
   
   implicit none
   private
   
   public   :: Merge
   
   interface Merge
      procedure :: Merge_int, Merge_real, Merge_dp
   end interface
   
   contains
   
   ! ==========================================================
   ! INTEGER
   function Merge_int( A, B ) result( C )
      integer, dimension(:), intent(in)   :: A, B
      integer, dimension(:), allocatable  :: C
      
      integer     :: NA, NB, NC
      integer     :: I, J, K
      logical     :: flagA, flagB
      
      flagA = .false.
      flagB = .false.
      NA = size(A)
      NB = size(B)
      NC = NA + NB
      allocate( C(NC) )
      
      I = 1
      J = 1
      K = 1
      do 
         if (A(I) <= B(J)) then
            C(K) = A(I)
            I = I + 1
            K = K + 1
         else
            C(K) = B(J)
            J = J + 1
            K = K + 1
         end if 
         if (I>NA) then
            flagA = .true.
            exit
         else if (J>NB) then
            flagB = .true.
            exit
         end if
      end do
      
      if (flagA) then
         do 
            C(K) = B(J)
            K = K + 1
            J = J + 1
            if (K>NC .or. J>NB) exit
         end do
      else if (flagB) then
         do 
            C(K) = A(I)
            K = K + 1
            I = I + 1
            if (K>NC .or. I>NA) exit
         end do
      end if
      
   end function Merge_int
   
   ! ==========================================================
   ! REAL
   function Merge_real( A, B ) result( C )
      real, dimension(:), intent(in)   :: A, B
      real, dimension(:), allocatable  :: C
      
      integer     :: NA, NB, NC
      integer     :: I, J, K
      logical     :: flagA, flagB
      
      flagA = .false.
      flagB = .false.
      NA = size(A)
      NB = size(B)
      NC = NA + NB
      allocate( C(NC) )
      
      I = 1
      J = 1
      K = 1
      do 
         if (A(I) <= B(J)) then
            C(K) = A(I)
            I = I + 1
            K = K + 1
         else
            C(K) = B(J)
            J = J + 1
            K = K + 1
         end if 
         if (I>NA) then
            flagA = .true.
            exit
         else if (J>NB) then
            flagB = .true.
            exit
         end if
      end do
      
      if (flagA) then
         do 
            C(K) = B(J)
            K = K + 1
            J = J + 1
            if (K>NC .or. J>NB) exit
         end do
      else if (flagB) then
         do 
            C(K) = A(I)
            K = K + 1
            I = I + 1
            if (K>NC .or. I>NA) exit
         end do
      end if
      
   end function Merge_real
   
   ! ==========================================================
   ! DOUBLE PRECISION
   function Merge_dp( A, B ) result( C )
      double precision, dimension(:), intent(in)   :: A, B
      double precision, dimension(:), allocatable  :: C
      
      integer     :: NA, NB, NC
      integer     :: I, J, K
      logical     :: flagA, flagB
      
      flagA = .false.
      flagB = .false.
      NA = size(A)
      NB = size(B)
      NC = NA + NB
      allocate( C(NC) )
      
      I = 1
      J = 1
      K = 1
      do 
         if (A(I) <= B(J)) then
            C(K) = A(I)
            I = I + 1
            K = K + 1
         else
            C(K) = B(J)
            J = J + 1
            K = K + 1
         end if 
         if (I>NA) then
            flagA = .true.
            exit
         else if (J>NB) then
            flagB = .true.
            exit
         end if
      end do
      
      if (flagA) then
         do 
            C(K) = B(J)
            K = K + 1
            J = J + 1
            if (K>NC .or. J>NB) exit
         end do
      else if (flagB) then
         do 
            C(K) = A(I)
            K = K + 1
            I = I + 1
            if (K>NC .or. I>NA) exit
         end do
      end if
      
   end function Merge_dp
   
   end module
   
   