program Build_subroutines
   
    use quicksort_mod
    implicit none
    
    integer                   :: n, m
    integer, dimension(:,:), allocatable   :: BB
    integer                   :: j, k
    character(20)             :: FMT
    
    open(UNIT = 25, FILE = "C:\Users\al064648\Desktop\blah.txt", STATUS='OLD')
    
    read(25,*) n, m
    
    write(FMT, '(A,I1,A)') '(',m,'(I5))'
    
    allocate(BB(n,m))
    
    do j = 1, n
       read(25,trim(FMT)) (BB(j,k), k = 1, m)
    end do
    
    do j = 1, n
       write(*,trim(FMT)) (BB(j,k), k = 1, m)
    end do
    
    
    call QSort_int_aug(BB, n, m, 2, 1 )
    
    write(*,*)
    write(*,*)
    do j = 1, n
       write(*,trim(FMT)) (BB(j,k), k = 1, m)
    end do
    
    
    pause
    end program Build_subroutines
