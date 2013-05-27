        interface
          subroutine rkcah1(comp,y,pas,nvi,w,wk,h,eps,iret)
            character(len=16) :: comp(*)
            real(kind=8) :: y(*)
            real(kind=8) :: pas
            integer :: nvi
            real(kind=8) :: w
            real(kind=8) :: wk(*)
            real(kind=8) :: h
            real(kind=8) :: eps
            integer :: iret
          end subroutine rkcah1
        end interface
