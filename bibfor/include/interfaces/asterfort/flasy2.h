        interface
          subroutine flasy2(ltranl,ltranr,isgn,n1,n2,tl,ldtl,tr,ldtr,b&
     &,ldb,scale,x,ldx,xnorm,info)
            integer :: ldx
            integer :: ldb
            integer :: ldtr
            integer :: ldtl
            logical :: ltranl
            logical :: ltranr
            integer :: isgn
            integer :: n1
            integer :: n2
            real(kind=8) :: tl(ldtl,*)
            real(kind=8) :: tr(ldtr,*)
            real(kind=8) :: b(ldb,*)
            real(kind=8) :: scale
            real(kind=8) :: x(ldx,*)
            real(kind=8) :: xnorm
            integer :: info
          end subroutine flasy2
        end interface
