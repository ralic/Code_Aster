        interface
          subroutine ftrevc(side,howmny,select,n,t,ldt,vl,ldvl,vr,ldvr&
     &,mm,m,work,info)
            integer :: ldvr
            integer :: ldvl
            integer :: ldt
            character(len=1) :: side
            character(len=1) :: howmny
            logical :: select(*)
            integer :: n
            real(kind=8) :: t(ldt,*)
            real(kind=8) :: vl(ldvl,*)
            real(kind=8) :: vr(ldvr,*)
            integer :: mm
            integer :: m
            real(kind=8) :: work(*)
            integer :: info
          end subroutine ftrevc
        end interface
