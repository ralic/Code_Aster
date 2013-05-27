        interface
          subroutine gtrevc(side,howmny,select,n,t,ldt,vl,ldvl,vr,ldvr&
     &,mm,m,work,rwork,info)
            integer :: ldvr
            integer :: ldvl
            integer :: ldt
            character(len=1) :: side
            character(len=1) :: howmny
            logical :: select(*)
            integer :: n
            complex(kind=8) :: t(ldt,*)
            complex(kind=8) :: vl(ldvl,*)
            complex(kind=8) :: vr(ldvr,*)
            integer :: mm
            integer :: m
            complex(kind=8) :: work(*)
            real(kind=8) :: rwork(*)
            integer :: info
          end subroutine gtrevc
        end interface
