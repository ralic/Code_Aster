        interface
          subroutine ftrexc(compq,n,t,ldt,q,ldq,ifst,ilst,work,info)
            integer :: ldq
            integer :: ldt
            character(len=1) :: compq
            integer :: n
            real(kind=8) :: t(ldt,*)
            real(kind=8) :: q(ldq,*)
            integer :: ifst
            integer :: ilst
            real(kind=8) :: work(*)
            integer :: info
          end subroutine ftrexc
        end interface
