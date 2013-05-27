        interface
          subroutine gtrexc(compq,n,t,ldt,q,ldq,ifst,ilst,info)
            integer :: ldq
            integer :: ldt
            character(len=1) :: compq
            integer :: n
            complex(kind=8) :: t(ldt,*)
            complex(kind=8) :: q(ldq,*)
            integer :: ifst
            integer :: ilst
            integer :: info
          end subroutine gtrexc
        end interface
