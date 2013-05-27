        interface
          subroutine gtrsen(job,compq,select,n,t,ldt,q,ldq,w,m,s,sep,&
     &work,lwork,info)
            integer :: ldq
            integer :: ldt
            character(len=1) :: job
            character(len=1) :: compq
            logical :: select(*)
            integer :: n
            complex(kind=8) :: t(ldt,*)
            complex(kind=8) :: q(ldq,*)
            complex(kind=8) :: w(*)
            integer :: m
            real(kind=8) :: s
            real(kind=8) :: sep
            complex(kind=8) :: work(*)
            integer :: lwork
            integer :: info
          end subroutine gtrsen
        end interface
