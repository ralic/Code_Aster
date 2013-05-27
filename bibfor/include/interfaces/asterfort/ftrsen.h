        interface
          subroutine ftrsen(job,compq,select,n,t,ldt,q,ldq,wr,wi,m,s,&
     &sep,work,lwork,iwork,liwork,info)
            integer :: ldq
            integer :: ldt
            character(len=1) :: job
            character(len=1) :: compq
            logical :: select(*)
            integer :: n
            real(kind=8) :: t(ldt,*)
            real(kind=8) :: q(ldq,*)
            real(kind=8) :: wr(*)
            real(kind=8) :: wi(*)
            integer :: m
            real(kind=8) :: s
            real(kind=8) :: sep
            real(kind=8) :: work(*)
            integer :: lwork
            integer :: iwork(*)
            integer :: liwork
            integer :: info
          end subroutine ftrsen
        end interface
