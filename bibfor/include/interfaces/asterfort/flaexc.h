        interface
          subroutine flaexc(wantq,n,t,ldt,q,ldq,j1,n1,n2,work,info)
            integer :: ldq
            integer :: ldt
            logical :: wantq
            integer :: n
            real(kind=8) :: t(ldt,*)
            real(kind=8) :: q(ldq,*)
            integer :: j1
            integer :: n1
            integer :: n2
            real(kind=8) :: work(*)
            integer :: info
          end subroutine flaexc
        end interface
