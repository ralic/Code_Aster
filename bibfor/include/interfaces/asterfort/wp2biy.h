        interface
          subroutine wp2biy(lm,lc,lk,s2,dsr,isi,yh,yb,zh,zb,lbloq,u1,&
     &u2,u3,u4,n)
            integer :: lm
            integer :: lc
            integer :: lk
            real(kind=8) :: s2
            real(kind=8) :: dsr
            real(kind=8) :: isi
            real(kind=8) :: yh(*)
            real(kind=8) :: yb(*)
            real(kind=8) :: zh(*)
            real(kind=8) :: zb(*)
            integer :: lbloq(*)
            real(kind=8) :: u1(*)
            real(kind=8) :: u2(*)
            real(kind=8) :: u3(*)
            real(kind=8) :: u4(*)
            integer :: n
          end subroutine wp2biy
        end interface
