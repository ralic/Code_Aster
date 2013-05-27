        interface
          subroutine wp2bry(ldrf,lmasse,lamor,lraide,sr,si2,yh,yb,zh,&
     &zb,u1,u2,u3,u4,n,solveu)
            integer :: ldrf
            integer :: lmasse
            integer :: lamor
            integer :: lraide
            real(kind=8) :: sr
            real(kind=8) :: si2
            real(kind=8) :: yh(*)
            real(kind=8) :: yb(*)
            real(kind=8) :: zh(*)
            real(kind=8) :: zb(*)
            real(kind=8) :: u1(*)
            real(kind=8) :: u2(*)
            real(kind=8) :: u3(*)
            real(kind=8) :: u4(*)
            integer :: n
            character(len=19) :: solveu
          end subroutine wp2bry
        end interface
