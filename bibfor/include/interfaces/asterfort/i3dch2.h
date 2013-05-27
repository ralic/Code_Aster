        interface
          subroutine i3dch2(epsi,seuil,maxitr,fk,m,r,s,iret)
            real(kind=8) :: epsi
            real(kind=8) :: seuil
            integer :: maxitr
            real(kind=8) :: fk(4,*)
            real(kind=8) :: m(*)
            real(kind=8) :: r(*)
            real(kind=8) :: s(*)
            integer :: iret
          end subroutine i3dch2
        end interface
