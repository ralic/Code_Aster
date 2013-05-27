        interface
          subroutine mmmjeu(ndim,jeusup,norm,geome,geomm,ddeple,ddeplm&
     &,mprojt,jeu,djeu,djeut,iresog,tau1,tau2,gene11,gene21)
            integer :: ndim
            real(kind=8) :: jeusup
            real(kind=8) :: norm(3)
            real(kind=8) :: geome(3)
            real(kind=8) :: geomm(3)
            real(kind=8) :: ddeple(3)
            real(kind=8) :: ddeplm(3)
            real(kind=8) :: mprojt(3,3)
            real(kind=8) :: jeu
            real(kind=8) :: djeu(3)
            real(kind=8) :: djeut(3)
            integer :: iresog
            real(kind=8) :: tau1(3)
            real(kind=8) :: tau2(3)
            real(kind=8) :: gene11(3,3)
            real(kind=8) :: gene21(3,3)
          end subroutine mmmjeu
        end interface
