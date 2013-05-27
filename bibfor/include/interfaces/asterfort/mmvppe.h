        interface
          subroutine mmvppe(typmae,typmam,iresog,ndim,nne,nnm,nnl,nbdm&
     &,laxis,ldyna,lfovit,jeusup,ffe,ffm,ffl,norm,tau1,tau2,mprojt,&
     &jacobi,wpg,dlagrc,dlagrf,jeu,djeu,djeut)
            character(len=8) :: typmae
            character(len=8) :: typmam
            integer :: iresog
            integer :: ndim
            integer :: nne
            integer :: nnm
            integer :: nnl
            integer :: nbdm
            logical :: laxis
            logical :: ldyna
            logical :: lfovit
            real(kind=8) :: jeusup
            real(kind=8) :: ffe(9)
            real(kind=8) :: ffm(9)
            real(kind=8) :: ffl(9)
            real(kind=8) :: norm(3)
            real(kind=8) :: tau1(3)
            real(kind=8) :: tau2(3)
            real(kind=8) :: mprojt(3,3)
            real(kind=8) :: jacobi
            real(kind=8) :: wpg
            real(kind=8) :: dlagrc
            real(kind=8) :: dlagrf(2)
            real(kind=8) :: jeu
            real(kind=8) :: djeu(3)
            real(kind=8) :: djeut(3)
          end subroutine mmvppe
        end interface
