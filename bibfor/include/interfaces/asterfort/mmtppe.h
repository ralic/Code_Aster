        interface
          subroutine mmtppe(typmae,typmam,ndim,nne,nnm,nnl,nbdm,iresog&
     &,laxis,ldyna,jeusup,ffe,ffm,dffm,ffl,jacobi,wpg,jeu,djeut,dlagrc,&
     &dlagrf,norm,tau1,tau2,mprojn,mprojt,mprt1n,mprt2n,gene11,gene21)
            character(len=8) :: typmae
            character(len=8) :: typmam
            integer :: ndim
            integer :: nne
            integer :: nnm
            integer :: nnl
            integer :: nbdm
            integer :: iresog
            logical :: laxis
            logical :: ldyna
            real(kind=8) :: jeusup
            real(kind=8) :: ffe(9)
            real(kind=8) :: ffm(9)
            real(kind=8) :: dffm(2,9)
            real(kind=8) :: ffl(9)
            real(kind=8) :: jacobi
            real(kind=8) :: wpg
            real(kind=8) :: jeu
            real(kind=8) :: djeut(3)
            real(kind=8) :: dlagrc
            real(kind=8) :: dlagrf(2)
            real(kind=8) :: norm(3)
            real(kind=8) :: tau1(3)
            real(kind=8) :: tau2(3)
            real(kind=8) :: mprojn(3,3)
            real(kind=8) :: mprojt(3,3)
            real(kind=8) :: mprt1n(3,3)
            real(kind=8) :: mprt2n(3,3)
            real(kind=8) :: gene11(3,3)
            real(kind=8) :: gene21(3,3)
          end subroutine mmtppe
        end interface
