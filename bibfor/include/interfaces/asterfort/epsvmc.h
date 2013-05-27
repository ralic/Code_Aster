        interface
          subroutine epsvmc(fami,nno,ndim,nbsig,npg,ipoids,ivf,idfde,&
     &xyz,depl,instan,mater,repere,nharm,option,epsm)
            character(len=4) :: fami
            integer :: nno
            integer :: ndim
            integer :: nbsig
            integer :: npg
            integer :: ipoids
            integer :: ivf
            integer :: idfde
            real(kind=8) :: xyz(1)
            real(kind=8) :: depl(1)
            real(kind=8) :: instan
            integer :: mater
            real(kind=8) :: repere(7)
            real(kind=8) :: nharm
            character(len=16) :: option
            real(kind=8) :: epsm(1)
          end subroutine epsvmc
        end interface
