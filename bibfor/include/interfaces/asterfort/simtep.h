        interface
          subroutine simtep(fami,nno,ndim,nbsig,npg,ipoids,ivf,idfde,&
     &xyz,depl,instan,repere,mater,nharm,sigma)
            character(*) :: fami
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
            real(kind=8) :: repere(7)
            integer :: mater
            real(kind=8) :: nharm
            real(kind=8) :: sigma(1)
          end subroutine simtep
        end interface
