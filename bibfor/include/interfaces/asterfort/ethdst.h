        interface
          subroutine ethdst(fami,nno,ndim,nbsig,npg,ipoids,ivf,idfde,&
     &xyz,depl,instan,repere,mater,option,enthth)
            character(*) :: fami
            integer :: nno
            integer :: ndim
            integer :: nbsig
            integer :: npg
            integer :: ipoids
            integer :: ivf
            integer :: idfde
            real(kind=8) :: xyz(*)
            real(kind=8) :: depl(*)
            real(kind=8) :: instan
            real(kind=8) :: repere(7)
            integer :: mater
            character(len=16) :: option
            real(kind=8) :: enthth
          end subroutine ethdst
        end interface
