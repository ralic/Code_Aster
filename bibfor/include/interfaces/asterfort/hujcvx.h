        interface
          subroutine hujcvx(mod,nmat,materf,vinf,deps,sigd,sigf,seuil,&
     &iret)
            integer :: nmat
            character(len=8) :: mod
            real(kind=8) :: materf(nmat,2)
            real(kind=8) :: vinf(*)
            real(kind=8) :: deps(6)
            real(kind=8) :: sigd(6)
            real(kind=8) :: sigf(6)
            real(kind=8) :: seuil
            integer :: iret
          end subroutine hujcvx
        end interface
