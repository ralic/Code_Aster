        interface
          subroutine aptgnn(sdappa,noma,defico,ndimg,jdecno,nbno,itype&
     &,vector)
            character(len=19) :: sdappa
            character(len=8) :: noma
            character(len=24) :: defico
            integer :: ndimg
            integer :: jdecno
            integer :: nbno
            integer :: itype
            real(kind=8) :: vector(3)
          end subroutine aptgnn
        end interface
