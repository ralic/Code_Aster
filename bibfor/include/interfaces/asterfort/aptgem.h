        interface
          subroutine aptgem(sdappa,noma,newgeo,defico,ndimg,izone,&
     &typzon,itemax,epsmax,jdecma,nbma)
            character(len=19) :: sdappa
            character(len=8) :: noma
            character(len=19) :: newgeo
            character(len=24) :: defico
            integer :: ndimg
            integer :: izone
            character(len=4) :: typzon
            integer :: itemax
            real(kind=8) :: epsmax
            integer :: jdecma
            integer :: nbma
          end subroutine aptgem
        end interface
