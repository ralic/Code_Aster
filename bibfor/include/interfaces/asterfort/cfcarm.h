        interface
          subroutine cfcarm(noma,defico,newgeo,posmai,typmai,nummai,&
     &alias,nommai,ndim,nnomam,coorma)
            character(len=8) :: noma
            character(len=24) :: defico
            character(len=19) :: newgeo
            integer :: posmai
            character(len=4) :: typmai
            integer :: nummai
            character(len=8) :: alias
            character(len=8) :: nommai
            integer :: ndim
            integer :: nnomam
            real(kind=8) :: coorma(27)
          end subroutine cfcarm
        end interface
