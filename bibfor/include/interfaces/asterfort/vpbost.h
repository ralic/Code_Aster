        interface
          subroutine vpbost(typres,nbmode,nbvect,omeshi,valpro,nvpro,&
     &vpinf,vpmax,precdc,method,omecor)
            integer :: nvpro
            character(len=16) :: typres
            integer :: nbmode
            integer :: nbvect
            real(kind=8) :: omeshi
            real(kind=8) :: valpro(nvpro)
            real(kind=8) :: vpinf
            real(kind=8) :: vpmax
            real(kind=8) :: precdc
            character(len=8) :: method
            real(kind=8) :: omecor
          end subroutine vpbost
        end interface
