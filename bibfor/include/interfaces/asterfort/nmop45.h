        interface
          subroutine nmop45(matrig,matgeo,defo,option,nfreq,cdsp,bande&
     &,mod45,ddlexc,nddle,modes,modes2,ddlsta,nsta)
            character(len=19) :: matrig
            character(len=19) :: matgeo
            integer :: defo
            character(len=16) :: option
            integer :: nfreq
            integer :: cdsp
            real(kind=8) :: bande(2)
            character(len=4) :: mod45
            character(len=24) :: ddlexc
            integer :: nddle
            character(len=8) :: modes
            character(len=8) :: modes2
            character(len=24) :: ddlsta
            integer :: nsta
          end subroutine nmop45
        end interface
