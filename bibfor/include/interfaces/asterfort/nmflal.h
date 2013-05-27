        interface
          subroutine nmflal(option,compor,sdpost,mod45,defo,nfreq,cdsp&
     &,typmat,optmod,bande,nddle,ddlexc,nsta,ddlsta,modrig)
            character(len=16) :: option
            character(len=24) :: compor
            character(len=19) :: sdpost
            character(len=4) :: mod45
            integer :: defo
            integer :: nfreq
            integer :: cdsp
            character(len=16) :: typmat
            character(len=16) :: optmod
            real(kind=8) :: bande(2)
            integer :: nddle
            character(len=24) :: ddlexc
            integer :: nsta
            character(len=24) :: ddlsta
            character(len=16) :: modrig
          end subroutine nmflal
        end interface
