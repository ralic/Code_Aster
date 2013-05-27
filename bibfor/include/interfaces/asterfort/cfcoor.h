        interface
          subroutine cfcoor(noma,defico,newgeo,posmam,ksi1,ksi2,coordp&
     &)
            character(len=8) :: noma
            character(len=24) :: defico
            character(len=19) :: newgeo
            integer :: posmam
            real(kind=8) :: ksi1
            real(kind=8) :: ksi2
            real(kind=8) :: coordp(3)
          end subroutine cfcoor
        end interface
