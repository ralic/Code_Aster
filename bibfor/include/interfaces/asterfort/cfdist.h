        interface
          subroutine cfdist(defico,method,izone,posnoe,posmae,coord,&
     &dist)
            character(len=24) :: defico
            character(len=8) :: method
            integer :: izone
            integer :: posnoe
            integer :: posmae
            real(kind=8) :: coord(3)
            real(kind=8) :: dist
          end subroutine cfdist
        end interface
