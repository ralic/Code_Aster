        interface
          subroutine ustuen(dimtub,tubuse,rcray,nomt19,ns,parusu,&
     &typusu)
            integer :: dimtub
            real(kind=8) :: tubuse(*)
            real(kind=8) :: rcray
            character(len=19) :: nomt19
            integer :: ns
            real(kind=8) :: parusu(20,*)
            integer :: typusu(*)
          end subroutine ustuen
        end interface
