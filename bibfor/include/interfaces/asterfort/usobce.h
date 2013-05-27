        interface
          subroutine usobce(dimobs,obsuse,rcarte,nomt19,nbusur,parusu,&
     &typusu)
            integer :: dimobs
            real(kind=8) :: obsuse(*)
            real(kind=8) :: rcarte
            character(len=19) :: nomt19
            integer :: nbusur
            real(kind=8) :: parusu(20,*)
            integer :: typusu(*)
          end subroutine usobce
        end interface
