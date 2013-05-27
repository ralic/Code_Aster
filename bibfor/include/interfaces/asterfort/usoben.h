        interface
          subroutine usoben(guidag,dimobs,obsuse,nco,rayo,thet,nbsect,&
     &parusu,typusu,nomt19,arete,arete2,rcarte,denc)
            character(len=8) :: guidag
            integer :: dimobs
            real(kind=8) :: obsuse(*)
            integer :: nco
            real(kind=8) :: rayo(*)
            real(kind=8) :: thet(*)
            integer :: nbsect
            real(kind=8) :: parusu(20,*)
            integer :: typusu(*)
            character(len=19) :: nomt19
            real(kind=8) :: arete
            real(kind=8) :: arete2
            real(kind=8) :: rcarte
            real(kind=8) :: denc
          end subroutine usoben
        end interface
