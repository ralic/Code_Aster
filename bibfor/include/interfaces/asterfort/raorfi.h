        interface
          subroutine raorfi(noma,ligrel,noepou,cara,coorig,eg1,eg2,eg3&
     &,typrac,rayon)
            character(len=8) :: noma
            character(len=19) :: ligrel
            character(len=8) :: noepou
            character(len=8) :: cara
            real(kind=8) :: coorig(3)
            real(kind=8) :: eg1(3)
            real(kind=8) :: eg2(3)
            real(kind=8) :: eg3(3)
            character(len=8) :: typrac
            real(kind=8) :: rayon
          end subroutine raorfi
        end interface
