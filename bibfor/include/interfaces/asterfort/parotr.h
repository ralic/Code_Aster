        interface
          subroutine parotr(nomma,iageom,ima,nbno,o,mrot,t,coor)
            character(len=8) :: nomma
            integer :: iageom
            integer :: ima
            integer :: nbno
            real(kind=8) :: o(3)
            real(kind=8) :: mrot(3,3)
            real(kind=8) :: t(3)
            real(kind=8) :: coor(*)
          end subroutine parotr
        end interface
