        interface
          subroutine ecrcho(iordre,nbnl,old,depbut,vitbut,forbut)
            integer :: nbnl
            integer :: iordre
            real(kind=8) :: old(9,*)
            real(kind=8) :: depbut(nbnl,3,*)
            real(kind=8) :: vitbut(nbnl,3,*)
            real(kind=8) :: forbut(nbnl,3,*)
          end subroutine ecrcho
        end interface
