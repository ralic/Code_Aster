        interface
          subroutine mppffn(zimat,nmnbn,nmplas,nmzef,nmzeg,nmief,normm&
     &)
            integer :: zimat
            real(kind=8) :: nmnbn(6)
            real(kind=8) :: nmplas(2,3)
            real(kind=8) :: nmzef
            real(kind=8) :: nmzeg
            integer :: nmief
            real(kind=8) :: normm
          end subroutine mppffn
        end interface
