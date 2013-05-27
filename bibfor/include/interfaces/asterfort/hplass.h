        interface
          subroutine hplass(nmnbn,nmplas,nmdpla,nmddpl,bend,hplas)
            real(kind=8) :: nmnbn(6)
            real(kind=8) :: nmplas(2,3)
            real(kind=8) :: nmdpla(2,2)
            real(kind=8) :: nmddpl(2,2)
            integer :: bend
            real(kind=8) :: hplas(6,*)
          end subroutine hplass
        end interface
