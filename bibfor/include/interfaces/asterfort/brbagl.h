        interface
          subroutine brbagl(zimat,nmnbn,nmplas,nmdpla,nmddpl,nmzef,&
     &nmzeg,nmief,nmprox,depsp,ddissi,dc1,dc2,dtg,bbok,normm,normn)
            integer :: zimat
            real(kind=8) :: nmnbn(6)
            real(kind=8) :: nmplas(2,3)
            real(kind=8) :: nmdpla(2,2)
            real(kind=8) :: nmddpl(2,2)
            real(kind=8) :: nmzef
            real(kind=8) :: nmzeg
            integer :: nmief
            integer :: nmprox(2)
            real(kind=8) :: depsp(6)
            real(kind=8) :: ddissi
            real(kind=8) :: dc1(6,6)
            real(kind=8) :: dc2(6,6)
            real(kind=8) :: dtg(6,6)
            logical :: bbok
            real(kind=8) :: normm
            real(kind=8) :: normn
          end subroutine brbagl
        end interface
