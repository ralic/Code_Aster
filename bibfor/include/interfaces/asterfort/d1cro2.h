        interface
          subroutine d1cro2(zimat,nmnbn,nmplas,nmdpla,nmddpl,nmprox,&
     &cnbn,cplas,rpara,cief,cdeps,cdtg,cier,cdepsp,dc,bend)
            integer :: zimat
            real(kind=8) :: nmnbn(6)
            real(kind=8) :: nmplas(2,3)
            real(kind=8) :: nmdpla(2,2)
            real(kind=8) :: nmddpl(2,2)
            integer :: nmprox(2)
            real(kind=8) :: cnbn(6)
            real(kind=8) :: cplas(2,3)
            real(kind=8) :: rpara(3)
            integer :: cief
            real(kind=8) :: cdeps(6)
            real(kind=8) :: cdtg(6,6)
            integer :: cier
            real(kind=8) :: cdepsp(6)
            real(kind=8) :: dc(6,6)
            integer :: bend
          end subroutine d1cro2
        end interface
