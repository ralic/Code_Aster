        interface
          subroutine nmnet1(zimat,nmnbn,cnbn,cplas,czef,czeg,cief,&
     &cdeps,cdtg,cier,cdepsp,dc,normm)
            integer :: zimat
            real(kind=8) :: nmnbn(6)
            real(kind=8) :: cnbn(6)
            real(kind=8) :: cplas(2,3)
            real(kind=8) :: czef
            real(kind=8) :: czeg
            integer :: cief
            real(kind=8) :: cdeps(6)
            real(kind=8) :: cdtg(6,6)
            integer :: cier
            real(kind=8) :: cdepsp(6)
            real(kind=8) :: dc(6,6)
            real(kind=8) :: normm
          end subroutine nmnet1
        end interface
