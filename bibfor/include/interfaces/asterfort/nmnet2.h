        interface
          subroutine nmnet2(zimat,nmnbn,cnbn,cplas,czef,czeg,cief,&
     &cdeps,cdtg,cier,dc1,dc2,depsp2,normm)
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
            real(kind=8) :: dc1(6,6)
            real(kind=8) :: dc2(6,6)
            real(kind=8) :: depsp2(6,2)
            real(kind=8) :: normm
          end subroutine nmnet2
        end interface
