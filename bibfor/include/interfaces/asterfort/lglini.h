        interface
          subroutine lglini(yd,nbmat,mater,f0,sigd,deps,devg,devgii,&
     &traceg,dy,codret)
            integer :: nbmat
            real(kind=8) :: yd(10)
            real(kind=8) :: mater(nbmat,2)
            real(kind=8) :: f0
            real(kind=8) :: sigd(6)
            real(kind=8) :: deps(6)
            real(kind=8) :: devg(6)
            real(kind=8) :: devgii
            real(kind=8) :: traceg
            real(kind=8) :: dy(10)
            integer :: codret
          end subroutine lglini
        end interface
