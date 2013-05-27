        interface
          subroutine lgljpl(mod,nbmat,mater,sig,devg,devgii,vin,dsde,&
     &codret)
            integer :: nbmat
            character(len=8) :: mod
            real(kind=8) :: mater(nbmat,2)
            real(kind=8) :: sig(6)
            real(kind=8) :: devg(6)
            real(kind=8) :: devgii
            real(kind=8) :: vin(*)
            real(kind=8) :: dsde(6,6)
            integer :: codret
          end subroutine lgljpl
        end interface
