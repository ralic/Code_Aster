        interface
          subroutine dpvpot(mod,vim,vip,nbmat,mater,sig,dt,dp,plas,&
     &dsidep)
            integer :: nbmat
            character(len=8) :: mod
            real(kind=8) :: vim(4)
            real(kind=8) :: vip(4)
            real(kind=8) :: mater(nbmat,2)
            real(kind=8) :: sig(6)
            real(kind=8) :: dt
            real(kind=8) :: dp
            real(kind=8) :: plas
            real(kind=8) :: dsidep(6,6)
          end subroutine dpvpot
        end interface
