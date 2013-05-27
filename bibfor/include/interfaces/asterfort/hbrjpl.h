        interface
          subroutine hbrjpl(mod,nbmat,materf,sigp,vip,vim,vp,vecp,&
     &dsidep)
            integer :: nbmat
            character(len=8) :: mod(*)
            real(kind=8) :: materf(nbmat,2)
            real(kind=8) :: sigp(6)
            real(kind=8) :: vip(*)
            real(kind=8) :: vim(*)
            real(kind=8) :: vp(3)
            real(kind=8) :: vecp(3,3)
            real(kind=8) :: dsidep(6,6)
          end subroutine hbrjpl
        end interface
