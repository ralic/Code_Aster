        interface
          subroutine rvechb(epsi,typmai,ndfac,r,valcpm,nbcp,nbso,nbsi,&
     &valcp)
            real(kind=8) :: epsi
            character(len=8) :: typmai
            integer :: ndfac(*)
            real(kind=8) :: r(*)
            real(kind=8) :: valcpm(*)
            integer :: nbcp
            integer :: nbso
            integer :: nbsi
            real(kind=8) :: valcp(*)
          end subroutine rvechb
        end interface
