        interface
          subroutine cvmjpl(mod,nmat,mater,timed,timef,epsd,deps,sigf,&
     &vinf,sigd,vind,nvi,nr,dsde)
            common/tdim/ ndt,ndi
              integer :: ndt
              integer :: ndi
            integer :: nr
            integer :: nvi
            integer :: nmat
            character(len=8) :: mod
            real(kind=8) :: mater(nmat,2)
            real(kind=8) :: timed
            real(kind=8) :: timef
            real(kind=8) :: epsd(*)
            real(kind=8) :: deps(*)
            real(kind=8) :: sigf(*)
            real(kind=8) :: vinf(*)
            real(kind=8) :: sigd(*)
            real(kind=8) :: vind(*)
            real(kind=8) :: dsde(6,6)
          end subroutine cvmjpl
        end interface
