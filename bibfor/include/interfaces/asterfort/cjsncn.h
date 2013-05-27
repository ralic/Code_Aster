        interface
          subroutine cjsncn(roucjs,essmax,ndt,nvi,umess,relax,rotagd,&
     &epsd,deps,sigd,vind)
            integer :: nvi
            integer :: ndt
            integer :: essmax
            character(*) :: roucjs
            integer :: umess
            real(kind=8) :: relax(essmax)
            real(kind=8) :: rotagd(essmax)
            real(kind=8) :: epsd(ndt)
            real(kind=8) :: deps(ndt)
            real(kind=8) :: sigd(ndt)
            real(kind=8) :: vind(nvi)
          end subroutine cjsncn
        end interface
