        interface
          subroutine cjsncv(roucjs,nitimp,iter,ndt,nvi,umess,erimp,&
     &epsd,deps,sigd,vind)
            integer :: nvi
            integer :: ndt
            integer :: nitimp
            character(*) :: roucjs
            integer :: iter
            integer :: umess
            real(kind=8) :: erimp(nitimp,3)
            real(kind=8) :: epsd(ndt)
            real(kind=8) :: deps(ndt)
            real(kind=8) :: sigd(ndt)
            real(kind=8) :: vind(nvi)
          end subroutine cjsncv
        end interface
