        interface
          subroutine hujncv(rouhuj,nitimp,iter,ndt,nvi,umess,erimp,&
     &deps,sigd,vind)
            integer :: nvi
            integer :: ndt
            integer :: nitimp
            character(*) :: rouhuj
            integer :: iter
            integer :: umess
            real(kind=8) :: erimp(nitimp,3)
            real(kind=8) :: deps(ndt)
            real(kind=8) :: sigd(ndt)
            real(kind=8) :: vind(nvi)
          end subroutine hujncv
        end interface
