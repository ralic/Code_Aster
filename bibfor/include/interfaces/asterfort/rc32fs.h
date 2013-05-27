        interface
          subroutine rc32fs(nbsigr,nocc,situ,fuijs,fuij,fuse,ns,nscy,&
     &ug)
            integer :: nbsigr
            integer :: nocc(*)
            integer :: situ(*)
            real(kind=8) :: fuijs(*)
            real(kind=8) :: fuij(*)
            real(kind=8) :: fuse
            integer :: ns
            integer :: nscy
            real(kind=8) :: ug
          end subroutine rc32fs
        end interface
