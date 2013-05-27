        interface
          subroutine fointc(codmes,nomf,nbpu,nompu,valpu,resure,resuim&
     &,ier)
            character(*) :: codmes
            character(*) :: nomf
            integer :: nbpu
            character(*) :: nompu(*)
            real(kind=8) :: valpu(*)
            real(kind=8) :: resure
            real(kind=8) :: resuim
            integer :: ier
          end subroutine fointc
        end interface
