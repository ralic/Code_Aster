        interface
          subroutine fointr(nomfon,chprol,nbvar,var,fon,nbres,varres,&
     &fonres,ier)
            character(*) :: nomfon
            character(*) :: chprol(*)
            integer :: nbvar
            real(kind=8) :: var(*)
            real(kind=8) :: fon(*)
            integer :: nbres
            real(kind=8) :: varres(*)
            real(kind=8) :: fonres(*)
            integer :: ier
          end subroutine fointr
        end interface
