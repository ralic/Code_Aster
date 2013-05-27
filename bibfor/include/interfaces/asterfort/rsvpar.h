        interface
          subroutine rsvpar(nomsd,iordr,nompar,ipar,rpar,kpar,ier)
            character(*) :: nomsd
            integer :: iordr
            character(*) :: nompar
            integer :: ipar
            real(kind=8) :: rpar
            character(*) :: kpar
            integer :: ier
          end subroutine rsvpar
        end interface
