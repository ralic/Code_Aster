        interface
          subroutine zeropn(kstop,degre,ai,racine,ier)
            integer :: degre
            character(len=1) :: kstop
            real(kind=8) :: ai(degre)
            real(kind=8) :: racine(2*degre)
            integer :: ier
          end subroutine zeropn
        end interface
