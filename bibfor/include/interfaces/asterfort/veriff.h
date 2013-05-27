        interface
          subroutine veriff(nbfonc,nomfon,nbp1,nbp2,long)
            integer :: nbfonc
            character(len=8) :: nomfon(nbfonc)
            integer :: nbp1
            integer :: nbp2
            real(kind=8) :: long
          end subroutine veriff
        end interface
