        interface
          subroutine rvchlm(ssch19,m2d,noeud,nbn,nbcmp,nbco,nbsp,val)
            character(len=19) :: ssch19
            integer :: m2d
            integer :: noeud(*)
            integer :: nbn
            integer :: nbcmp
            integer :: nbco
            integer :: nbsp
            real(kind=8) :: val(*)
          end subroutine rvchlm
        end interface
