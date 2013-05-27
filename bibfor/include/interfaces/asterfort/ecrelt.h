        interface
          subroutine ecrelt(imod,maxnod,nbtyma,nomail,nbmail,mint,mant&
     &,limail,nbmtot)
            integer :: nbtyma
            integer :: imod
            integer :: maxnod
            character(len=8) :: nomail(nbtyma)
            integer :: nbmail(nbtyma)
            integer :: mint(nbtyma)
            integer :: mant(nbtyma)
            integer :: limail(nbtyma)
            integer :: nbmtot
          end subroutine ecrelt
        end interface
