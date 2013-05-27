        interface
          subroutine xvoise(nnotot,nse,nnop,nno,jcnset,cninv,cvoise)
            integer :: nse
            integer :: nnotot
            integer :: nnop
            integer :: nno
            integer :: jcnset
            integer :: cninv(nnotot,nse+1)
            integer :: cvoise(3,nse)
          end subroutine xvoise
        end interface
