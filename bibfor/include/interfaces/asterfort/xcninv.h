        interface
          subroutine xcninv(nnotot,nse,nnop,nno,jcnset,cninv)
            integer :: nse
            integer :: nnotot
            integer :: nnop
            integer :: nno
            integer :: jcnset
            integer :: cninv(nnotot,nse+1)
          end subroutine xcninv
        end interface
