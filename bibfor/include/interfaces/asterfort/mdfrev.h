        interface
          subroutine mdfrev(nbmode,vitgen,fexgen,nbrevi,dplrev,fonrev)
            integer :: nbrevi
            integer :: nbmode
            real(kind=8) :: vitgen(*)
            real(kind=8) :: fexgen(*)
            real(kind=8) :: dplrev(nbrevi,nbmode,*)
            character(len=8) :: fonrev(nbrevi,*)
          end subroutine mdfrev
        end interface
