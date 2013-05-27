        interface
          subroutine lislfc(excit,ichar,indic,iexcit,nexci,lfcplx,&
     &lacce,fctcsr,nomfct)
            character(len=19) :: excit
            integer :: ichar
            integer :: indic
            integer :: iexcit
            integer :: nexci
            logical :: lfcplx
            logical :: lacce
            character(len=8) :: fctcsr
            character(len=8) :: nomfct
          end subroutine lislfc
        end interface
