        interface
          subroutine ccchcf(nomfor,nbcmp,valin,licmp,nbcmpr,valres,&
     &iret)
            integer :: nbcmpr
            integer :: nbcmp
            character(len=8) :: nomfor(nbcmpr)
            real(kind=8) :: valin(nbcmp)
            character(len=8) :: licmp(nbcmp)
            real(kind=8) :: valres(nbcmpr)
            integer :: iret
          end subroutine ccchcf
        end interface
