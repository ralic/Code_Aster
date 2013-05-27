        interface
          subroutine ccchcr(crit,nomgd,nbcmp,valin,licmp,nbcmpr,valres&
     &,iret)
            integer :: nbcmpr
            integer :: nbcmp
            character(len=16) :: crit
            character(len=8) :: nomgd
            real(kind=8) :: valin(nbcmp)
            character(len=8) :: licmp(nbcmp)
            real(kind=8) :: valres(nbcmpr)
            integer :: iret
          end subroutine ccchcr
        end interface
