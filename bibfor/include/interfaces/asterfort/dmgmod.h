        interface
          subroutine dmgmod(nomsym,nomsd,nomsd2,nommat,nbordr,jordr,&
     &jcoef,nbpt,ntcmp,numcmp,impr,vdomag)
            integer :: nbordr
            character(len=16) :: nomsym
            character(len=19) :: nomsd
            character(len=19) :: nomsd2
            character(len=8) :: nommat
            integer :: jordr
            integer :: jcoef
            integer :: nbpt
            integer :: ntcmp
            integer :: numcmp(*)
            integer :: impr
            real(kind=8) :: vdomag(*)
          end subroutine dmgmod
        end interface
