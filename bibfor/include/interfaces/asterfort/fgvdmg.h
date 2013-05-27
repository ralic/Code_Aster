        interface
          subroutine fgvdmg(nomsym,nomsd,nommat,nomnap,nomfon,mexpic,&
     &mcompt,mdomag,nbord,nbpt,ntcmp,nbcmp,numcmp,impr,vdomag)
            character(len=16) :: nomsym
            character(len=19) :: nomsd
            character(len=8) :: nommat
            character(len=8) :: nomnap
            character(len=8) :: nomfon
            character(*) :: mexpic
            character(*) :: mcompt
            character(*) :: mdomag
            integer :: nbord
            integer :: nbpt
            integer :: ntcmp
            integer :: nbcmp
            integer :: numcmp(*)
            integer :: impr
            real(kind=8) :: vdomag(*)
          end subroutine fgvdmg
        end interface
