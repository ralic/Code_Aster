        interface
          subroutine vechmx(nomo,lischa,ichar,nbch,nomlis,nbin,lpain,&
     &lchin,lastin,vecele)
            integer :: nbin
            character(len=8) :: nomo
            character(len=19) :: lischa
            integer :: ichar
            integer :: nbch
            character(len=24) :: nomlis
            character(len=8) :: lpain(nbin)
            character(len=19) :: lchin(nbin)
            integer :: lastin
            character(len=19) :: vecele
          end subroutine vechmx
        end interface
