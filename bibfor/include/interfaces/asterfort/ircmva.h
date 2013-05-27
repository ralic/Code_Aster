        interface
          subroutine ircmva(numcmp,ncmpve,ncmprf,nvalec,nbpg,nbsp,adsv&
     &,adsd,adsl,adsk,partie,tymast,modnum,nuanom,typech,val,profas,ideb&
     &,ifin,codret)
            integer :: nbsp
            integer :: nbpg
            integer :: nvalec
            integer :: ncmprf
            integer :: ncmpve
            integer :: numcmp(ncmprf)
            integer :: adsv
            integer :: adsd
            integer :: adsl
            integer :: adsk
            character(*) :: partie
            integer :: tymast
            integer :: modnum(69)
            integer :: nuanom(69,*)
            character(len=8) :: typech
            real(kind=8) :: val(ncmpve,nbsp,nbpg,nvalec)
            integer :: profas(*)
            integer :: ideb
            integer :: ifin
            integer :: codret
          end subroutine ircmva
        end interface
