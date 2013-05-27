        interface
          subroutine ircam1(nofimd,nochmd,existc,ncmprf,numpt,instan,&
     &numord,adsd,adsv,adsl,adsk,partie,ncmpve,ntlcmp,ntncmp,ntucmp,&
     &ntproa,nbimpr,caimpi,caimpk,typech,nomamd,nomtyp,modnum,nuanom,&
     &codret)
            integer :: nbimpr
            character(*) :: nofimd
            character(len=64) :: nochmd
            integer :: existc
            integer :: ncmprf
            integer :: numpt
            real(kind=8) :: instan
            integer :: numord
            integer :: adsd
            integer :: adsv
            integer :: adsl
            integer :: adsk
            character(*) :: partie
            integer :: ncmpve
            character(len=24) :: ntlcmp
            character(len=24) :: ntncmp
            character(len=24) :: ntucmp
            character(len=24) :: ntproa
            integer :: caimpi(10,nbimpr)
            character(*) :: caimpk(3,nbimpr)
            character(len=8) :: typech
            character(*) :: nomamd
            character(len=8) :: nomtyp(*)
            integer :: modnum(69)
            integer :: nuanom(69,*)
            integer :: codret
          end subroutine ircam1
        end interface
