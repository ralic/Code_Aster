        interface
          subroutine irmpga(nofimd,chanom,typech,nomtyp,nbimpr,caimpi,&
     &caimpk,modnum,nuanom,sdcarm,codret)
            integer :: nbimpr
            character(*) :: nofimd
            character(len=19) :: chanom
            character(len=8) :: typech
            character(len=8) :: nomtyp(*)
            integer :: caimpi(10,nbimpr)
            character(len=80) :: caimpk(3,nbimpr)
            integer :: modnum(69)
            integer :: nuanom(69,*)
            character(len=8) :: sdcarm
            integer :: codret
          end subroutine irmpga
        end interface
