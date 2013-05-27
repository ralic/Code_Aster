        interface
          subroutine irmaes(idfimd,nomaas,nomamd,nbimpr,caimpi,modnum,&
     &nuanom,nomtyp,nnotyp,sdcarm)
            integer :: nbimpr
            integer :: idfimd
            character(len=8) :: nomaas
            character(len=64) :: nomamd
            integer :: caimpi(10,nbimpr)
            integer :: modnum(69)
            integer :: nuanom(69,*)
            character(len=8) :: nomtyp(*)
            integer :: nnotyp(*)
            character(len=8) :: sdcarm
          end subroutine irmaes
        end interface
