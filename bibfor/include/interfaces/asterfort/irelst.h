        interface
          subroutine irelst(nofimd,chanom,typech,nomaas,nomamd,nbimpr,&
     &caimpi,caimpk,sdcarm)
            integer :: nbimpr
            character(*) :: nofimd
            character(len=19) :: chanom
            character(len=8) :: typech
            character(len=8) :: nomaas
            character(len=64) :: nomamd
            integer :: caimpi(10,nbimpr)
            character(len=80) :: caimpk(3,nbimpr)
            character(len=8) :: sdcarm
          end subroutine irelst
        end interface
