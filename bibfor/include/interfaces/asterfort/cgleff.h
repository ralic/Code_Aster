        interface
          subroutine cgleff(typfis,nomfis,fonoeu,chfond,basfon,taillr,&
     &conf,lnoff)
            character(len=8) :: typfis
            character(len=8) :: nomfis
            character(len=24) :: fonoeu
            character(len=24) :: chfond
            character(len=24) :: basfon
            character(len=24) :: taillr
            character(len=8) :: conf
            integer :: lnoff
          end subroutine cgleff
        end interface
