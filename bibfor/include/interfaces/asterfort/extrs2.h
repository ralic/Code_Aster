        interface
          subroutine extrs2(resu0,resu1,typcon,lrest,mailla,modele,&
     &nbordr,nuordr,nbacc,nomacc,nbarch,nuarch,nbexcl,chexcl,nbnosy)
            character(*) :: resu0
            character(*) :: resu1
            character(len=16) :: typcon
            logical :: lrest
            character(len=8) :: mailla
            character(len=8) :: modele
            integer :: nbordr
            integer :: nuordr(*)
            integer :: nbacc
            character(len=16) :: nomacc(*)
            integer :: nbarch
            integer :: nuarch(*)
            integer :: nbexcl
            character(len=16) :: chexcl(*)
            integer :: nbnosy
          end subroutine extrs2
        end interface
