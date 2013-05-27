        interface
          subroutine mdexch(nofimd,idfimd,nochmd,numpt,numord,nbcmpc,&
     &nomcmc,nbvato,typent,typgeo,existc,nbcmfi,nmcmfi,nbval,codret)
            character(*) :: nofimd
            integer :: idfimd
            character(*) :: nochmd
            integer :: numpt
            integer :: numord
            integer :: nbcmpc
            character(*) :: nomcmc
            integer :: nbvato
            integer :: typent
            integer :: typgeo
            integer :: existc
            integer :: nbcmfi
            character(*) :: nmcmfi
            integer :: nbval
            integer :: codret
          end subroutine mdexch
        end interface
