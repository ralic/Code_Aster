        interface
          subroutine mdchin(nofimd,idfimd,nochmd,typent,typgeo,prefix,&
     &nbtv,codret)
            character(*) :: nofimd
            integer :: idfimd
            character(*) :: nochmd
            integer :: typent
            integer :: typgeo
            character(len=19) :: prefix
            integer :: nbtv
            integer :: codret
          end subroutine mdchin
        end interface
