        interface
          subroutine ircmec(idfimd,nochmd,nomprf,nolopg,numpt,instan,&
     &numord,val,ncmpve,nbenty,nbrepg,nvalec,typent,typgeo,codret)
            integer :: idfimd
            character(*) :: nochmd
            character(*) :: nomprf
            character(*) :: nolopg
            integer :: numpt
            real(kind=8) :: instan
            integer :: numord
            real(kind=8) :: val(*)
            integer :: ncmpve
            integer :: nbenty
            integer :: nbrepg
            integer :: nvalec
            integer :: typent
            integer :: typgeo
            integer :: codret
          end subroutine ircmec
        end interface
