        interface
          subroutine pemica(champ,long,vr,nbmail,nummai,orig,iorig,&
     &icage)
            character(*) :: champ
            integer :: long
            real(kind=8) :: vr(*)
            integer :: nbmail
            integer :: nummai(*)
            real(kind=8) :: orig(3)
            integer :: iorig
            integer :: icage
          end subroutine pemica
        end interface
