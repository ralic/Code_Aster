        interface
          subroutine peenca(champ,long,vr,nbmail,nummai)
            integer :: long
            character(*) :: champ
            real(kind=8) :: vr(long)
            integer :: nbmail
            integer :: nummai(*)
          end subroutine peenca
        end interface
