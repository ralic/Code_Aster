        interface
          subroutine memaxm(typmx,champ,nocmp,nbcmp,lcmp,vr,nbmail,&
     &numail)
            integer :: nbcmp
            character(*) :: typmx
            character(*) :: champ
            character(*) :: nocmp
            character(*) :: lcmp(*)
            real(kind=8) :: vr(*)
            integer :: nbmail
            integer :: numail(*)
          end subroutine memaxm
        end interface
