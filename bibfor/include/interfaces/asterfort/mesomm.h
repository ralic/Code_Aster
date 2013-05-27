        interface
          subroutine mesomm(champ,long,vi,vr,vc,nbmail,numail)
            character(*) :: champ
            integer :: long
            integer :: vi(*)
            real(kind=8) :: vr(*)
            complex(kind=8) :: vc(*)
            integer :: nbmail
            integer :: numail(*)
          end subroutine mesomm
        end interface
