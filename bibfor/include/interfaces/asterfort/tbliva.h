        interface
          subroutine tbliva(nomta,npacri,lipacr,vi,vr,vc,vk,crit,prec,&
     &para,ctype,vali,valr,valc,valk,ier)
            character(*) :: nomta
            integer :: npacri
            character(*) :: lipacr(*)
            integer :: vi(*)
            real(kind=8) :: vr(*)
            complex(kind=8) :: vc(*)
            character(*) :: vk(*)
            character(*) :: crit(*)
            real(kind=8) :: prec(*)
            character(*) :: para
            character(*) :: ctype
            integer :: vali
            real(kind=8) :: valr
            complex(kind=8) :: valc
            character(*) :: valk
            integer :: ier
          end subroutine tbliva
        end interface
