        interface
          subroutine irpara(resu,form,ifi,nbordr,ordr,nbpa,nompar,cecr&
     &)
            character(*) :: resu
            character(*) :: form
            integer :: ifi
            integer :: nbordr
            integer :: ordr(*)
            integer :: nbpa
            character(*) :: nompar(*)
            character(*) :: cecr
          end subroutine irpara
        end interface
