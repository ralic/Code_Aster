        interface
          subroutine flust1(melflu,typflu,base,nuor,amor,amoc,freq,&
     &masg,fact,vite,nbm,calcul,npv,nivpar,nivdef)
            character(len=19) :: melflu
            character(len=8) :: typflu
            character(len=8) :: base
            integer :: nuor(*)
            real(kind=8) :: amor(*)
            real(kind=8) :: amoc(*)
            real(kind=8) :: freq(*)
            real(kind=8) :: masg(*)
            real(kind=8) :: fact(*)
            real(kind=8) :: vite(*)
            integer :: nbm
            logical :: calcul(2)
            integer :: npv
            integer :: nivpar
            integer :: nivdef
          end subroutine flust1
        end interface
