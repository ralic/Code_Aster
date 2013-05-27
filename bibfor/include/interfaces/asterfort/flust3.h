        interface
          subroutine flust3(melflu,typflu,base,nuor,amor,freq,masg,&
     &fact,vite,nbm,npv,nivpar,nivdef)
            character(len=19) :: melflu
            character(len=8) :: typflu
            character(len=8) :: base
            integer :: nuor(*)
            real(kind=8) :: amor(*)
            real(kind=8) :: freq(*)
            real(kind=8) :: masg(*)
            real(kind=8) :: fact(*)
            real(kind=8) :: vite(*)
            integer :: nbm
            integer :: npv
            integer :: nivpar
            integer :: nivdef
          end subroutine flust3
        end interface
