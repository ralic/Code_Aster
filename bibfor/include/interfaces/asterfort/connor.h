        interface
          subroutine connor(melflu,typflu,freq,base,nuor,amoc,carac,&
     &masg,lnoe,nbm,vite,rho,abscur)
            integer :: nbm
            integer :: lnoe
            character(len=19) :: melflu
            character(len=8) :: typflu
            real(kind=8) :: freq(nbm)
            character(len=8) :: base
            integer :: nuor(nbm)
            real(kind=8) :: amoc(nbm)
            real(kind=8) :: carac(2)
            real(kind=8) :: masg(nbm)
            real(kind=8) :: vite(lnoe)
            real(kind=8) :: rho(2*lnoe)
            real(kind=8) :: abscur(lnoe)
          end subroutine connor
        end interface
