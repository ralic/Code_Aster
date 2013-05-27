        interface
          subroutine fluimp(itypfl,nivpar,nivdef,melflu,typflu,nuor,&
     &freq,freqi,nbm,vite,npv,carac,calcul,amoc)
            integer :: npv
            integer :: nbm
            integer :: itypfl
            integer :: nivpar
            integer :: nivdef
            character(len=19) :: melflu
            character(len=8) :: typflu
            integer :: nuor(nbm)
            real(kind=8) :: freq(2*nbm*npv)
            real(kind=8) :: freqi(*)
            real(kind=8) :: vite(npv)
            real(kind=8) :: carac(2)
            logical :: calcul(2)
            real(kind=8) :: amoc(*)
          end subroutine fluimp
        end interface
