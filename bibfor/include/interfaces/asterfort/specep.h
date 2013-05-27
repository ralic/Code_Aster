        interface
          subroutine specep(casint,nomu,spectr,base,vite,nuor,imodi,&
     &imodf,nbm,nbpf)
            integer :: nbm
            logical :: casint
            character(len=8) :: nomu
            character(len=19) :: spectr
            character(len=19) :: base
            real(kind=8) :: vite
            integer :: nuor(nbm)
            integer :: imodi
            integer :: imodf
            integer :: nbpf
          end subroutine specep
        end interface
