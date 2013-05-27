        interface
          subroutine spect1(casint,nomu,spectr,ispect,base,vite,nuor,&
     &imodi,imodf,nbm,nbpf,nomzon,vmoyzi,vmoyto)
            integer :: nbm
            logical :: casint
            character(len=8) :: nomu
            character(len=19) :: spectr
            integer :: ispect
            character(len=19) :: base
            real(kind=8) :: vite
            integer :: nuor(nbm)
            integer :: imodi
            integer :: imodf
            integer :: nbpf
            character(len=8) :: nomzon
            real(kind=8) :: vmoyzi
            real(kind=8) :: vmoyto
          end subroutine spect1
        end interface
