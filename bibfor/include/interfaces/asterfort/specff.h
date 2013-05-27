        interface
          subroutine specff(casint,nomu,spectr,base,nuor,imodi,imodf,&
     &nbm,nbpf)
            integer :: nbm
            logical :: casint
            character(len=8) :: nomu
            character(len=19) :: spectr
            character(len=19) :: base
            integer :: nuor(nbm)
            integer :: imodi
            integer :: imodf
            integer :: nbpf
          end subroutine specff
        end interface
