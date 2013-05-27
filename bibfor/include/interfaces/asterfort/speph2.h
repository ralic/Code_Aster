        interface
          subroutine speph2(movrep,napexc,nbmode,nbpf,intmod,table,&
     &specmr,specmi)
            integer :: nbpf
            character(len=16) :: movrep
            integer :: napexc
            integer :: nbmode
            logical :: intmod
            character(len=8) :: table
            real(kind=8) :: specmr(nbpf,*)
            real(kind=8) :: specmi(nbpf,*)
          end subroutine speph2
        end interface
