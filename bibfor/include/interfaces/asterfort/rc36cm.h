        interface
          subroutine rc36cm(iocc,etat,nbma,listma,nbchar,lichar,chmome&
     &)
            integer :: iocc
            character(len=1) :: etat
            integer :: nbma
            integer :: listma(*)
            integer :: nbchar
            integer :: lichar(*)
            character(len=24) :: chmome
          end subroutine rc36cm
        end interface
