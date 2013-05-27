        interface
          subroutine veripl(ma,nbma,linuma,ang,typerr)
            integer :: nbma
            character(*) :: ma
            integer :: linuma(nbma)
            real(kind=8) :: ang
            character(len=1) :: typerr
          end subroutine veripl
        end interface
