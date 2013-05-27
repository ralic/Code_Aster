        interface
          subroutine uttcpl(dim,nbmesu,nomc,noml,prpal)
            integer :: dim
            integer :: nbmesu
            character(len=24) :: nomc(dim)
            character(len=80) :: noml(dim)
            character(len=1) :: prpal(dim)
          end subroutine uttcpl
        end interface
