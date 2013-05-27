        interface
          subroutine extrai(nin,lchin,lpain,opt,nute,ligrel,init)
            integer :: nin
            character(*) :: lchin(*)
            character(len=8) :: lpain(*)
            integer :: opt
            integer :: nute
            character(len=19) :: ligrel
            character(*) :: init
          end subroutine extrai
        end interface
