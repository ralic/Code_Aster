        interface
          subroutine initia(neq,lgrot,indro,chamro,chamin)
            integer :: neq
            logical :: lgrot
            integer :: indro(*)
            real(kind=8) :: chamro(*)
            real(kind=8) :: chamin(*)
          end subroutine initia
        end interface
