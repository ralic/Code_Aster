        interface
          subroutine impmv(ifm,txt,mv,n,isym)
            integer :: n
            integer :: ifm
            character(len=8) :: txt
            real(kind=8) :: mv(n)
            integer :: isym
          end subroutine impmv
        end interface
