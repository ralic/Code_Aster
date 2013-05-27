        interface
          subroutine noeddl(nume,nbnoe,lnonoe,neq,ivec)
            integer :: neq
            integer :: nbnoe
            character(len=14) :: nume
            character(*) :: lnonoe(nbnoe)
            integer :: ivec(neq)
          end subroutine noeddl
        end interface
