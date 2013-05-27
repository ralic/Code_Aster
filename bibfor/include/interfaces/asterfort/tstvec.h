        interface
          subroutine tstvec(perm,iad,nlong,type,sommi,sommr,nbign)
            character(*) :: perm
            integer :: iad
            integer :: nlong
            character(len=3) :: type
            integer :: sommi
            real(kind=8) :: sommr
            integer :: nbign
          end subroutine tstvec
        end interface
