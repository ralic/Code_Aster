        interface
          subroutine almulr(czero,table,nbval,mantis,expo)
            integer :: nbval
            character(*) :: czero
            real(kind=8) :: table(nbval)
            real(kind=8) :: mantis
            integer :: expo
          end subroutine almulr
        end interface
