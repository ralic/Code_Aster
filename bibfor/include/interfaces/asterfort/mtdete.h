        interface
          subroutine mtdete(option,method,lmat,mantis,expo,cmod)
            integer :: option
            character(len=24) :: method
            integer :: lmat
            real(kind=8) :: mantis
            integer :: expo
            complex(kind=8) :: cmod
          end subroutine mtdete
        end interface
