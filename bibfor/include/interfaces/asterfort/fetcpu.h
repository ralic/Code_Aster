        interface
          subroutine fetcpu(option,temps,infofe,rang,ifm)
            integer :: option
            real(kind=8) :: temps(6)
            character(len=24) :: infofe
            integer :: rang
            integer :: ifm
          end subroutine fetcpu
        end interface
