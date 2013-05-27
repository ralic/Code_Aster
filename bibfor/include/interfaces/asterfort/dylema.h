        interface
          subroutine dylema(baseno,nbmat,nomat,raide,masse,amor,impe)
            character(len=8) :: baseno
            integer :: nbmat
            character(len=24) :: nomat(*)
            character(len=19) :: raide
            character(len=19) :: masse
            character(len=19) :: amor
            character(len=19) :: impe
          end subroutine dylema
        end interface
