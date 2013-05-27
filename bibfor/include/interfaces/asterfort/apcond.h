        interface
          subroutine apcond(sdappa,newgeo,numno,coorno)
            character(len=19) :: sdappa
            character(len=19) :: newgeo
            integer :: numno
            real(kind=8) :: coorno(3)
          end subroutine apcond
        end interface
