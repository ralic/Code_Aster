        interface
          function ucritp(nbmat,mater,parame,rgdev,invar1)
            integer :: nbmat
            real(kind=8) :: mater(nbmat,2)
            real(kind=8) :: parame(5)
            real(kind=8) :: rgdev
            real(kind=8) :: invar1
            real(kind=8) :: ucritp
          end function ucritp
        end interface
