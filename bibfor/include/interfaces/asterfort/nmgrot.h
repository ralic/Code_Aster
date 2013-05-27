        interface
          subroutine nmgrot(iran,deldet,theta,chamaj)
            integer :: iran(3)
            real(kind=8) :: deldet(3)
            real(kind=8) :: theta(3)
            real(kind=8) :: chamaj(*)
          end subroutine nmgrot
        end interface
