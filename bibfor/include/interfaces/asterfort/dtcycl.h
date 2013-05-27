        interface
          subroutine dtcycl(vecdt,nitnew,icycl,dtmin)
            real(kind=8) :: vecdt(*)
            integer :: nitnew
            integer :: icycl
            real(kind=8) :: dtmin
          end subroutine dtcycl
        end interface
