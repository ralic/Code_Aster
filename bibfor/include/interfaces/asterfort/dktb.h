        interface
          subroutine dktb(carat3,igau,jacgau,bmat)
            real(kind=8) :: carat3(*)
            integer :: igau
            real(kind=8) :: jacgau
            real(kind=8) :: bmat(8,1)
          end subroutine dktb
        end interface
