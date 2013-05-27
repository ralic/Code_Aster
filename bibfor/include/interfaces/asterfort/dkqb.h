        interface
          subroutine dkqb(caraq4,xyzl,igau,jacgau,bmat)
            real(kind=8) :: caraq4(*)
            real(kind=8) :: xyzl(3,1)
            integer :: igau
            real(kind=8) :: jacgau
            real(kind=8) :: bmat(8,1)
          end subroutine dkqb
        end interface
