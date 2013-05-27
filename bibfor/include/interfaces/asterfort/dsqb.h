        interface
          subroutine dsqb(caraq4,xyzl,pgl,igau,jacgau,bmat)
            real(kind=8) :: caraq4(*)
            real(kind=8) :: xyzl(3,1)
            real(kind=8) :: pgl(3,3)
            integer :: igau
            real(kind=8) :: jacgau
            real(kind=8) :: bmat(8,1)
          end subroutine dsqb
        end interface
