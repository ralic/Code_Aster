        interface
          subroutine dstb(carat3,pgl,igau,jacgau,bmat)
            real(kind=8) :: carat3(*)
            real(kind=8) :: pgl(3,3)
            integer :: igau
            real(kind=8) :: jacgau
            real(kind=8) :: bmat(8,1)
          end subroutine dstb
        end interface
