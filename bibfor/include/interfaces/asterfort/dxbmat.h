        interface
          subroutine dxbmat(nomte,cara,xyzl,pgl,igau,jacgau,bmat)
            character(len=16) :: nomte
            real(kind=8) :: cara(*)
            real(kind=8) :: xyzl(3,1)
            real(kind=8) :: pgl(3,1)
            integer :: igau
            real(kind=8) :: jacgau
            real(kind=8) :: bmat(8,1)
          end subroutine dxbmat
        end interface
