        interface
          subroutine fgdoma(nommat,nbcycl,epsmin,epsmax,dom)
            character(*) :: nommat
            integer :: nbcycl
            real(kind=8) :: epsmin(*)
            real(kind=8) :: epsmax(*)
            real(kind=8) :: dom(*)
          end subroutine fgdoma
        end interface
