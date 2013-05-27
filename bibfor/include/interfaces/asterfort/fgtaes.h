        interface
          subroutine fgtaes(nommat,nomnap,nbcycl,epsmin,epsmax,dom)
            character(*) :: nommat
            character(*) :: nomnap
            integer :: nbcycl
            real(kind=8) :: epsmin(*)
            real(kind=8) :: epsmax(*)
            real(kind=8) :: dom(*)
          end subroutine fgtaes
        end interface
