        interface
          subroutine fgtaep(nommat,nomfo1,nomnap,nbcycl,epsmin,epsmax,&
     &dom)
            character(*) :: nommat
            character(*) :: nomfo1
            character(*) :: nomnap
            integer :: nbcycl
            real(kind=8) :: epsmin(*)
            real(kind=8) :: epsmax(*)
            real(kind=8) :: dom(*)
          end subroutine fgtaep
        end interface
