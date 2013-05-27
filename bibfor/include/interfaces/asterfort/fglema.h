        interface
          subroutine fglema(nbf,nbpoin,sig,defpla,temp,nommat,dom)
            integer :: nbf
            integer :: nbpoin
            real(kind=8) :: sig(*)
            real(kind=8) :: defpla(*)
            real(kind=8) :: temp(*)
            character(*) :: nommat
            real(kind=8) :: dom(*)
          end subroutine fglema
        end interface
