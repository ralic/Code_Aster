        interface
          subroutine morevu(tabpus,dinst,nbsect,sect,voltub,volobs)
            character(*) :: tabpus
            real(kind=8) :: dinst
            integer :: nbsect
            real(kind=8) :: sect(*)
            real(kind=8) :: voltub(*)
            real(kind=8) :: volobs(*)
          end subroutine morevu
        end interface
