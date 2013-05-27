        interface
          subroutine uexcep(nexc,idmess,nk,valk,ni,vali,nr,valr)
            integer :: nexc
            character(*) :: idmess
            integer :: nk
            character(*) :: valk(*)
            integer :: ni
            integer :: vali(*)
            integer :: nr
            real(kind=8) :: valr(*)
          end subroutine uexcep
        end interface
