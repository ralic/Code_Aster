        interface
          subroutine u2mesg(typ,idmess,nk,valk,ni,vali,nr,valr)
            character(*) :: typ
            character(*) :: idmess
            integer :: nk
            character(*) :: valk(*)
            integer :: ni
            integer :: vali(*)
            integer :: nr
            real(kind=8) :: valr(*)
          end subroutine u2mesg
        end interface
