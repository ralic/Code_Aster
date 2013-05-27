        interface
          subroutine utexcm(num,idmess,nk,valk,ni,vali,nr,valr)
            integer :: num
            character(*) :: idmess
            integer :: nk
            character(*) :: valk(*)
            integer :: ni
            integer :: vali(*)
            integer :: nr
            real(kind=8) :: valr(*)
          end subroutine utexcm
        end interface
