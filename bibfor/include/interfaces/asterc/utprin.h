        interface
          subroutine utprin(ch1,num,idmess,nk,valk,ni,vali,nr,valr)
            character(*) :: ch1
            integer :: num
            character(*) :: idmess
            integer :: nk
            character(*) :: valk(*)
            integer :: ni
            integer :: vali(*)
            integer :: nr
            real(kind=8) :: valr(*)
          end subroutine utprin
        end interface
