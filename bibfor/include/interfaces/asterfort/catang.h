        interface
          subroutine catang(noma,nbma,numail,nbno,nunoeu,tang)
            integer :: nbno
            integer :: nbma
            character(len=8) :: noma
            integer :: numail(nbma)
            integer :: nunoeu(nbno)
            real(kind=8) :: tang(3*nbno)
          end subroutine catang
        end interface
