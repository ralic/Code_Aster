        interface
          subroutine xstano(noma,lisno,nmafis,jmafis,cnslt,cnsln,cnslj&
     &,rayon,cnxinv,stano)
            character(len=8) :: noma
            character(len=24) :: lisno
            integer :: nmafis
            integer :: jmafis
            character(len=19) :: cnslt
            character(len=19) :: cnsln
            character(len=19) :: cnslj
            real(kind=8) :: rayon
            character(len=19) :: cnxinv
            character(len=24) :: stano
          end subroutine xstano
        end interface
