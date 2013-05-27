        interface
          subroutine nmextl(noma,nomo,motfac,iocc,nomcha,typcha,listno&
     &,listma,nbno,nbma,extrch)
            character(len=8) :: noma
            character(len=8) :: nomo
            character(len=16) :: motfac
            integer :: iocc
            character(len=24) :: nomcha
            character(len=4) :: typcha
            character(len=24) :: listno
            character(len=24) :: listma
            integer :: nbno
            integer :: nbma
            character(len=8) :: extrch
          end subroutine nmextl
        end interface
