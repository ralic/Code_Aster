        interface
          subroutine raire2(noma,rigi,nbgr,ligrma,nbnoeu,nbno,tabnoe,&
     &rignoe)
            integer :: nbnoeu
            integer :: nbgr
            character(len=8) :: noma
            real(kind=8) :: rigi(6)
            character(len=24) :: ligrma(nbgr)
            integer :: nbno
            integer :: tabnoe(nbnoeu)
            real(kind=8) :: rignoe(6*nbnoeu)
          end subroutine raire2
        end interface
