        interface
          subroutine ordcoq(imod,nbm,icoq,nbno,numno,inomax,nbnoto,&
     &coordo,iaxe,defm,nunoe0,drmax,torco)
            integer :: nbnoto
            integer :: nbno
            integer :: nbm
            integer :: imod
            integer :: icoq
            integer :: numno(nbno)
            integer :: inomax
            real(kind=8) :: coordo(3,nbnoto)
            integer :: iaxe
            real(kind=8) :: defm(2,nbnoto,nbm)
            integer :: nunoe0
            real(kind=8) :: drmax
            real(kind=8) :: torco(4,nbm)
          end subroutine ordcoq
        end interface
