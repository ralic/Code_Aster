        interface
          subroutine coedef(imod,fremod,nbm,young,poiss,rho,icoq,nbno,&
     &numno,nunoe0,nbnoto,coordo,iaxe,kec,geom,defm,drmax,torco,tcoef)
            integer :: nbnoto
            integer :: nbno
            integer :: nbm
            integer :: imod
            real(kind=8) :: fremod
            real(kind=8) :: young
            real(kind=8) :: poiss
            real(kind=8) :: rho
            integer :: icoq
            integer :: numno(nbno)
            integer :: nunoe0
            real(kind=8) :: coordo(3,nbnoto)
            integer :: iaxe
            integer :: kec
            real(kind=8) :: geom(9)
            real(kind=8) :: defm(2,nbnoto,nbm)
            real(kind=8) :: drmax
            real(kind=8) :: torco(4,nbm)
            real(kind=8) :: tcoef(10,nbm)
          end subroutine coedef
        end interface
