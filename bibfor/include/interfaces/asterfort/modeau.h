        interface
          subroutine modeau(melflu,noma,geom,fsvr,base,freqi,nbm,nuor,&
     &vicoq,torco,tcoef,amor,masg,fact,amfr,vecpr,maj)
            integer :: nbm
            character(len=19) :: melflu
            character(len=8) :: noma
            real(kind=8) :: geom(9)
            real(kind=8) :: fsvr(7)
            character(len=8) :: base
            real(kind=8) :: freqi(*)
            integer :: nuor(nbm)
            integer :: vicoq(nbm)
            real(kind=8) :: torco(4,nbm)
            real(kind=8) :: tcoef(10,nbm)
            real(kind=8) :: amor(nbm)
            real(kind=8) :: masg(nbm)
            real(kind=8) :: fact(nbm)
            real(kind=8) :: amfr(nbm,2)
            real(kind=8) :: vecpr(nbm,nbm)
            real(kind=8) :: maj(nbm)
          end subroutine modeau
        end interface
