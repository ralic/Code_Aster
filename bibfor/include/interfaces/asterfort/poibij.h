        interface
          subroutine poibij(npv,vabs,geom,fsvr,nbm,vicoq,torco,tcoef,&
     &freq,imasse,maj,vecpr)
            integer :: nbm
            integer :: npv
            real(kind=8) :: vabs(npv)
            real(kind=8) :: geom(9)
            real(kind=8) :: fsvr(7)
            integer :: vicoq(nbm)
            real(kind=8) :: torco(4,nbm)
            real(kind=8) :: tcoef(10,nbm)
            real(kind=8) :: freq(2*nbm*npv)
            integer :: imasse
            real(kind=8) :: maj(nbm)
            real(kind=8) :: vecpr(*)
          end subroutine poibij
        end interface
