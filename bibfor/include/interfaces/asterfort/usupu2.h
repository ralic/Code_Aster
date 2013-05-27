        interface
          subroutine usupu2(nbpt,nbpair,coef,ang,isupp,nbinst,temps,&
     &puusur,vustub,vusob,pus,pmoye,pourpu,poupre)
            integer :: nbinst
            integer :: nbpair
            integer :: nbpt
            real(kind=8) :: coef(*)
            real(kind=8) :: ang(*)
            integer :: isupp
            real(kind=8) :: temps(*)
            real(kind=8) :: puusur
            real(kind=8) :: vustub(nbpair,nbinst)
            real(kind=8) :: vusob(nbpair,nbinst)
            real(kind=8) :: pus(*)
            real(kind=8) :: pmoye
            real(kind=8) :: pourpu(*)
            real(kind=8) :: poupre(*)
          end subroutine usupu2
        end interface
