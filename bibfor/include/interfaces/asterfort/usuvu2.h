        interface
          subroutine usuvu2(puusur,vusur,nbinst,temps,isupp,nbpt,&
     &nbpair,coef,ang,fn,vg,iret,vustub,vusob,pus,pmoye,pourpu,poupre)
            integer :: nbpair
            integer :: nbinst
            real(kind=8) :: puusur
            real(kind=8) :: vusur(*)
            real(kind=8) :: temps(*)
            integer :: isupp
            integer :: nbpt
            real(kind=8) :: coef(*)
            real(kind=8) :: ang(*)
            real(kind=8) :: fn(*)
            real(kind=8) :: vg(*)
            integer :: iret
            real(kind=8) :: vustub(nbpair,nbinst)
            real(kind=8) :: vusob(nbpair,nbinst)
            real(kind=8) :: pus(*)
            real(kind=8) :: pmoye
            real(kind=8) :: pourpu(*)
            real(kind=8) :: poupre(*)
          end subroutine usuvu2
        end interface
