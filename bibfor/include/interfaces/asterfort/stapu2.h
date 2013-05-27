        interface
          subroutine stapu2(nbobst,nbpt,nbpair,temps,fcho,vgli,dloc,&
     &coef,ang,wk1,wk2,wk3,wk4,wk5,wk6,idebut,nbloc,nbval,inoe,isupp,&
     &nbinst,tmpvu,pusurn,vustub,vusob,pus,pmoye,pourpu,poupre)
            integer :: nbinst
            integer :: nbpair
            integer :: nbobst
            integer :: nbpt
            real(kind=8) :: temps(*)
            real(kind=8) :: fcho(*)
            real(kind=8) :: vgli(*)
            real(kind=8) :: dloc(*)
            real(kind=8) :: coef(*)
            real(kind=8) :: ang(*)
            real(kind=8) :: wk1(*)
            real(kind=8) :: wk2(*)
            real(kind=8) :: wk3(*)
            real(kind=8) :: wk4(*)
            real(kind=8) :: wk5(*)
            real(kind=8) :: wk6(*)
            integer :: idebut
            integer :: nbloc
            integer :: nbval
            integer :: inoe
            integer :: isupp
            real(kind=8) :: tmpvu(*)
            real(kind=8) :: pusurn
            real(kind=8) :: vustub(nbpair,nbinst)
            real(kind=8) :: vusob(nbpair,nbinst)
            real(kind=8) :: pus(*)
            real(kind=8) :: pmoye
            real(kind=8) :: pourpu(*)
            real(kind=8) :: poupre(*)
          end subroutine stapu2
        end interface
