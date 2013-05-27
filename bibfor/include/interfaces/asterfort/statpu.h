        interface
          subroutine statpu(nbobst,nbpt,temps,fcho,vgli,iadh,wk1,wk2,&
     &wk3,iwk4,idebut,nbloc,nbval,ifires,inoe,impr,pusurn)
            integer :: nbobst
            integer :: nbpt
            real(kind=8) :: temps(*)
            real(kind=8) :: fcho(*)
            real(kind=8) :: vgli(*)
            integer :: iadh(*)
            real(kind=8) :: wk1(*)
            real(kind=8) :: wk2(*)
            real(kind=8) :: wk3(*)
            integer :: iwk4(*)
            integer :: idebut
            integer :: nbloc
            integer :: nbval
            integer :: ifires
            integer :: inoe
            integer :: impr
            real(kind=8) :: pusurn
          end subroutine statpu
        end interface
