        interface
          subroutine bijsom(umoy,rhof,r1,r2,long,cf0,icoq,jcoq,jmod,&
     &nbm,rki,thetai,thetaj,tcoef,ysol,bij)
            integer :: nbm
            real(kind=8) :: umoy
            real(kind=8) :: rhof
            real(kind=8) :: r1
            real(kind=8) :: r2
            real(kind=8) :: long
            real(kind=8) :: cf0
            integer :: icoq
            integer :: jcoq
            integer :: jmod
            real(kind=8) :: rki
            real(kind=8) :: thetai
            real(kind=8) :: thetaj
            real(kind=8) :: tcoef(10,nbm)
            complex(kind=8) :: ysol(3,101)
            complex(kind=8) :: bij
          end subroutine bijsom
        end interface
