        interface
          subroutine approj(sdappa,noma,newgeo,defico,posnom,dirapp,&
     &dir,itemax,epsmax,toleou,coorpt,posmam,iprojm,ksi1m,ksi2m,tau1m,&
     &tau2m,distm,vecpmm)
            character(len=19) :: sdappa
            character(len=8) :: noma
            character(len=19) :: newgeo
            character(len=24) :: defico
            integer :: posnom
            logical :: dirapp
            real(kind=8) :: dir(3)
            integer :: itemax
            real(kind=8) :: epsmax
            real(kind=8) :: toleou
            real(kind=8) :: coorpt(3)
            integer :: posmam
            integer :: iprojm
            real(kind=8) :: ksi1m
            real(kind=8) :: ksi2m
            real(kind=8) :: tau1m(3)
            real(kind=8) :: tau2m(3)
            real(kind=8) :: distm
            real(kind=8) :: vecpmm(3)
          end subroutine approj
        end interface
