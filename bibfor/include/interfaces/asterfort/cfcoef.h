        interface
          subroutine cfcoef(ndimg,resoco,nbnom,posnsm,coefno,posnoe,&
     &norm,tau1,tau2,coef,cofx,cofy,nbddlt,ddl)
            integer :: ndimg
            character(len=24) :: resoco
            integer :: nbnom
            integer :: posnsm(9)
            real(kind=8) :: coefno(9)
            integer :: posnoe
            real(kind=8) :: norm(3)
            real(kind=8) :: tau1(3)
            real(kind=8) :: tau2(3)
            real(kind=8) :: coef(30)
            real(kind=8) :: cofx(30)
            real(kind=8) :: cofy(30)
            integer :: nbddlt
            integer :: ddl(30)
          end subroutine cfcoef
        end interface
