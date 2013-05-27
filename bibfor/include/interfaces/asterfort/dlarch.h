        interface
          subroutine dlarch(result,neq,istoc,iarchi,texte,alarm,ifm,&
     &temps,nbtyar,typear,masse,depl,vite,acce,fexte,famor,fliai)
            integer :: nbtyar
            integer :: neq
            character(len=8) :: result
            integer :: istoc
            integer :: iarchi
            character(*) :: texte
            integer :: alarm
            integer :: ifm
            real(kind=8) :: temps
            character(len=16) :: typear(nbtyar)
            character(len=8) :: masse
            real(kind=8) :: depl(neq)
            real(kind=8) :: vite(neq)
            real(kind=8) :: acce(neq)
            real(kind=8) :: fexte(neq)
            real(kind=8) :: famor(neq)
            real(kind=8) :: fliai(neq)
          end subroutine dlarch
        end interface
