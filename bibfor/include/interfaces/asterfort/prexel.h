        interface
          subroutine prexel(champ,ioc,mamax,nomax,ispmax,cmpmax,valmax&
     &,mamin,nomin,ispmin,cmpmin,valmin,maamax,noamax,isamax,cmamax,&
     &vaamax,maamin,noamin,isamin,cmamin,vaamin)
            character(*) :: champ
            integer :: ioc
            character(len=8) :: mamax
            character(len=8) :: nomax
            integer :: ispmax
            character(len=8) :: cmpmax
            real(kind=8) :: valmax
            character(len=8) :: mamin
            character(len=8) :: nomin
            integer :: ispmin
            character(len=8) :: cmpmin
            real(kind=8) :: valmin
            character(len=8) :: maamax
            character(len=8) :: noamax
            integer :: isamax
            character(len=8) :: cmamax
            real(kind=8) :: vaamax
            character(len=8) :: maamin
            character(len=8) :: noamin
            integer :: isamin
            character(len=8) :: cmamin
            real(kind=8) :: vaamin
          end subroutine prexel
        end interface
