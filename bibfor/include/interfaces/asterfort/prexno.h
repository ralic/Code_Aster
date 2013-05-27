        interface
          subroutine prexno(champ,ioc,nomax,cmpmax,valmax,nomin,cmpmin&
     &,valmin,noamax,cmamax,vaamax,noamin,cmamin,vaamin)
            character(*) :: champ
            integer :: ioc
            character(len=8) :: nomax
            character(len=8) :: cmpmax
            real(kind=8) :: valmax
            character(len=8) :: nomin
            character(len=8) :: cmpmin
            real(kind=8) :: valmin
            character(len=8) :: noamax
            character(len=8) :: cmamax
            real(kind=8) :: vaamax
            character(len=8) :: noamin
            character(len=8) :: cmamin
            real(kind=8) :: vaamin
          end subroutine prexno
        end interface
