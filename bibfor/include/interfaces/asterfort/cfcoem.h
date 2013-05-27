        interface
          subroutine cfcoem(resoco,lctfd,lctf3d,posnoe,iliai,nbddlt,&
     &nbnom,posnsm,ddl,coef,cofx,cofy)
            character(len=24) :: resoco
            logical :: lctfd
            logical :: lctf3d
            integer :: posnoe
            integer :: iliai
            integer :: nbddlt
            integer :: nbnom
            integer :: posnsm(9)
            integer :: ddl(30)
            real(kind=8) :: coef(30)
            real(kind=8) :: cofx(30)
            real(kind=8) :: cofy(30)
          end subroutine cfcoem
        end interface
