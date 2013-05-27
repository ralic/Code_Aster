        interface
          subroutine mudirx(nbsom,geom,idim,al1,al2,axe,ang)
            integer :: idim
            integer :: nbsom
            real(kind=8) :: geom(idim,nbsom)
            real(kind=8) :: al1
            real(kind=8) :: al2
            real(kind=8) :: axe(3,3)
            real(kind=8) :: ang(2)
          end subroutine mudirx
        end interface
