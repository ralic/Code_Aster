        interface
          subroutine pj2dgb(ino2,geom2,geom1,tria3,btdi,btvr,btnb,btlc&
     &,btco,p1,q1,p2,q2)
            integer :: ino2
            real(kind=8) :: geom2(*)
            real(kind=8) :: geom1(*)
            integer :: tria3(*)
            integer :: btdi(*)
            real(kind=8) :: btvr(*)
            integer :: btnb(*)
            integer :: btlc(*)
            integer :: btco(*)
            integer :: p1
            integer :: q1
            integer :: p2
            integer :: q2
          end subroutine pj2dgb
        end interface
