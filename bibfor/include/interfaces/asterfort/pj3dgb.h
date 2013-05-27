        interface
          subroutine pj3dgb(ino2,geom2,geom1,tetr4,ndec,btdi,btvr,btnb&
     &,btlc,btco,p1,q1,r1,p2,q2,r2)
            integer :: ino2
            real(kind=8) :: geom2(*)
            real(kind=8) :: geom1(*)
            integer :: tetr4(*)
            integer :: ndec
            integer :: btdi(*)
            real(kind=8) :: btvr(*)
            integer :: btnb(*)
            integer :: btlc(*)
            integer :: btco(*)
            integer :: p1
            integer :: q1
            integer :: r1
            integer :: p2
            integer :: q2
            integer :: r2
          end subroutine pj3dgb
        end interface
