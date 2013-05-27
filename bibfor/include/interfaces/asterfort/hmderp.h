        interface
          subroutine hmderp(yate,yavp,t,r,kh,pvp,pad,rho11,rho12,h11,&
     &h12,cliq,alpliq,dp11p1,dp11p2,dp11t,dp12p1,dp12p2,dp12t,dp21p1,&
     &dp21p2,dp21t,dp22p1,dp22p2,dp22t,dp1pp1,dp2pp1,dtpp1,dp1pp2,dp2pp2&
     &,dtpp2,dp1pt,dp2pt,dtpt)
            integer :: yate
            logical :: yavp
            real(kind=8) :: t
            real(kind=8) :: r
            real(kind=8) :: kh
            real(kind=8) :: pvp
            real(kind=8) :: pad
            real(kind=8) :: rho11
            real(kind=8) :: rho12
            real(kind=8) :: h11
            real(kind=8) :: h12
            real(kind=8) :: cliq
            real(kind=8) :: alpliq
            real(kind=8) :: dp11p1
            real(kind=8) :: dp11p2
            real(kind=8) :: dp11t
            real(kind=8) :: dp12p1
            real(kind=8) :: dp12p2
            real(kind=8) :: dp12t
            real(kind=8) :: dp21p1
            real(kind=8) :: dp21p2
            real(kind=8) :: dp21t
            real(kind=8) :: dp22p1
            real(kind=8) :: dp22p2
            real(kind=8) :: dp22t
            real(kind=8) :: dp1pp1(2)
            real(kind=8) :: dp2pp1(2)
            real(kind=8) :: dtpp1(2)
            real(kind=8) :: dp1pp2(2)
            real(kind=8) :: dp2pp2(2)
            real(kind=8) :: dtpp2(2)
            real(kind=8) :: dp1pt(2)
            real(kind=8) :: dp2pt(2)
            real(kind=8) :: dtpt(2)
          end subroutine hmderp
        end interface
