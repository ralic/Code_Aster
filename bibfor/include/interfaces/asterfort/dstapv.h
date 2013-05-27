        interface
          subroutine dstapv(nbpt,d,t,dmin,dmax,dmoy,detyp,drms,sd,sde,&
     &sd2)
            integer :: nbpt
            real(kind=8) :: d(*)
            real(kind=8) :: t(*)
            real(kind=8) :: dmin
            real(kind=8) :: dmax
            real(kind=8) :: dmoy
            real(kind=8) :: detyp
            real(kind=8) :: drms
            real(kind=8) :: sd
            real(kind=8) :: sde
            real(kind=8) :: sd2
          end subroutine dstapv
        end interface
