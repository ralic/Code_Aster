        interface
          subroutine hujact(mater,vind,vinf,vins,sigd,sigf,negmul,&
     &chgmec,indi)
            real(kind=8) :: mater(22,2)
            real(kind=8) :: vind(*)
            real(kind=8) :: vinf(*)
            real(kind=8) :: vins(50)
            real(kind=8) :: sigd(6)
            real(kind=8) :: sigf(6)
            logical :: negmul(8)
            logical :: chgmec
            integer :: indi(7)
          end subroutine hujact
        end interface
