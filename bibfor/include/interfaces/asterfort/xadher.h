        interface
          subroutine xadher(p,saut,lamb1,cstafr,cpenfr,algofr,vitang,&
     &pboul,kn,ptknp,ik,adher)
            real(kind=8) :: p(3,3)
            real(kind=8) :: saut(3)
            real(kind=8) :: lamb1(3)
            real(kind=8) :: cstafr
            real(kind=8) :: cpenfr
            integer :: algofr
            real(kind=8) :: vitang(3)
            real(kind=8) :: pboul(3)
            real(kind=8) :: kn(3,3)
            real(kind=8) :: ptknp(3,3)
            real(kind=8) :: ik(3,3)
            logical :: adher
          end subroutine xadher
        end interface
