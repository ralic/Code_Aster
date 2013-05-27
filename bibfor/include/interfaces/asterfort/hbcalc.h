        interface
          subroutine hbcalc(fmoins,gamma,dg,nbmat,materf,i1e,sigeqe,vp&
     &,etap,vh,vg,parame,derive,incrg)
            integer :: nbmat
            real(kind=8) :: fmoins
            real(kind=8) :: gamma
            real(kind=8) :: dg
            real(kind=8) :: materf(nbmat,2)
            real(kind=8) :: i1e
            real(kind=8) :: sigeqe
            real(kind=8) :: vp(3)
            real(kind=8) :: etap
            real(kind=8) :: vh
            real(kind=8) :: vg
            real(kind=8) :: parame(4)
            real(kind=8) :: derive(5)
            real(kind=8) :: incrg
          end subroutine hbcalc
        end interface
