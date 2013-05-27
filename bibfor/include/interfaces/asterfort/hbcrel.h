        interface
          subroutine hbcrel(vp,gamma,dg,nbmat,materf,sigeqe,i1e,etap,&
     &parame,seuil)
            integer :: nbmat
            real(kind=8) :: vp(3)
            real(kind=8) :: gamma
            real(kind=8) :: dg
            real(kind=8) :: materf(nbmat,2)
            real(kind=8) :: sigeqe
            real(kind=8) :: i1e
            real(kind=8) :: etap
            real(kind=8) :: parame(4)
            real(kind=8) :: seuil
          end subroutine hbcrel
        end interface
