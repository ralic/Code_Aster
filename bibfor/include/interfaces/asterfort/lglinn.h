        interface
          subroutine lglinn(nbmat,mater,parame,derive,ge,ie,q,vecn,f0,&
     &delta,devg,devgii,traceg,dy)
            integer :: nbmat
            real(kind=8) :: mater(nbmat,2)
            real(kind=8) :: parame(5)
            real(kind=8) :: derive(4)
            real(kind=8) :: ge
            real(kind=8) :: ie
            real(kind=8) :: q(6)
            real(kind=8) :: vecn(6)
            real(kind=8) :: f0
            real(kind=8) :: delta
            real(kind=8) :: devg(6)
            real(kind=8) :: devgii
            real(kind=8) :: traceg
            real(kind=8) :: dy(10)
          end subroutine lglinn
        end interface
