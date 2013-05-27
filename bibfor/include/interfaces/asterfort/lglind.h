        interface
          subroutine lglind(nbmat,mater,parame,ge,q,vecn,deps,devg,&
     &devgii,traceg,dy)
            integer :: nbmat
            real(kind=8) :: mater(nbmat,2)
            real(kind=8) :: parame(5)
            real(kind=8) :: ge
            real(kind=8) :: q(6)
            real(kind=8) :: vecn(6)
            real(kind=8) :: deps(6)
            real(kind=8) :: devg(6)
            real(kind=8) :: devgii
            real(kind=8) :: traceg
            real(kind=8) :: dy(10)
          end subroutine lglind
        end interface
