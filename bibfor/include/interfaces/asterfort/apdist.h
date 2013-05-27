        interface
          subroutine apdist(elrefe,coorma,nbno,ksi1,ksi2,coorpt,dist,&
     &vecpm)
            character(len=8) :: elrefe
            real(kind=8) :: coorma(27)
            integer :: nbno
            real(kind=8) :: ksi1
            real(kind=8) :: ksi2
            real(kind=8) :: coorpt(3)
            real(kind=8) :: dist
            real(kind=8) :: vecpm(3)
          end subroutine apdist
        end interface
