        interface
          subroutine nmmalu(nno,axi,r,vff,dfdi,lij)
            integer :: nno
            logical :: axi
            real(kind=8) :: r
            real(kind=8) :: vff(nno)
            real(kind=8) :: dfdi(nno,4)
            integer :: lij(3,3)
          end subroutine nmmalu
        end interface
