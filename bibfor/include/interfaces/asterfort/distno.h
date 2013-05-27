        interface
          subroutine distno(xlocal,signe,typeob,xjeu,dist1,dist2,dnorm&
     &,cost,sint)
            real(kind=8) :: xlocal(6)
            real(kind=8) :: signe(*)
            character(len=8) :: typeob
            real(kind=8) :: xjeu
            real(kind=8) :: dist1
            real(kind=8) :: dist2
            real(kind=8) :: dnorm
            real(kind=8) :: cost
            real(kind=8) :: sint
          end subroutine distno
        end interface
