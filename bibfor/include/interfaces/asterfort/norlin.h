        interface
          subroutine norlin(typma,l,knumai,coor,dfonc,in,prec,a,b,c)
            character(len=3) :: typma
            integer :: l
            character(len=8) :: knumai
            real(kind=8) :: coor(3,*)
            real(kind=8) :: dfonc(*)
            integer :: in
            real(kind=8) :: prec
            real(kind=8) :: a
            real(kind=8) :: b
            real(kind=8) :: c
          end subroutine norlin
        end interface
