        interface
          subroutine padtma(coor1,coor2,nbnott,icoupl,dmin)
            real(kind=8) :: coor1(*)
            real(kind=8) :: coor2(*)
            integer :: nbnott(3)
            integer :: icoupl(*)
            real(kind=8) :: dmin
          end subroutine padtma
        end interface
