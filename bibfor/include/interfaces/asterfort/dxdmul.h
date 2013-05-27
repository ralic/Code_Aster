        interface
          subroutine dxdmul(lcalct,icou,iniv,t1ve,t2ve,h,d1i,d2i,x3i,&
     &epi)
            logical :: lcalct
            integer :: icou
            integer :: iniv
            real(kind=8) :: t1ve(3,3)
            real(kind=8) :: t2ve(2,2)
            real(kind=8) :: h(3,3)
            real(kind=8) :: d1i(2,2)
            real(kind=8) :: d2i(2,4)
            real(kind=8) :: x3i
            real(kind=8) :: epi
          end subroutine dxdmul
        end interface
