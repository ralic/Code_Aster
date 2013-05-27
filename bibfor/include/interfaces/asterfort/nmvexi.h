        interface
          subroutine nmvexi(sigi,grj2,dj2ds,nb,mate,nmat,xhi,dxhids)
            integer :: nmat
            integer :: nb
            real(kind=8) :: sigi(nb)
            real(kind=8) :: grj2
            real(kind=8) :: dj2ds(nb)
            real(kind=8) :: mate(nmat,2)
            real(kind=8) :: xhi
            real(kind=8) :: dxhids(nb)
          end subroutine nmvexi
        end interface
