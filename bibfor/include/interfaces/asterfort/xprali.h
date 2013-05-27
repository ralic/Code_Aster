        interface
          subroutine xprali(p1,p2,vnele,nelcou,poifis,trifis,libre,vin&
     &)
            integer :: p1
            integer :: p2
            real(kind=8) :: vnele(3)
            integer :: nelcou
            character(len=19) :: poifis
            character(len=19) :: trifis
            logical :: libre
            real(kind=8) :: vin(3)
          end subroutine xprali
        end interface
