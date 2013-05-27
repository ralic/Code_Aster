        interface
          subroutine mltclm(nb,n,p,front,adper,t1,ad,eps,ier,c)
            integer :: nb
            integer :: n
            integer :: p
            complex(kind=8) :: front(*)
            integer :: adper(*)
            complex(kind=8) :: t1(*)
            integer :: ad(*)
            real(kind=8) :: eps
            integer :: ier
            complex(kind=8) :: c(nb,nb,*)
          end subroutine mltclm
        end interface
