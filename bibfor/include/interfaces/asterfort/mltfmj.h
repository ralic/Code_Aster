        interface
          subroutine mltfmj(nb,n,p,front,frn,adper,trav,c)
            integer :: p
            integer :: nb
            integer :: n
            real(kind=8) :: front(*)
            real(kind=8) :: frn(*)
            integer :: adper(*)
            real(kind=8) :: trav(p,nb,*)
            real(kind=8) :: c(nb,nb,*)
          end subroutine mltfmj
        end interface
