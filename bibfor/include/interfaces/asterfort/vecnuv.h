        interface
          subroutine vecnuv(ipre,ider,gamma,phinit,dphi,n,k,dim,vectn,&
     &vectu,vectv)
            integer :: dim
            integer :: ipre
            integer :: ider
            real(kind=8) :: gamma
            real(kind=8) :: phinit
            real(kind=8) :: dphi
            integer :: n
            integer :: k
            real(kind=8) :: vectn(dim)
            real(kind=8) :: vectu(dim)
            real(kind=8) :: vectv(dim)
          end subroutine vecnuv
        end interface
