        interface
          subroutine elagon(ndim,imate,crit,sat,biot,tm,tp,alpha,deps,&
     &e,nu,snetm,option,snetp,dsidep,p1,p2,dp1,dsidp1,dsidp2)
            integer :: ndim
            integer :: imate
            real(kind=8) :: crit(*)
            real(kind=8) :: sat
            real(kind=8) :: biot
            real(kind=8) :: tm
            real(kind=8) :: tp
            real(kind=8) :: alpha
            real(kind=8) :: deps(6)
            real(kind=8) :: e
            real(kind=8) :: nu
            real(kind=8) :: snetm(6)
            character(len=16) :: option
            real(kind=8) :: snetp(6)
            real(kind=8) :: dsidep(6,6)
            real(kind=8) :: p1
            real(kind=8) :: p2
            real(kind=8) :: dp1
            real(kind=8) :: dsidp1(6)
            real(kind=8) :: dsidp2(6)
          end subroutine elagon
        end interface
