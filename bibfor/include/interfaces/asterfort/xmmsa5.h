        interface
          subroutine xmmsa5(ndim,ipgf,imate,saut,lamb,nd,tau1,tau2,&
     &cohes,job,rela,alpha,dsidep,delta,p,am,r)
            integer :: ndim
            integer :: ipgf
            integer :: imate
            real(kind=8) :: saut(3)
            real(kind=8) :: lamb(3)
            real(kind=8) :: nd(3)
            real(kind=8) :: tau1(3)
            real(kind=8) :: tau2(3)
            real(kind=8) :: cohes(3)
            character(len=8) :: job
            real(kind=8) :: rela
            real(kind=8) :: alpha(3)
            real(kind=8) :: dsidep(6,6)
            real(kind=8) :: delta(6)
            real(kind=8) :: p(3,3)
            real(kind=8) :: am(3)
            real(kind=8) :: r
          end subroutine xmmsa5
        end interface
