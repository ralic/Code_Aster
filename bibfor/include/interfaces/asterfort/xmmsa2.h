        interface
          subroutine xmmsa2(ndim,ipgf,imate,saut,nd,tau1,tau2,cohes,&
     &job,rela,alpha,dsidep,sigma,pp,dnor,dtang,p,am)
            integer :: ndim
            integer :: ipgf
            integer :: imate
            real(kind=8) :: saut(3)
            real(kind=8) :: nd(3)
            real(kind=8) :: tau1(3)
            real(kind=8) :: tau2(3)
            real(kind=8) :: cohes(3)
            character(len=8) :: job
            real(kind=8) :: rela
            real(kind=8) :: alpha(3)
            real(kind=8) :: dsidep(6,6)
            real(kind=8) :: sigma(6)
            real(kind=8) :: pp(3,3)
            real(kind=8) :: dnor(3)
            real(kind=8) :: dtang(3)
            real(kind=8) :: p(3,3)
            real(kind=8) :: am(3)
          end subroutine xmmsa2
        end interface
