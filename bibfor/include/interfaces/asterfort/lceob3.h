        interface
          subroutine lceob3(intmax,tole,eps,bm,dm,lambda,mu,alpha,&
     &ecrob,ecrod,seuil,bdim,b,d,mult,elas,dbloq,iret)
            integer :: intmax
            real(kind=8) :: tole
            real(kind=8) :: eps(6)
            real(kind=8) :: bm(6)
            real(kind=8) :: dm
            real(kind=8) :: lambda
            real(kind=8) :: mu
            real(kind=8) :: alpha
            real(kind=8) :: ecrob
            real(kind=8) :: ecrod
            real(kind=8) :: seuil
            integer :: bdim
            real(kind=8) :: b(6)
            real(kind=8) :: d
            real(kind=8) :: mult
            logical :: elas
            logical :: dbloq
            integer :: iret
          end subroutine lceob3
        end interface
