        interface
          subroutine lceobb(intmax,toler,epsm,deps,bm,dm,lambda,mu,&
     &alpha,ecrob,ecrod,rk,rk1,rk2,b,d,mult,elas,dbloq,iret)
            integer :: intmax
            real(kind=8) :: toler
            real(kind=8) :: epsm(6)
            real(kind=8) :: deps(6)
            real(kind=8) :: bm(6)
            real(kind=8) :: dm
            real(kind=8) :: lambda
            real(kind=8) :: mu
            real(kind=8) :: alpha
            real(kind=8) :: ecrob
            real(kind=8) :: ecrod
            real(kind=8) :: rk
            real(kind=8) :: rk1
            real(kind=8) :: rk2
            real(kind=8) :: b(6)
            real(kind=8) :: d
            real(kind=8) :: mult
            logical :: elas
            logical :: dbloq
            integer :: iret
          end subroutine lceobb
        end interface
