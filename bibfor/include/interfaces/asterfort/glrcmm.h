        interface
          subroutine glrcmm(zimat,matr,ep,surfgp,p,epst,deps,dsig,ecr,&
     &delas,dsidep,crit,codret)
            integer :: zimat
            real(kind=8) :: matr(*)
            real(kind=8) :: ep
            real(kind=8) :: surfgp
            real(kind=8) :: p(3,3)
            real(kind=8) :: epst(*)
            real(kind=8) :: deps(*)
            real(kind=8) :: dsig(*)
            real(kind=8) :: ecr(*)
            real(kind=8) :: delas(6,*)
            real(kind=8) :: dsidep(6,*)
            real(kind=8) :: crit(*)
            integer :: codret
          end subroutine glrcmm
        end interface
