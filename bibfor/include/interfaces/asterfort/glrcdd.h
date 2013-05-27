        interface
          subroutine glrcdd(zimat,maxmp,minmp,matr,ep,surfgp,q,epst,&
     &deps,dsig,ecr,delas,dsidep,normm,normn,crit,codret)
            integer :: zimat
            real(kind=8) :: maxmp(*)
            real(kind=8) :: minmp(*)
            real(kind=8) :: matr(*)
            real(kind=8) :: ep
            real(kind=8) :: surfgp
            real(kind=8) :: q(2,2)
            real(kind=8) :: epst(*)
            real(kind=8) :: deps(*)
            real(kind=8) :: dsig(*)
            real(kind=8) :: ecr(*)
            real(kind=8) :: delas(6,*)
            real(kind=8) :: dsidep(6,*)
            real(kind=8) :: normm
            real(kind=8) :: normn
            real(kind=8) :: crit(*)
            integer :: codret
          end subroutine glrcdd
        end interface
