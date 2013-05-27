        interface
          subroutine brendo(sige6,bt6,sut,bc1,suc,local,t33,n33,lct,bw&
     &,pw,bch,pch,delta,lcc,mt,mc,siget6,sigec6,nu,dt66,dc0,sut6,suc1,&
     &siga6,dt6)
            real(kind=8) :: sige6(6)
            real(kind=8) :: bt6(6)
            real(kind=8) :: sut
            real(kind=8) :: bc1
            real(kind=8) :: suc
            logical :: local
            real(kind=8) :: t33(3,3)
            real(kind=8) :: n33(3,3)
            real(kind=8) :: lct
            real(kind=8) :: bw
            real(kind=8) :: pw
            real(kind=8) :: bch
            real(kind=8) :: pch
            real(kind=8) :: delta
            real(kind=8) :: lcc
            real(kind=8) :: mt
            real(kind=8) :: mc
            real(kind=8) :: siget6(6)
            real(kind=8) :: sigec6(6)
            real(kind=8) :: nu
            real(kind=8) :: dt66(6,6)
            real(kind=8) :: dc0
            real(kind=8) :: sut6(6)
            real(kind=8) :: suc1
            real(kind=8) :: siga6(6)
            real(kind=8) :: dt6(6)
          end subroutine brendo
        end interface
