        interface
          subroutine e1e2nn(nno,dfde,dfdk,e1n,e2n,nxn,nyn,nzn,normn,&
     &j1n,j2n,san,can)
            integer :: nno
            real(kind=8) :: dfde(9,9)
            real(kind=8) :: dfdk(9,9)
            real(kind=8) :: e1n(3,9)
            real(kind=8) :: e2n(3,9)
            real(kind=8) :: nxn(9)
            real(kind=8) :: nyn(9)
            real(kind=8) :: nzn(9)
            real(kind=8) :: normn(3,9)
            real(kind=8) :: j1n(9)
            real(kind=8) :: j2n(9)
            real(kind=8) :: san(9)
            real(kind=8) :: can(9)
          end subroutine e1e2nn
        end interface
