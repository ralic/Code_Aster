        interface
          subroutine gdstag(stoudy,kp,nno,ajacob,en,enprim,x0k,tetak,&
     &qim,qikm1,qik,x0pg,tetag,tetapg,rotm,rotkm1,rotk)
            real(kind=8) :: stoudy
            integer :: kp
            integer :: nno
            real(kind=8) :: ajacob
            real(kind=8) :: en(3,2)
            real(kind=8) :: enprim(3,2)
            real(kind=8) :: x0k(3,3)
            real(kind=8) :: tetak(3,3)
            real(kind=8) :: qim(3,3)
            real(kind=8) :: qikm1(3,3)
            real(kind=8) :: qik(3,3)
            real(kind=8) :: x0pg(3)
            real(kind=8) :: tetag(3)
            real(kind=8) :: tetapg(3)
            real(kind=8) :: rotm(3,3)
            real(kind=8) :: rotkm1(3,3)
            real(kind=8) :: rotk(3,3)
          end subroutine gdstag
        end interface
