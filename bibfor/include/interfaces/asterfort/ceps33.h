        interface
          subroutine ceps33(lambda,deuxmu,alfmc,gmt,gmc,tr2d,da1,da2,&
     &eps33,de33d1,de33d2,ksi2d,dksi1,dksi2,cof1,q2d,emp,cof2,dq2d)
            real(kind=8) :: lambda
            real(kind=8) :: deuxmu
            real(kind=8) :: alfmc
            real(kind=8) :: gmt
            real(kind=8) :: gmc
            real(kind=8) :: tr2d
            real(kind=8) :: da1
            real(kind=8) :: da2
            real(kind=8) :: eps33
            real(kind=8) :: de33d1
            real(kind=8) :: de33d2
            real(kind=8) :: ksi2d
            real(kind=8) :: dksi1
            real(kind=8) :: dksi2
            real(kind=8) :: cof1(2)
            real(kind=8) :: q2d(2)
            real(kind=8) :: emp(2)
            real(kind=8) :: cof2(2)
            real(kind=8) :: dq2d(2)
          end subroutine ceps33
        end interface
