        interface
          subroutine cntmat(lambda,deuxmu,lamf,deumuf,alf,alfmc,emp,&
     &efp,eps,vmp,vfp,tr2d,trot,treps,gmt,gmc,gf,da1,da2,ksi2d,qff,cof1,&
     &q2d,de33d1,de33d2,elas,elas1,elas2,coup,rigi,resi,option,dsidep,&
     &sig,cof2,dq2d)
            real(kind=8) :: lambda
            real(kind=8) :: deuxmu
            real(kind=8) :: lamf
            real(kind=8) :: deumuf
            real(kind=8) :: alf
            real(kind=8) :: alfmc
            real(kind=8) :: emp(2)
            real(kind=8) :: efp(2)
            real(kind=8) :: eps(2)
            real(kind=8) :: vmp(2,2)
            real(kind=8) :: vfp(2,2)
            real(kind=8) :: tr2d
            real(kind=8) :: trot
            real(kind=8) :: treps
            real(kind=8) :: gmt
            real(kind=8) :: gmc
            real(kind=8) :: gf
            real(kind=8) :: da1
            real(kind=8) :: da2
            real(kind=8) :: ksi2d
            real(kind=8) :: qff(2)
            real(kind=8) :: cof1(2)
            real(kind=8) :: q2d(2)
            real(kind=8) :: de33d1
            real(kind=8) :: de33d2
            logical :: elas
            logical :: elas1
            logical :: elas2
            logical :: coup
            logical :: rigi
            logical :: resi
            character(len=16) :: option
            real(kind=8) :: dsidep(6,6)
            real(kind=8) :: sig(6)
            real(kind=8) :: cof2(2)
            real(kind=8) :: dq2d(2)
          end subroutine cntmat
        end interface
