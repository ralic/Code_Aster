        interface
          subroutine gldloc(lambda,deuxmu,deumuf,seuil,alf,alfmc,gmt,&
     &gmc,gf,cof1,vim,q2d,qff,tr2d,eps33,de33d1,de33d2,ksi2d,dksi1,dksi2&
     &,da1,da2,kdmax,told,codret,emp)
            real(kind=8) :: lambda
            real(kind=8) :: deuxmu
            real(kind=8) :: deumuf
            real(kind=8) :: seuil
            real(kind=8) :: alf
            real(kind=8) :: alfmc
            real(kind=8) :: gmt
            real(kind=8) :: gmc
            real(kind=8) :: gf
            real(kind=8) :: cof1(2)
            real(kind=8) :: vim(*)
            real(kind=8) :: q2d(2)
            real(kind=8) :: qff(2)
            real(kind=8) :: tr2d
            real(kind=8) :: eps33
            real(kind=8) :: de33d1
            real(kind=8) :: de33d2
            real(kind=8) :: ksi2d
            real(kind=8) :: dksi1
            real(kind=8) :: dksi2
            real(kind=8) :: da1
            real(kind=8) :: da2
            integer :: kdmax
            real(kind=8) :: told
            integer :: codret
            real(kind=8) :: emp(2)
          end subroutine gldloc
        end interface
