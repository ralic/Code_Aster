        interface
          subroutine lcgldm(epsm,deps,vim,option,sig,vip,dsidep,lambda&
     &,deuxmu,lamf,deumuf,gmt,gmc,gf,seuil,alf,alfmc,crit,codret)
            real(kind=8) :: epsm(6)
            real(kind=8) :: deps(6)
            real(kind=8) :: vim(*)
            character(len=16) :: option
            real(kind=8) :: sig(6)
            real(kind=8) :: vip(*)
            real(kind=8) :: dsidep(6,6)
            real(kind=8) :: lambda
            real(kind=8) :: deuxmu
            real(kind=8) :: lamf
            real(kind=8) :: deumuf
            real(kind=8) :: gmt
            real(kind=8) :: gmc
            real(kind=8) :: gf
            real(kind=8) :: seuil
            real(kind=8) :: alf
            real(kind=8) :: alfmc
            real(kind=8) :: crit(*)
            integer :: codret
          end subroutine lcgldm
        end interface
