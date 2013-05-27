        interface
          subroutine nm1dco(fami,kpg,ksp,option,imate,materi,e,sigm,&
     &epsm,deps,vim,sigp,vip,dsde,crildc,codret)
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            character(len=16) :: option
            integer :: imate
            character(*) :: materi
            real(kind=8) :: e
            real(kind=8) :: sigm
            real(kind=8) :: epsm
            real(kind=8) :: deps
            real(kind=8) :: vim(*)
            real(kind=8) :: sigp
            real(kind=8) :: vip(*)
            real(kind=8) :: dsde
            real(kind=8) :: crildc(*)
            integer :: codret
          end subroutine nm1dco
        end interface
