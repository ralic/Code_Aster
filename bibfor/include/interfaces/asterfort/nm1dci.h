        interface
          subroutine nm1dci(fami,kpg,ksp,imate,em,ep,sigm,deps,vim,&
     &option,materi,sigp,vip,dsde)
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            integer :: imate
            real(kind=8) :: em
            real(kind=8) :: ep
            real(kind=8) :: sigm
            real(kind=8) :: deps
            real(kind=8) :: vim(2)
            character(len=16) :: option
            character(*) :: materi
            real(kind=8) :: sigp
            real(kind=8) :: vip(2)
            real(kind=8) :: dsde
          end subroutine nm1dci
        end interface
