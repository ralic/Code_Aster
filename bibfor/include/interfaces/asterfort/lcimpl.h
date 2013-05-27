        interface
          subroutine lcimpl(fami,kpg,ksp,imate,em,ep,sigm,tmoins,tplus&
     &,deps,vim,option,compor,sigp,vip,dsde)
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            integer :: imate
            real(kind=8) :: em
            real(kind=8) :: ep
            real(kind=8) :: sigm
            real(kind=8) :: tmoins
            real(kind=8) :: tplus
            real(kind=8) :: deps
            real(kind=8) :: vim(*)
            character(len=16) :: option
            character(len=16) :: compor(*)
            real(kind=8) :: sigp
            real(kind=8) :: vip(*)
            real(kind=8) :: dsde
          end subroutine lcimpl
        end interface
