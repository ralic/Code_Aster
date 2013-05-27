        interface
          subroutine crirup(fami,imat,ndim,npg,lgpg,option,compor,sigp&
     &,vip,vim,instam,instap)
            integer :: lgpg
            integer :: npg
            integer :: ndim
            character(*) :: fami
            integer :: imat
            character(len=16) :: option
            character(len=16) :: compor(*)
            real(kind=8) :: sigp(2*ndim,npg)
            real(kind=8) :: vip(lgpg,npg)
            real(kind=8) :: vim(lgpg,npg)
            real(kind=8) :: instam
            real(kind=8) :: instap
          end subroutine crirup
        end interface
