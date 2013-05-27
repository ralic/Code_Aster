        interface
          subroutine lcdsbe(fami,ndim,typmod,imate,compor,epstm,depst,&
     &vim,option,sig,vip,dsidpt,proj)
            character(*) :: fami
            integer :: ndim
            character(len=8) :: typmod(2)
            integer :: imate
            character(len=16) :: compor(*)
            real(kind=8) :: epstm(12)
            real(kind=8) :: depst(12)
            real(kind=8) :: vim(2)
            character(len=16) :: option
            real(kind=8) :: sig(6)
            real(kind=8) :: vip(2)
            real(kind=8) :: dsidpt(6,6,2)
            real(kind=8) :: proj(6,6)
          end subroutine lcdsbe
        end interface
