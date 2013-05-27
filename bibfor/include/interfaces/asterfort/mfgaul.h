        interface
          subroutine mfgaul(fid,tr1,tr2,tr3,nbt,k64,cret)
            integer :: fid
            real(kind=8) :: tr1(*)
            real(kind=8) :: tr2(*)
            real(kind=8) :: tr3(*)
            integer :: nbt
            character(len=64) :: k64
            integer :: cret
          end subroutine mfgaul
        end interface
