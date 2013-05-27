        interface
          subroutine hujpot(mod,mater,vind,depsh,sigd,sige,etatf,&
     &rdctps,iret,aredec)
            character(len=8) :: mod
            real(kind=8) :: mater(22,2)
            real(kind=8) :: vind(*)
            real(kind=8) :: depsh(6)
            real(kind=8) :: sigd(6)
            real(kind=8) :: sige(6)
            character(len=7) :: etatf
            logical :: rdctps
            integer :: iret
            logical :: aredec
          end subroutine hujpot
        end interface
