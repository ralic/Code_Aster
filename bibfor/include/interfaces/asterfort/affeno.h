        interface
          subroutine affeno(ioc,ino,nocmp,nbcmp,ncmpgd,ncmpmx,val,kval&
     &,desc,valglo,kvalgl,type,nec)
            integer :: ioc
            integer :: ino
            character(len=8) :: nocmp(*)
            integer :: nbcmp
            character(len=8) :: ncmpgd(*)
            integer :: ncmpmx
            real(kind=8) :: val(*)
            character(len=8) :: kval(*)
            integer :: desc(*)
            real(kind=8) :: valglo(*)
            character(len=8) :: kvalgl(*)
            character(*) :: type
            integer :: nec
          end subroutine affeno
        end interface
