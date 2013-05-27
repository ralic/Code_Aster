        interface
          subroutine liscli(lischa,ichar,nomcha,nomfct,nbinfo,lisinz,&
     &ival)
            character(len=19) :: lischa
            integer :: ichar
            character(len=8) :: nomcha
            character(len=8) :: nomfct
            integer :: nbinfo
            character(*) :: lisinz(*)
            integer :: ival
          end subroutine liscli
        end interface
