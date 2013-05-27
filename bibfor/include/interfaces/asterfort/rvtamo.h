        interface
          subroutine rvtamo(t,nomcmp,nbcp,nbco,nbsp,nomtab,iocc,xnovar&
     &,ncheff,i1,ioc,isd)
            real(kind=8) :: t(*)
            character(*) :: nomcmp(*)
            integer :: nbcp
            integer :: nbco
            integer :: nbsp
            character(*) :: nomtab
            integer :: iocc
            character(len=24) :: xnovar
            character(len=16) :: ncheff
            integer :: i1
            integer :: ioc
            integer :: isd
          end subroutine rvtamo
        end interface
