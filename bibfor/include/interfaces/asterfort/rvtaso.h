        interface
          subroutine rvtaso(releve,nomcmp,nbcmp,nbco,nbsp,nomtab,iocc,&
     &ncheff,i1,ioc,isd)
            real(kind=8) :: releve(*)
            character(len=8) :: nomcmp(*)
            integer :: nbcmp
            integer :: nbco
            integer :: nbsp
            character(len=19) :: nomtab
            integer :: iocc
            character(len=16) :: ncheff
            integer :: i1
            integer :: ioc
            integer :: isd
          end subroutine rvtaso
        end interface
