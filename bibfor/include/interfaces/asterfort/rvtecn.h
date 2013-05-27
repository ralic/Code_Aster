        interface
          subroutine rvtecn(releve,absc,itcopt,itsppt,coor,nomcmp,&
     &nomnoe,nbcmp,nbpoin,docu,nomtab,iocc,xnovar,ncheff,i1,ioc,isd)
            real(kind=8) :: releve(*)
            real(kind=8) :: absc(*)
            integer :: itcopt(*)
            integer :: itsppt(*)
            real(kind=8) :: coor(*)
            character(len=8) :: nomcmp(*)
            character(len=8) :: nomnoe(*)
            integer :: nbcmp
            integer :: nbpoin
            character(len=4) :: docu
            character(len=19) :: nomtab
            integer :: iocc
            character(len=24) :: xnovar
            character(len=16) :: ncheff
            integer :: i1
            integer :: ioc
            integer :: isd
          end subroutine rvtecn
        end interface
