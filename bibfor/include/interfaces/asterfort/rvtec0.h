        interface
          subroutine rvtec0(t,co,sp,absc,x,cmp,nd,sdm,nbpoin,docu,&
     &nbcmp,padr,nomtab,ioc,iocc,xnovar,ncheff,i1,isd)
            real(kind=8) :: t(*)
            integer :: co(*)
            integer :: sp(*)
            real(kind=8) :: absc(*)
            real(kind=8) :: x(*)
            character(len=8) :: cmp(*)
            character(len=8) :: nd(*)
            character(len=24) :: sdm
            integer :: nbpoin
            character(len=4) :: docu
            integer :: nbcmp
            integer :: padr(*)
            character(len=19) :: nomtab
            integer :: ioc
            integer :: iocc
            character(len=24) :: xnovar
            character(len=16) :: ncheff
            integer :: i1
            integer :: isd
          end subroutine rvtec0
        end interface
