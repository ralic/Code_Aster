        interface
          subroutine vpqzla(typeqz,qrn,iqrn,lqrn,qrar,qrai,qrba,qrvl,&
     &lvec,kqrn,lvalpr,nconv,omecor,ktyp,kqrnr,neqact,ilscal,irscal,&
     &optiof,typres,omemin,omemax,omeshi,ddlexc,nfreq,lmasse,lraide,&
     &lamor,numedd,sigma,icscal,ivscal,iiscal,ibscal,flage)
            character(len=16) :: typeqz
            integer :: qrn
            integer :: iqrn
            integer :: lqrn
            integer :: qrar
            integer :: qrai
            integer :: qrba
            integer :: qrvl
            integer :: lvec
            integer :: kqrn
            integer :: lvalpr
            integer :: nconv
            real(kind=8) :: omecor
            character(len=1) :: ktyp
            integer :: kqrnr
            integer :: neqact
            integer :: ilscal
            integer :: irscal
            character(len=16) :: optiof
            character(len=16) :: typres
            real(kind=8) :: omemin
            real(kind=8) :: omemax
            real(kind=8) :: omeshi
            integer :: ddlexc(*)
            integer :: nfreq
            integer :: lmasse
            integer :: lraide
            integer :: lamor
            character(len=19) :: numedd
            complex(kind=8) :: sigma
            integer :: icscal
            integer :: ivscal
            integer :: iiscal
            integer :: ibscal
            logical :: flage
          end subroutine vpqzla
        end interface
