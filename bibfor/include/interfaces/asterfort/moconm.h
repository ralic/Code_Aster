        interface
          subroutine moconm(dir,sigb,siga,hh,nlit,om,rr,nufsup,nufinf,&
     &nufsd1,nufid1,nufsd2,nufid2,prec)
            integer :: nlit
            character(len=1) :: dir
            real(kind=8) :: sigb
            real(kind=8) :: siga(nlit)
            real(kind=8) :: hh
            real(kind=8) :: om(nlit)
            real(kind=8) :: rr(nlit)
            character(len=8) :: nufsup
            character(len=8) :: nufinf
            character(len=8) :: nufsd1
            character(len=8) :: nufid1
            character(len=8) :: nufsd2
            character(len=8) :: nufid2
            real(kind=8) :: prec
          end subroutine moconm
        end interface
