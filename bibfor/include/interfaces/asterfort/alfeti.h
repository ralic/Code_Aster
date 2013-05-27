        interface
          subroutine alfeti(opt,sdfeti,matas,chsecm,chsol,niter,epsi,&
     &criter,testco,nbreor,tyreor,preco,scalin,stogi,nbreoi,acma,acsm,&
     &reacre)
            character(len=24) :: opt
            character(len=19) :: sdfeti
            character(len=19) :: matas
            character(len=19) :: chsecm
            character(len=19) :: chsol
            integer :: niter
            real(kind=8) :: epsi
            character(*) :: criter
            real(kind=8) :: testco
            integer :: nbreor
            character(len=24) :: tyreor
            character(len=24) :: preco
            character(len=24) :: scalin
            character(len=24) :: stogi
            integer :: nbreoi
            character(len=24) :: acma
            character(len=24) :: acsm
            integer :: reacre
          end subroutine alfeti
        end interface
