        interface
          subroutine lcloca(coeft,e,nu,nmat,nbcomm,nphas,sigi,vini,&
     &iphas,granb,loca,sigg)
            integer :: nmat
            real(kind=8) :: coeft(nmat)
            real(kind=8) :: e
            real(kind=8) :: nu
            integer :: nbcomm(nmat,3)
            integer :: nphas
            real(kind=8) :: sigi(6)
            real(kind=8) :: vini(*)
            integer :: iphas
            real(kind=8) :: granb(6)
            character(len=16) :: loca
            real(kind=8) :: sigg(6)
          end subroutine lcloca
        end interface
