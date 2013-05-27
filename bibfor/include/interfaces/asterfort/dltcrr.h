        interface
          subroutine dltcrr(result,neq,nbordr,iarchi,texte,ifm,t0,&
     &lcrea,typres,masse,rigid,amort,dep0,vit0,acc0,fexte,famor,fliai,&
     &numedd,nume,nbtyar,typear)
            integer :: nbtyar
            integer :: neq
            character(len=8) :: result
            integer :: nbordr
            integer :: iarchi
            character(*) :: texte
            integer :: ifm
            real(kind=8) :: t0
            logical :: lcrea
            character(len=16) :: typres
            character(len=8) :: masse
            character(len=8) :: rigid
            character(len=8) :: amort
            real(kind=8) :: dep0(neq)
            real(kind=8) :: vit0(neq)
            real(kind=8) :: acc0(neq)
            real(kind=8) :: fexte(2*neq)
            real(kind=8) :: famor(2*neq)
            real(kind=8) :: fliai(2*neq)
            character(len=24) :: numedd
            integer :: nume
            character(len=16) :: typear(nbtyar)
          end subroutine dltcrr
        end interface
