        interface
          subroutine modint(ssami,raiint,nddlin,nbmod,shift,matmod,&
     &masse,raide,neq,coint,noddli,nnoint,vefreq,switch)
            character(len=19) :: ssami
            character(len=19) :: raiint
            integer :: nddlin
            integer :: nbmod
            real(kind=8) :: shift
            character(len=24) :: matmod
            character(len=19) :: masse
            character(len=19) :: raide
            integer :: neq
            character(len=24) :: coint
            character(len=24) :: noddli
            integer :: nnoint
            character(len=24) :: vefreq
            integer :: switch
          end subroutine modint
        end interface
