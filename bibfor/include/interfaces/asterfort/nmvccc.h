        interface
          subroutine nmvccc(modele,nbin,nbout,lpain,lchin,lpaout,&
     &lchout,exitem,exihyd,exipto,exisec,exiepa,exipha,vecel)
            integer :: nbout
            integer :: nbin
            character(len=8) :: modele
            character(len=8) :: lpain(nbin)
            character(len=19) :: lchin(nbin)
            character(len=8) :: lpaout(nbout)
            character(len=19) :: lchout(nbout)
            logical :: exitem
            logical :: exihyd
            logical :: exipto
            logical :: exisec
            logical :: exiepa
            logical :: exipha
            character(len=19) :: vecel
          end subroutine nmvccc
        end interface
