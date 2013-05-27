        interface
          subroutine inical(nbin,lpain,lchin,nbout,lpaout,lchout)
            integer :: nbout
            integer :: nbin
            character(len=8) :: lpain(nbin)
            character(len=19) :: lchin(nbin)
            character(len=8) :: lpaout(nbout)
            character(len=19) :: lchout(nbout)
          end subroutine inical
        end interface
