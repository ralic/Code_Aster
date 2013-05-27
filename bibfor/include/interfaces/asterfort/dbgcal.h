        interface
          subroutine dbgcal(optioz,ifm,nbin,lpaiz,lchiz,nbout,lpaouz,&
     &lchouz)
            integer :: nbout
            integer :: nbin
            character(*) :: optioz
            integer :: ifm
            character(*) :: lpaiz(nbin)
            character(*) :: lchiz(nbin)
            character(*) :: lpaouz(nbout)
            character(*) :: lchouz(nbout)
          end subroutine dbgcal
        end interface
