        interface
          subroutine xprpls(dnomo,dcnsln,dcnslt,nomo,noma,cnsln,cnslt,&
     &grln,grlt,corres,ndim,ndomp,edomg)
            character(len=8) :: dnomo
            character(len=19) :: dcnsln
            character(len=19) :: dcnslt
            character(len=8) :: nomo
            character(len=8) :: noma
            character(len=19) :: cnsln
            character(len=19) :: cnslt
            character(len=19) :: grln
            character(len=19) :: grlt
            character(len=16) :: corres
            integer :: ndim
            character(len=19) :: ndomp
            character(len=19) :: edomg
          end subroutine xprpls
        end interface
