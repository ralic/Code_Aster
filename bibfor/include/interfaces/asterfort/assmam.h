        interface
          subroutine assmam(base,matas,nbmat,tlimat,licoef,nu,motcle,&
     &itysca)
            character(*) :: base
            character(*) :: matas
            integer :: nbmat
            character(*) :: tlimat(*)
            real(kind=8) :: licoef(*)
            character(*) :: nu
            character(len=4) :: motcle
            integer :: itysca
          end subroutine assmam
        end interface
