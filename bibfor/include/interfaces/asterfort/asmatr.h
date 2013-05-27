        interface
          subroutine asmatr(nbmat,tlimat,licoef,nu,solveu,infcha,cumul&
     &,base,itysca,mataz)
            integer :: nbmat
            character(*) :: tlimat(*)
            character(*) :: licoef
            character(*) :: nu
            character(*) :: solveu
            character(*) :: infcha
            character(len=4) :: cumul
            character(*) :: base
            integer :: itysca
            character(*) :: mataz
          end subroutine asmatr
        end interface
