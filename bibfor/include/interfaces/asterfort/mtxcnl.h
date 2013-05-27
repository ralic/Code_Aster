        interface
          subroutine mtxcnl(cumul,typcst,const,typmat,lmat,typres,lres&
     &,neq)
            character(*) :: cumul
            character(*) :: typcst
            real(kind=8) :: const(2)
            character(len=1) :: typmat
            integer :: lmat
            character(len=1) :: typres
            integer :: lres
            integer :: neq
          end subroutine mtxcnl
        end interface
