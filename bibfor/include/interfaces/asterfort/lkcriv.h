        interface
          subroutine lkcriv(vintr,invar,s,vin,nbmat,mater,ucriv,seuil)
            integer :: nbmat
            real(kind=8) :: vintr
            real(kind=8) :: invar
            real(kind=8) :: s(6)
            real(kind=8) :: vin(7)
            real(kind=8) :: mater(nbmat,2)
            real(kind=8) :: ucriv
            real(kind=8) :: seuil
          end subroutine lkcriv
        end interface
