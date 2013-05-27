        interface
          subroutine lkcrip(invar,s,vin,nbmat,mater,ucrip,seuil)
            integer :: nbmat
            real(kind=8) :: invar
            real(kind=8) :: s(6)
            real(kind=8) :: vin(7)
            real(kind=8) :: mater(nbmat,2)
            real(kind=8) :: ucrip
            real(kind=8) :: seuil
          end subroutine lkcrip
        end interface
