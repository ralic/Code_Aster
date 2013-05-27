        interface
          function lkbpri(val,vin,nbmat,mater,para,invar,s)
            integer :: nbmat
            integer :: val
            real(kind=8) :: vin(7)
            real(kind=8) :: mater(nbmat,2)
            real(kind=8) :: para(3)
            real(kind=8) :: invar
            real(kind=8) :: s(6)
            real(kind=8) :: lkbpri
          end function lkbpri
        end interface
