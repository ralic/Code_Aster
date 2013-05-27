        interface
          subroutine defapp(ma,geomi,alpha,depla,base,geomf)
            character(len=8) :: ma
            character(len=19) :: geomi
            real(kind=8) :: alpha
            character(len=8) :: depla
            character(len=1) :: base
            character(len=19) :: geomf
          end subroutine defapp
        end interface
