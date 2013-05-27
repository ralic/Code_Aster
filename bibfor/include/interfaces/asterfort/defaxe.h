        interface
          function defaxe(icoq,imod,z,long,nbm,tcoef)
            integer :: nbm
            integer :: icoq
            integer :: imod
            real(kind=8) :: z
            real(kind=8) :: long
            real(kind=8) :: tcoef(10,nbm)
            real(kind=8) :: defaxe
          end function defaxe
        end interface
