        interface
          subroutine chtpcn(chno1,tgeom,tailmi,tmin,epsi,base,chno2)
            character(*) :: chno1
            real(kind=8) :: tgeom(6)
            real(kind=8) :: tailmi
            real(kind=8) :: tmin
            real(kind=8) :: epsi
            character(*) :: base
            character(*) :: chno2
          end subroutine chtpcn
        end interface
