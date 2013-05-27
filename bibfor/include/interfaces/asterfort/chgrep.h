        interface
          subroutine chgrep(type,pgl1,pgl2,matl,matg)
            character(len=2) :: type
            real(kind=8) :: pgl1(3,3)
            real(kind=8) :: pgl2(3,3)
            real(kind=8) :: matl(*)
            real(kind=8) :: matg(*)
          end subroutine chgrep
        end interface
