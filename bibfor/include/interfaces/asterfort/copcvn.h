        interface
          subroutine copcvn(nb,vec1,vec2,indir,fact)
            integer :: nb
            real(kind=8) :: vec1(*)
            real(kind=8) :: vec2(nb)
            integer :: indir(nb)
            real(kind=8) :: fact
          end subroutine copcvn
        end interface
