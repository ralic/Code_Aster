        interface
          subroutine vdrep2(alpha,beta,zilzi,zrlzr,matevn,matevg)
            real(kind=8) :: alpha
            real(kind=8) :: beta
            integer :: zilzi(*)
            real(kind=8) :: zrlzr(*)
            real(kind=8) :: matevn(2,2,1)
            real(kind=8) :: matevg(2,2,1)
          end subroutine vdrep2
        end interface
