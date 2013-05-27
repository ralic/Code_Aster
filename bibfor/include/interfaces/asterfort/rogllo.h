        interface
          subroutine rogllo(nb1,nb2,vrg,blam,ctor,knn)
            integer :: nb1
            integer :: nb2
            real(kind=8) :: vrg(2601)
            real(kind=8) :: blam(9,3,3)
            real(kind=8) :: ctor
            real(kind=8) :: knn
          end subroutine rogllo
        end interface
