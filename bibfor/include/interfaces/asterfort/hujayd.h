        interface
          subroutine hujayd(nmat,mater,nvi,vind,vinf,nr,yd,bnews,mtrac&
     &)
            integer :: nr
            integer :: nvi
            integer :: nmat
            real(kind=8) :: mater(nmat,2)
            real(kind=8) :: vind(nvi)
            real(kind=8) :: vinf(nvi)
            real(kind=8) :: yd(nr)
            logical :: bnews(3)
            logical :: mtrac
          end subroutine hujayd
        end interface
