        interface
          subroutine dsqdis(xyzl,caraq4,df,dci,an)
            real(kind=8) :: xyzl(3,*)
            real(kind=8) :: caraq4(*)
            real(kind=8) :: df(3,3)
            real(kind=8) :: dci(2,2)
            real(kind=8) :: an(4,12)
          end subroutine dsqdis
        end interface
