        interface
          subroutine mmmcri(criter,noma,depmoi,depgeo,depplu,resoco,&
     &epsmax,cvgnoe,cvgval,mmconv)
            character(len=4) :: criter
            character(len=8) :: noma
            character(len=19) :: depmoi
            character(len=19) :: depgeo
            character(len=19) :: depplu
            character(len=24) :: resoco
            real(kind=8) :: epsmax
            character(len=16) :: cvgnoe
            real(kind=8) :: cvgval
            logical :: mmconv
          end subroutine mmmcri
        end interface
