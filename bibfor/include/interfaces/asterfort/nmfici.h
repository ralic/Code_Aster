        interface
          subroutine nmfici(nno,nddl,wref,vff,dfde,geom,poids,b)
            integer :: nddl
            integer :: nno
            real(kind=8) :: wref
            real(kind=8) :: vff(nno)
            real(kind=8) :: dfde(2,nno)
            real(kind=8) :: geom(3,nddl/3)
            real(kind=8) :: poids
            real(kind=8) :: b(3,3,nddl/3)
          end subroutine nmfici
        end interface
