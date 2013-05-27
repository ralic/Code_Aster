        interface
          subroutine mdfflu(dnorm,vnorm,anorm,vitloc,accloc,cost,sint,&
     &coefa,coefb,coefc,coefd,ffluid,flocal)
            real(kind=8) :: dnorm
            real(kind=8) :: vnorm
            real(kind=8) :: anorm
            real(kind=8) :: vitloc(3)
            real(kind=8) :: accloc(3)
            real(kind=8) :: cost
            real(kind=8) :: sint
            real(kind=8) :: coefa
            real(kind=8) :: coefb
            real(kind=8) :: coefc
            real(kind=8) :: coefd
            real(kind=8) :: ffluid
            real(kind=8) :: flocal(3)
          end subroutine mdfflu
        end interface
