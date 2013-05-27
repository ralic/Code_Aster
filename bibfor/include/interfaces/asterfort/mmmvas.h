        interface
          subroutine mmmvas(ndim,nne,nnm,nnl,nbdm,nbcps,vectee,vectmm,&
     &vectcc,vectff,vtmp)
            integer :: ndim
            integer :: nne
            integer :: nnm
            integer :: nnl
            integer :: nbdm
            integer :: nbcps
            real(kind=8) :: vectee(27)
            real(kind=8) :: vectmm(27)
            real(kind=8) :: vectcc(9)
            real(kind=8) :: vectff(18)
            real(kind=8) :: vtmp(81)
          end subroutine mmmvas
        end interface
