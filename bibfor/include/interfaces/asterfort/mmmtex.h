        interface
          subroutine mmmtex(ndexfr,ndim,nnl,nne,nnm,nbcps,matrff,&
     &matrfe,matrfm,matref,matrmf)
            integer :: ndexfr
            integer :: ndim
            integer :: nnl
            integer :: nne
            integer :: nnm
            integer :: nbcps
            real(kind=8) :: matrff(18,18)
            real(kind=8) :: matrfe(18,27)
            real(kind=8) :: matrfm(18,27)
            real(kind=8) :: matref(27,18)
            real(kind=8) :: matrmf(27,18)
          end subroutine mmmtex
        end interface
