        interface
          subroutine mmmte2(ndim,nnl,nne,nnm,nbcpf,ndexcl,matrff,&
     &matrfe,matrfm,matref,matrmf)
            integer :: ndim
            integer :: nnl
            integer :: nne
            integer :: nnm
            integer :: nbcpf
            integer :: ndexcl(10)
            real(kind=8) :: matrff(18,18)
            real(kind=8) :: matrfe(18,27)
            real(kind=8) :: matrfm(18,27)
            real(kind=8) :: matref(27,18)
            real(kind=8) :: matrmf(27,18)
          end subroutine mmmte2
        end interface
