        interface
          subroutine mmmtas(nbdm,ndim,nnl,nne,nnm,nbcps,matrcc,matree,&
     &matrmm,matrem,matrme,matrce,matrcm,matrmc,matrec,matrff,matrfe,&
     &matrfm,matrmf,matref,mmat)
            integer :: nbdm
            integer :: ndim
            integer :: nnl
            integer :: nne
            integer :: nnm
            integer :: nbcps
            real(kind=8) :: matrcc(9,9)
            real(kind=8) :: matree(27,27)
            real(kind=8) :: matrmm(27,27)
            real(kind=8) :: matrem(27,27)
            real(kind=8) :: matrme(27,27)
            real(kind=8) :: matrce(9,27)
            real(kind=8) :: matrcm(9,27)
            real(kind=8) :: matrmc(27,9)
            real(kind=8) :: matrec(27,9)
            real(kind=8) :: matrff(18,18)
            real(kind=8) :: matrfe(18,27)
            real(kind=8) :: matrfm(18,27)
            real(kind=8) :: matrmf(27,18)
            real(kind=8) :: matref(27,18)
            real(kind=8) :: mmat(81,81)
          end subroutine mmmtas
        end interface
