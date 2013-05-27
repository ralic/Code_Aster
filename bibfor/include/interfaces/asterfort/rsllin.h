        interface
          subroutine rsllin(mod,nmat,materd,materf,matcst,deps,sigd,&
     &vind,sigf,theta)
            integer :: nmat
            character(len=8) :: mod
            real(kind=8) :: materd(nmat,2)
            real(kind=8) :: materf(nmat,2)
            character(len=3) :: matcst
            real(kind=8) :: deps(6)
            real(kind=8) :: sigd(6)
            real(kind=8) :: vind(*)
            real(kind=8) :: sigf(6)
            real(kind=8) :: theta
          end subroutine rsllin
        end interface
