        interface
          subroutine mmstaf(noma,ndim,chdepd,coefaf,lpenaf,nummae,&
     &aliase,nne,nummam,ksipc1,ksipc2,ksipr1,ksipr2,mlagf1,mlagf2,tau1,&
     &tau2,norm,indco,indfr,rese)
            character(len=8) :: noma
            integer :: ndim
            character(len=19) :: chdepd
            real(kind=8) :: coefaf
            logical :: lpenaf
            integer :: nummae
            character(len=8) :: aliase
            integer :: nne
            integer :: nummam
            real(kind=8) :: ksipc1
            real(kind=8) :: ksipc2
            real(kind=8) :: ksipr1
            real(kind=8) :: ksipr2
            real(kind=8) :: mlagf1(9)
            real(kind=8) :: mlagf2(9)
            real(kind=8) :: tau1(3)
            real(kind=8) :: tau2(3)
            real(kind=8) :: norm(3)
            integer :: indco
            integer :: indfr
            real(kind=8) :: rese(3)
          end subroutine mmstaf
        end interface
