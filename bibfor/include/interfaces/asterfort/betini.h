        interface
          subroutine betini(materf,nmat,sig,sigeq,sigh,coefa,coefb,&
     &coefar,coefbr,coneco,conetr)
            integer :: nmat
            real(kind=8) :: materf(nmat,2)
            real(kind=8) :: sig(6)
            real(kind=8) :: sigeq
            real(kind=8) :: sigh
            real(kind=8) :: coefa(2,2)
            real(kind=8) :: coefb(2)
            real(kind=8) :: coefar(2,2)
            real(kind=8) :: coefbr(2)
            real(kind=8) :: coneco
            real(kind=8) :: conetr
          end subroutine betini
        end interface
