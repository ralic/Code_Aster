        interface
          subroutine flaln2(ltrans,na,nw,smin,ca,a,lda,d1,d2,b,ldb,wr,&
     &wi,x,ldx,scale,xnorm,info)
            integer :: ldx
            integer :: ldb
            integer :: lda
            logical :: ltrans
            integer :: na
            integer :: nw
            real(kind=8) :: smin
            real(kind=8) :: ca
            real(kind=8) :: a(lda,*)
            real(kind=8) :: d1
            real(kind=8) :: d2
            real(kind=8) :: b(ldb,*)
            real(kind=8) :: wr
            real(kind=8) :: wi
            real(kind=8) :: x(ldx,*)
            real(kind=8) :: scale
            real(kind=8) :: xnorm
            integer :: info
          end subroutine flaln2
        end interface
