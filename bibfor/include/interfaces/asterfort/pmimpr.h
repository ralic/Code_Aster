        interface
          subroutine pmimpr(ind,inst,indimp,fonimp,valimp,iter,eps,sig&
     &,vi,nbvari,r,ee,eini)
            integer :: nbvari
            integer :: ind
            real(kind=8) :: inst
            integer :: indimp(6)
            character(len=8) :: fonimp(6)
            real(kind=8) :: valimp(6)
            integer :: iter
            real(kind=8) :: eps(6)
            real(kind=8) :: sig(6)
            real(kind=8) :: vi(nbvari)
            real(kind=8) :: r(12)
            real(kind=8) :: ee
            real(kind=8) :: eini
          end subroutine pmimpr
        end interface
