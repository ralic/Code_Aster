        interface
          subroutine mefist(melflu,ndim,som,alpha,ru,promas,provis,&
     &matma,numgrp,nuor,freq,masg,fact,facpar,vite,xint,yint,rint,z,phix&
     &,phiy,defm,itypg,zg,hg,dg,tg,cdg,cpg,rugg,base)
            character(len=19) :: melflu
            integer :: ndim(14)
            real(kind=8) :: som(9)
            real(kind=8) :: alpha
            real(kind=8) :: ru
            character(len=8) :: promas
            character(len=8) :: provis
            real(kind=8) :: matma(*)
            integer :: numgrp(*)
            integer :: nuor(*)
            real(kind=8) :: freq(*)
            real(kind=8) :: masg(*)
            real(kind=8) :: fact(*)
            real(kind=8) :: facpar(*)
            real(kind=8) :: vite(*)
            real(kind=8) :: xint(*)
            real(kind=8) :: yint(*)
            real(kind=8) :: rint(*)
            real(kind=8) :: z(*)
            real(kind=8) :: phix(*)
            real(kind=8) :: phiy(*)
            real(kind=8) :: defm(*)
            integer :: itypg(*)
            real(kind=8) :: zg(*)
            real(kind=8) :: hg(*)
            real(kind=8) :: dg(*)
            real(kind=8) :: tg(*)
            real(kind=8) :: cdg(*)
            real(kind=8) :: cpg(*)
            real(kind=8) :: rugg(*)
            character(len=8) :: base
          end subroutine mefist
        end interface
