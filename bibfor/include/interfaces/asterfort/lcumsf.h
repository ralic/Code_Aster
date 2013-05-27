        interface
          subroutine lcumsf(sigi,sigf,nstrs,vari,nvari,cmat,nmat,isph,&
     &tdt,hini,hfin,varf)
            integer :: nstrs
            real(kind=8) :: sigi(nstrs)
            real(kind=8) :: sigf(nstrs)
            real(kind=8) :: vari(20)
            integer :: nvari
            real(kind=8) :: cmat(15)
            integer :: nmat
            integer :: isph
            real(kind=8) :: tdt
            real(kind=8) :: hini
            real(kind=8) :: hfin
            real(kind=8) :: varf(20)
          end subroutine lcumsf
        end interface
