        interface
          subroutine lcafyd(comp,materd,materf,nbcomm,cpmono,nmat,mod,&
     &nvi,vind,vinf,sigd,nr,yd,bnews,mtrac)
            integer :: nvi
            integer :: nmat
            character(len=16) :: comp(*)
            real(kind=8) :: materd(nmat,2)
            real(kind=8) :: materf(nmat,2)
            integer :: nbcomm(nmat,3)
            character(len=24) :: cpmono(5*nmat+1)
            character(len=8) :: mod
            real(kind=8) :: vind(*)
            real(kind=8) :: vinf(nvi)
            real(kind=8) :: sigd(6)
            integer :: nr
            real(kind=8) :: yd(*)
            logical :: bnews(3)
            logical :: mtrac
          end subroutine lcafyd
        end interface
