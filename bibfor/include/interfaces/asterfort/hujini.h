        interface
          subroutine hujini(mod,nmat,mater,intg,deps,nr,yd,nvi,vind,&
     &sigd,sigf,bnews,mtrac,dy,indi,iret)
            integer :: nvi
            integer :: nmat
            character(len=8) :: mod
            real(kind=8) :: mater(nmat,2)
            integer :: intg
            real(kind=8) :: deps(6)
            integer :: nr
            real(kind=8) :: yd(18)
            real(kind=8) :: vind(nvi)
            real(kind=8) :: sigd(6)
            real(kind=8) :: sigf(6)
            logical :: bnews(3)
            logical :: mtrac
            real(kind=8) :: dy(18)
            integer :: indi(7)
            integer :: iret
          end subroutine hujini
        end interface
