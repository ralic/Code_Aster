        interface
          subroutine tgveri(option,carcri,compor,nno,geom,ndim,nddl,&
     &deplp,sdepl,vectu,svect,ncont,contp,scont,nvari,varip,svari,matuu,&
     &smatr,matsym,epsilo,varia,iret)
            character(len=16) :: option
            real(kind=8) :: carcri(*)
            character(len=16) :: compor(*)
            integer :: nno
            real(kind=8) :: geom(*)
            integer :: ndim
            integer :: nddl
            real(kind=8) :: deplp(*)
            real(kind=8) :: sdepl(*)
            real(kind=8) :: vectu(*)
            real(kind=8) :: svect(*)
            integer :: ncont
            real(kind=8) :: contp(*)
            real(kind=8) :: scont(*)
            integer :: nvari
            real(kind=8) :: varip(*)
            real(kind=8) :: svari(*)
            real(kind=8) :: matuu(*)
            real(kind=8) :: smatr(*)
            logical :: matsym
            real(kind=8) :: epsilo
            real(kind=8) :: varia(*)
            integer :: iret
          end subroutine tgveri
        end interface
