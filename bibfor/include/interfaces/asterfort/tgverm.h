        interface
          subroutine tgverm(option,carcri,compor,nno1,nno2,nno3,geom,&
     &ndim,nddl,deplp,sdepl,vu,vg,vp,vectu,svect,ncont,contp,scont,nvari&
     &,varip,svari,matuu,smatr,matsym,epsilo,epsilp,epsilg,varia,iret)
            integer :: nddl
            character(len=16) :: option
            real(kind=8) :: carcri(*)
            character(len=16) :: compor(*)
            integer :: nno1
            integer :: nno2
            integer :: nno3
            real(kind=8) :: geom(*)
            integer :: ndim
            real(kind=8) :: deplp(*)
            real(kind=8) :: sdepl(*)
            integer :: vu(3,27)
            integer :: vg(27)
            integer :: vp(27)
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
            real(kind=8) :: epsilp
            real(kind=8) :: epsilg
            real(kind=8) :: varia(*)
            integer :: iret
          end subroutine tgverm
        end interface
