        interface
          subroutine lkoptg(val,dum,dt,nbmat,mater,invar,s,iel,sel,&
     &ucrpm,ucrvm,ucriv,seuilv,vinm,de,depsv,dside,retcom)
            integer :: nbmat
            integer :: val
            integer :: dum
            real(kind=8) :: dt
            real(kind=8) :: mater(nbmat,2)
            real(kind=8) :: invar
            real(kind=8) :: s(6)
            real(kind=8) :: iel
            real(kind=8) :: sel(6)
            real(kind=8) :: ucrpm
            real(kind=8) :: ucrvm
            real(kind=8) :: ucriv
            real(kind=8) :: seuilv
            real(kind=8) :: vinm(7)
            real(kind=8) :: de(6,6)
            real(kind=8) :: depsv(6)
            real(kind=8) :: dside(6,6)
            integer :: retcom
          end subroutine lkoptg
        end interface
