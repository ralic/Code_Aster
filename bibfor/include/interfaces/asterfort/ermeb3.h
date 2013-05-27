        interface
          subroutine ermeb3(noe,ifa,tymvol,nnof,iref1,iref2,ivois,&
     &igeom,isig,nbcmp,inst,nx,ny,nz,sig11,sig22,sig33,sig12,sig13,sig23&
     &,chx,chy,chz)
            integer :: noe(9,6,4)
            integer :: ifa
            integer :: tymvol
            integer :: nnof
            integer :: iref1
            integer :: iref2
            integer :: ivois
            integer :: igeom
            integer :: isig
            integer :: nbcmp
            real(kind=8) :: inst
            real(kind=8) :: nx(9)
            real(kind=8) :: ny(9)
            real(kind=8) :: nz(9)
            real(kind=8) :: sig11(9)
            real(kind=8) :: sig22(9)
            real(kind=8) :: sig33(9)
            real(kind=8) :: sig12(9)
            real(kind=8) :: sig13(9)
            real(kind=8) :: sig23(9)
            real(kind=8) :: chx(9)
            real(kind=8) :: chy(9)
            real(kind=8) :: chz(9)
          end subroutine ermeb3
        end interface
