        interface
          subroutine ermeb2(ino,iref1,iref2,ivois,igeom,isig,typema,&
     &nbcmp,inst,nx,ny,tx,ty,sig11,sig22,sig12,chx,chy)
            integer :: ino
            integer :: iref1
            integer :: iref2
            integer :: ivois
            integer :: igeom
            integer :: isig
            character(len=8) :: typema
            integer :: nbcmp
            real(kind=8) :: inst
            real(kind=8) :: nx(3)
            real(kind=8) :: ny(3)
            real(kind=8) :: tx(3)
            real(kind=8) :: ty(3)
            real(kind=8) :: sig11(3)
            real(kind=8) :: sig22(3)
            real(kind=8) :: sig12(3)
            real(kind=8) :: chx(3)
            real(kind=8) :: chy(3)
          end subroutine ermeb2
        end interface
