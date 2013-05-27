        interface
          subroutine trprot(model,bamo,tgeom,imodg,iadx,iady,iadz,isst&
     &,iadrp,norm1,norm2,ndble,num,nu,ma,mate,moint,ilires,k,icor)
            character(len=2) :: model
            character(len=8) :: bamo
            real(kind=8) :: tgeom(6)
            integer :: imodg
            integer :: iadx
            integer :: iady
            integer :: iadz
            integer :: isst
            integer :: iadrp
            real(kind=8) :: norm1
            real(kind=8) :: norm2
            integer :: ndble
            character(len=14) :: num
            character(len=14) :: nu
            character(len=8) :: ma
            character(*) :: mate
            character(len=8) :: moint
            integer :: ilires
            integer :: k
            integer :: icor(2)
          end subroutine trprot
        end interface
