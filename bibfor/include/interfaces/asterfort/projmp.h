        interface
          subroutine projmp(np1,np2,nbm,nbnl,phii,accg,vitg,depg,acc,&
     &vit,dep)
            integer :: np2
            integer :: np1
            integer :: nbm
            integer :: nbnl
            real(kind=8) :: phii(np2,np1,3)
            real(kind=8) :: accg(*)
            real(kind=8) :: vitg(*)
            real(kind=8) :: depg(*)
            real(kind=8) :: acc(3,*)
            real(kind=8) :: vit(3,*)
            real(kind=8) :: dep(3,*)
          end subroutine projmp
        end interface
