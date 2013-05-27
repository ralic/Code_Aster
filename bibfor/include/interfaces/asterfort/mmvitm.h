        interface
          subroutine mmvitm(nbdm,ndim,nne,nnm,ffe,ffm,jvitm,jaccm,&
     &jvitp,vitme,vitmm,vitpe,vitpm,accme,accmm)
            integer :: nbdm
            integer :: ndim
            integer :: nne
            integer :: nnm
            real(kind=8) :: ffe(9)
            real(kind=8) :: ffm(9)
            integer :: jvitm
            integer :: jaccm
            integer :: jvitp
            real(kind=8) :: vitme(3)
            real(kind=8) :: vitmm(3)
            real(kind=8) :: vitpe(3)
            real(kind=8) :: vitpm(3)
            real(kind=8) :: accme(3)
            real(kind=8) :: accmm(3)
          end subroutine mmvitm
        end interface
