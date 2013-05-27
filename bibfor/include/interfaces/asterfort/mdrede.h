        interface
          subroutine mdrede(numddl,nbrede,nbmode,bmodal,neq,dplred,&
     &parred,fonred,ier)
            integer :: neq
            integer :: nbmode
            integer :: nbrede
            character(len=14) :: numddl
            real(kind=8) :: bmodal(neq,*)
            real(kind=8) :: dplred(nbrede,nbmode,*)
            real(kind=8) :: parred(nbrede,*)
            character(len=8) :: fonred(nbrede,*)
            integer :: ier
          end subroutine mdrede
        end interface
