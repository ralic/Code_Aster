        interface
          subroutine mdrevi(numddl,nbrevi,nbmode,bmodal,neq,dplrev,&
     &fonrev,ier)
            integer :: neq
            integer :: nbmode
            integer :: nbrevi
            character(len=14) :: numddl
            real(kind=8) :: bmodal(neq,*)
            real(kind=8) :: dplrev(nbrevi,nbmode,*)
            character(len=8) :: fonrev(nbrevi,*)
            integer :: ier
          end subroutine mdrevi
        end interface
