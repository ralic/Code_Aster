        interface
          subroutine nmchoi(phase,sddyna,numins,fonact,metpre,metcor,&
     &reasma,lcamor,optrig,lcrigi,larigi,lcfint)
            character(len=10) :: phase
            character(len=19) :: sddyna
            integer :: numins
            integer :: fonact(*)
            character(len=16) :: metpre
            character(len=16) :: metcor
            logical :: reasma
            logical :: lcamor
            character(len=16) :: optrig
            logical :: lcrigi
            logical :: larigi
            logical :: lcfint
          end subroutine nmchoi
        end interface
