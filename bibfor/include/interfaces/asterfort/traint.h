        interface
          subroutine traint(resgen,modgen,numlia,sst1,sst2,intf1,intf2&
     &,nbmod,nl,nc)
            character(len=8) :: resgen
            character(len=8) :: modgen
            integer :: numlia
            character(len=8) :: sst1
            character(len=8) :: sst2
            character(len=8) :: intf1
            character(len=8) :: intf2
            integer :: nbmod
            integer :: nl
            integer :: nc
          end subroutine traint
        end interface
