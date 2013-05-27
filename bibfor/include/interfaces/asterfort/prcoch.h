        interface
          subroutine prcoch(noche8,nochs8,nocmp,ktype,itopo,ngroup,&
     &group)
            integer :: ngroup
            character(len=8) :: noche8
            character(len=8) :: nochs8
            character(len=8) :: nocmp
            character(len=8) :: ktype
            integer :: itopo
            character(len=8) :: group(ngroup)
          end subroutine prcoch
        end interface
