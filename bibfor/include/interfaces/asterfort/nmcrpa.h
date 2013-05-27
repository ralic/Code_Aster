        interface
          subroutine nmcrpa(motfaz,iocc,sdlist,base,nbinst,dtmin)
            character(*) :: motfaz
            integer :: iocc
            character(len=24) :: sdlist
            character(len=1) :: base
            integer :: nbinst
            real(kind=8) :: dtmin
          end subroutine nmcrpa
        end interface
