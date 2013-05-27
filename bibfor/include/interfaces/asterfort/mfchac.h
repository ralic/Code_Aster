        interface
          subroutine mfchac(fid,cha,nomamd,type,comp,unit,ncomp,cret)
            integer :: fid
            character(*) :: cha
            character(*) :: nomamd
            integer :: type
            character(*) :: comp(*)
            character(*) :: unit(*)
            integer :: ncomp
            integer :: cret
          end subroutine mfchac
        end interface
