        interface
          subroutine mpippv(optmpi,typsca,nbv,vi,vi4,vr,nudest,numess)
            character(*) :: optmpi
            character(*) :: typsca
            integer :: nbv
            integer :: vi(*)
            integer(kind=4) :: vi4(*)
            real(kind=8) :: vr(*)
            integer :: nudest
            integer :: numess
          end subroutine mpippv
        end interface
