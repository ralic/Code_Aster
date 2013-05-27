        interface
          subroutine permea(imate,hydr,phi,t,sat,ncon,cond)
            integer :: ncon
            integer :: imate
            character(len=16) :: hydr
            real(kind=8) :: phi
            real(kind=8) :: t
            real(kind=8) :: sat
            real(kind=8) :: cond(ncon)
          end subroutine permea
        end interface
