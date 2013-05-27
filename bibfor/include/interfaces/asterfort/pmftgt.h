        interface
          subroutine pmftgt(nf,e,sim,sip,dep,mod)
            integer :: nf
            real(kind=8) :: e
            real(kind=8) :: sim(nf)
            real(kind=8) :: sip(nf)
            real(kind=8) :: dep(nf)
            real(kind=8) :: mod(nf)
          end subroutine pmftgt
        end interface
