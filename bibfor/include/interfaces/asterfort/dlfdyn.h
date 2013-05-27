        interface
          subroutine dlfdyn(rigid,amort,lamort,neq,d0,v0,f,f0)
            integer :: rigid
            integer :: amort
            logical :: lamort
            integer :: neq
            real(kind=8) :: d0(*)
            real(kind=8) :: v0(*)
            real(kind=8) :: f(*)
            real(kind=8) :: f0(*)
          end subroutine dlfdyn
        end interface
