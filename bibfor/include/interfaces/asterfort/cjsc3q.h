        interface
          subroutine cjsc3q(sig,x,pa,qinit,q,qii,cos3tq,devnul,trac)
            real(kind=8) :: sig(6)
            real(kind=8) :: x(6)
            real(kind=8) :: pa
            real(kind=8) :: qinit
            real(kind=8) :: q(6)
            real(kind=8) :: qii
            real(kind=8) :: cos3tq
            logical :: devnul
            logical :: trac
          end subroutine cjsc3q
        end interface
