        interface
          subroutine coqrep(pgl,alpha,beta,t2ev,t2ve,c,s)
            real(kind=8) :: pgl(3,3)
            real(kind=8) :: alpha
            real(kind=8) :: beta
            real(kind=8) :: t2ev(*)
            real(kind=8) :: t2ve(*)
            real(kind=8) :: c
            real(kind=8) :: s
          end subroutine coqrep
        end interface
