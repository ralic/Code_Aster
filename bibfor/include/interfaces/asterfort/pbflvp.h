        interface
          subroutine pbflvp(umoy,hmoy,rmoy,cf0,mcf0,rkip,s1,s2,lambda)
            real(kind=8) :: umoy
            real(kind=8) :: hmoy
            real(kind=8) :: rmoy
            real(kind=8) :: cf0
            real(kind=8) :: mcf0
            real(kind=8) :: rkip
            real(kind=8) :: s1
            real(kind=8) :: s2
            complex(kind=8) :: lambda(3)
          end subroutine pbflvp
        end interface
