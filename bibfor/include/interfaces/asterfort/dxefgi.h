        interface
          subroutine dxefgi(nomte,xyzl,pgl,epsini,sigt)
            character(len=16) :: nomte
            real(kind=8) :: xyzl(3,1)
            real(kind=8) :: pgl(3,3)
            real(kind=8) :: epsini(6)
            real(kind=8) :: sigt(1)
          end subroutine dxefgi
        end interface
