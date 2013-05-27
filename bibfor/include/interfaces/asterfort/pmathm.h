        interface
          subroutine pmathm(dimmat,dimdef,dimcon,dimuel,dsde,drds,ck,b&
     &,poids,matri)
            integer :: dimuel
            integer :: dimcon
            integer :: dimdef
            integer :: dimmat
            real(kind=8) :: dsde(dimcon,dimdef)
            real(kind=8) :: drds(dimdef,dimcon)
            real(kind=8) :: ck(dimdef)
            real(kind=8) :: b(dimdef,dimuel)
            real(kind=8) :: poids
            real(kind=8) :: matri(dimmat,dimmat)
          end subroutine pmathm
        end interface
