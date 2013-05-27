        interface
          subroutine rc32sa(typz,nommat,mati,matj,snpq,spij,typeke,&
     &spmeca,spther,kemeca,kether,saltij,sm,fuij)
            character(*) :: typz
            character(len=8) :: nommat
            real(kind=8) :: mati(*)
            real(kind=8) :: matj(*)
            real(kind=8) :: snpq
            real(kind=8) :: spij(2)
            real(kind=8) :: typeke
            real(kind=8) :: spmeca(2)
            real(kind=8) :: spther(2)
            real(kind=8) :: kemeca
            real(kind=8) :: kether
            real(kind=8) :: saltij(2)
            real(kind=8) :: sm
            real(kind=8) :: fuij(2)
          end subroutine rc32sa
        end interface
