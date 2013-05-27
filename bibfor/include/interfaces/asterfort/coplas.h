        interface
          subroutine coplas(tempa,k1a,k1b,matrev,lrev,deklag,prodef,&
     &oridef,kal,kbl,dkma,dkmb,k1acp,k1bcp)
            real(kind=8) :: tempa
            real(kind=8) :: k1a
            real(kind=8) :: k1b
            character(len=8) :: matrev
            real(kind=8) :: lrev
            real(kind=8) :: deklag
            real(kind=8) :: prodef
            character(len=8) :: oridef
            real(kind=8) :: kal
            real(kind=8) :: kbl
            real(kind=8) :: dkma
            real(kind=8) :: dkmb
            real(kind=8) :: k1acp
            real(kind=8) :: k1bcp
          end subroutine coplas
        end interface
