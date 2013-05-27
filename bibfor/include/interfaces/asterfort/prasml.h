        interface
          subroutine prasml(option,nugene,tminbl,nomprn,modgen,tmnobl,&
     &tmadbl,knombl,inumbl,conleq,conlbl)
            character(len=11) :: option
            character(len=14) :: nugene
            character(len=24) :: tminbl
            character(len=8) :: nomprn
            character(len=8) :: modgen
            character(len=24) :: tmnobl
            character(len=24) :: tmadbl
            character(len=24) :: knombl(*)
            integer :: inumbl(*)
            real(kind=8) :: conleq(*)
            real(kind=8) :: conlbl(*)
          end subroutine prasml
        end interface
