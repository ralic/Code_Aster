        interface
          subroutine recupe(noma,ndim,nk1d,lrev,matrev,deklag,prodef,&
     &londef,oridef)
            character(len=8) :: noma
            integer :: ndim
            integer :: nk1d
            real(kind=8) :: lrev
            character(len=8) :: matrev
            real(kind=8) :: deklag
            real(kind=8) :: prodef
            real(kind=8) :: londef
            character(len=8) :: oridef
          end subroutine recupe
        end interface
