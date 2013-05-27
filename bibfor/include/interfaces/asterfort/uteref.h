        interface
          subroutine uteref(chanom,typech,tyelas,nomte,nomfpg,nnos,nno&
     &,nbpg,ndim,refcoo,gscoo,wg,codret)
            character(len=19) :: chanom
            character(len=8) :: typech
            integer :: tyelas
            character(len=16) :: nomte
            character(len=16) :: nomfpg
            integer :: nnos
            integer :: nno
            integer :: nbpg
            integer :: ndim
            real(kind=8) :: refcoo(*)
            real(kind=8) :: gscoo(*)
            real(kind=8) :: wg(*)
            integer :: codret
          end subroutine uteref
        end interface
