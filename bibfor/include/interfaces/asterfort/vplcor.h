        interface
          subroutine vplcor(ldynam,neq,nbvect,nborto,prorto,signes,&
     &vect,ivecp,pkx,plx)
            integer :: nbvect
            integer :: neq
            integer :: ldynam
            integer :: nborto
            real(kind=8) :: prorto
            real(kind=8) :: signes(nbvect)
            real(kind=8) :: vect(neq,nbvect)
            integer :: ivecp
            real(kind=8) :: pkx(neq,nbvect)
            real(kind=8) :: plx(neq)
          end subroutine vplcor
        end interface
