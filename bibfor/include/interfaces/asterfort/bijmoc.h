        interface
          subroutine bijmoc(umoy,geom,cf0,mcf0,fsvr,imod,jmod,nbm,&
     &vicoq,torco,tcoef,s1,s2,bij)
            integer :: nbm
            real(kind=8) :: umoy
            real(kind=8) :: geom(9)
            real(kind=8) :: cf0
            real(kind=8) :: mcf0
            real(kind=8) :: fsvr(7)
            integer :: imod
            integer :: jmod
            integer :: vicoq(nbm)
            real(kind=8) :: torco(4,nbm)
            real(kind=8) :: tcoef(10,nbm)
            real(kind=8) :: s1
            real(kind=8) :: s2
            complex(kind=8) :: bij
          end subroutine bijmoc
        end interface
