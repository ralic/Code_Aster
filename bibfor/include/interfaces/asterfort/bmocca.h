        interface
          subroutine bmocca(umoy,geom,cf0,mcf0,fsvr,nbm,vicoq,torco,&
     &tcoef,s1,s2,b)
            integer :: nbm
            real(kind=8) :: umoy
            real(kind=8) :: geom(9)
            real(kind=8) :: cf0
            real(kind=8) :: mcf0
            real(kind=8) :: fsvr(7)
            integer :: vicoq(nbm)
            real(kind=8) :: torco(4,nbm)
            real(kind=8) :: tcoef(10,nbm)
            real(kind=8) :: s1
            real(kind=8) :: s2
            complex(kind=8) :: b(nbm,nbm)
          end subroutine bmocca
        end interface
