        interface
          subroutine irmhdf(ifi,ndim,nbnoeu,coordo,nbmail,connex,point&
     &,nomast,typma,titre,nbtitr,nbgrno,nomgno,nbgrma,nomgma,nommai,&
     &nomnoe,infmed)
            integer :: ifi
            integer :: ndim
            integer :: nbnoeu
            real(kind=8) :: coordo(*)
            integer :: nbmail
            integer :: connex(*)
            integer :: point(*)
            character(len=8) :: nomast
            integer :: typma(*)
            character(len=80) :: titre(*)
            integer :: nbtitr
            integer :: nbgrno
            character(len=24) :: nomgno(*)
            integer :: nbgrma
            character(len=24) :: nomgma(*)
            character(len=8) :: nommai(*)
            character(len=8) :: nomnoe(*)
            integer :: infmed
          end subroutine irmhdf
        end interface
