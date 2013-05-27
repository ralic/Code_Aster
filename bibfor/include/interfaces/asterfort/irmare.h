        interface
          subroutine irmare(ifc,ndim,nno,coordo,nbma,connex,point,noma&
     &,typma,typel,lmod,titre,nbtitr,nbgrn,nbgrm,nomai,nonoe,formar)
            integer :: ifc
            integer :: ndim
            integer :: nno
            real(kind=8) :: coordo(*)
            integer :: nbma
            integer :: connex(*)
            integer :: point(*)
            character(len=8) :: noma
            integer :: typma(*)
            integer :: typel(*)
            logical :: lmod
            character(len=80) :: titre(*)
            integer :: nbtitr
            integer :: nbgrn
            integer :: nbgrm
            character(len=8) :: nomai(*)
            character(len=8) :: nonoe(*)
            character(len=16) :: formar
          end subroutine irmare
        end interface
