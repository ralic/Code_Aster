        interface
          subroutine irmaca(ifc,ndim,nno,coordo,nbma,connex,point,noma&
     &,typma,lmod,nbgrn,nogn,nbgrm,nogm,nive)
            integer :: ifc
            integer :: ndim
            integer :: nno
            real(kind=8) :: coordo(*)
            integer :: nbma
            integer :: connex(*)
            integer :: point(*)
            character(len=8) :: noma
            integer :: typma(*)
            logical :: lmod
            integer :: nbgrn
            character(len=24) :: nogn(*)
            integer :: nbgrm
            character(len=24) :: nogm(*)
            integer :: nive
          end subroutine irmaca
        end interface
