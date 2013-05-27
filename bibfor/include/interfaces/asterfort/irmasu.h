        interface
          subroutine irmasu(ifc,ndim,nno,coordo,nbma,connex,point,&
     &typma,typel,codgra,codphy,codphd,permut,maxnod,lmod,noma,nbgrn,&
     &nogn,nbgrm,nogm,lmasu,nomai,nonoe,versio)
            integer :: maxnod
            integer :: ifc
            integer :: ndim
            integer :: nno
            real(kind=8) :: coordo(*)
            integer :: nbma
            integer :: connex(*)
            integer :: point(*)
            integer :: typma(*)
            integer :: typel(*)
            integer :: codgra(*)
            integer :: codphy(*)
            integer :: codphd(*)
            integer :: permut(maxnod,*)
            logical :: lmod
            character(len=8) :: noma
            integer :: nbgrn
            character(len=24) :: nogn(*)
            integer :: nbgrm
            character(len=24) :: nogm(*)
            logical :: lmasu
            character(len=8) :: nomai(*)
            character(len=8) :: nonoe(*)
            integer :: versio
          end subroutine irmasu
        end interface
