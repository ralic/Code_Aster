        interface
          subroutine xtailm(ndim,vecdir,numa,typma,jcoor,jconx1,jconx2&
     &,ipt,jtail)
            integer :: ndim
            real(kind=8) :: vecdir(ndim)
            integer :: numa
            character(len=8) :: typma
            integer :: jcoor
            integer :: jconx1
            integer :: jconx2
            integer :: ipt
            integer :: jtail
          end subroutine xtailm
        end interface
