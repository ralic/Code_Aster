        interface
          subroutine reeref(elrefp,axi,nnop,nnos,geom,xg,idepl,grand,&
     &ndim,he,r,ur,fisno,nfiss,nfh,nfe,ddls,ddlm,fe,dgdgl,cinem,xe,ff,&
     &dfdi,f,eps,grad)
            integer :: nfiss
            integer :: ndim
            integer :: nnop
            character(len=8) :: elrefp
            logical :: axi
            integer :: nnos
            real(kind=8) :: geom(*)
            real(kind=8) :: xg(ndim)
            integer :: idepl
            logical :: grand
            real(kind=8) :: he(nfiss)
            real(kind=8) :: r
            real(kind=8) :: ur
            integer :: fisno(nnop,nfiss)
            integer :: nfh
            integer :: nfe
            integer :: ddls
            integer :: ddlm
            real(kind=8) :: fe(4)
            real(kind=8) :: dgdgl(4,ndim)
            character(len=3) :: cinem
            real(kind=8) :: xe(ndim)
            real(kind=8) :: ff(nnop)
            real(kind=8) :: dfdi(nnop,ndim)
            real(kind=8) :: f(3,3)
            real(kind=8) :: eps(6)
            real(kind=8) :: grad(ndim,ndim)
          end subroutine reeref
        end interface
