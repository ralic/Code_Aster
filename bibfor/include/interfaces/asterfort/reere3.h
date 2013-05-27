        interface
          subroutine reere3(elrefp,nnop,igeom,xg,depl,grand,ndim,he,&
     &fisno,nfiss,nfh,nfe,ddlt,fe,dgdgl,cinem,xe,ff,dfdi,f,eps,grad)
            integer :: ddlt
            integer :: nfiss
            integer :: ndim
            integer :: nnop
            character(len=8) :: elrefp
            integer :: igeom
            real(kind=8) :: xg(ndim)
            real(kind=8) :: depl(ddlt,nnop)
            logical :: grand
            real(kind=8) :: he(nfiss)
            integer :: fisno(nnop,nfiss)
            integer :: nfh
            integer :: nfe
            real(kind=8) :: fe(4)
            real(kind=8) :: dgdgl(4,ndim)
            character(len=3) :: cinem
            real(kind=8) :: xe(ndim)
            real(kind=8) :: ff(nnop)
            real(kind=8) :: dfdi(nnop,ndim)
            real(kind=8) :: f(3,3)
            real(kind=8) :: eps(6)
            real(kind=8) :: grad(ndim,ndim)
          end subroutine reere3
        end interface
