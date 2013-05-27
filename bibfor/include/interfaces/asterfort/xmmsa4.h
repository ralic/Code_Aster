        interface
          subroutine xmmsa4(ndim,nno,nnos,ffp,nddl,nvec,v1,v2,v3,nfh,&
     &singu,rr,ddls,ddlm,saut)
            integer :: nddl
            integer :: ndim
            integer :: nno
            integer :: nnos
            real(kind=8) :: ffp(27)
            integer :: nvec
            real(kind=8) :: v1(nddl)
            real(kind=8) :: v2(*)
            real(kind=8) :: v3(*)
            integer :: nfh
            integer :: singu
            real(kind=8) :: rr
            integer :: ddls
            integer :: ddlm
            real(kind=8) :: saut(3)
          end subroutine xmmsa4
        end interface
