        interface
          subroutine xmmsa1(algofr,ndim,nno,nnos,nnol,pla,ffc,ffp,&
     &idepd,idepm,nfh,nd,tau1,tau2,singu,rr,lact,ddls,ddlm,coeffr,coeffp&
     &,p,adher,knp,ptknp,ik)
            integer :: algofr
            integer :: ndim
            integer :: nno
            integer :: nnos
            integer :: nnol
            integer :: pla(27)
            real(kind=8) :: ffc(8)
            real(kind=8) :: ffp(27)
            integer :: idepd
            integer :: idepm
            integer :: nfh
            real(kind=8) :: nd(3)
            real(kind=8) :: tau1(3)
            real(kind=8) :: tau2(3)
            integer :: singu
            real(kind=8) :: rr
            integer :: lact(8)
            integer :: ddls
            integer :: ddlm
            real(kind=8) :: coeffr
            real(kind=8) :: coeffp
            real(kind=8) :: p(3,3)
            logical :: adher
            real(kind=8) :: knp(3,3)
            real(kind=8) :: ptknp(3,3)
            real(kind=8) :: ik(3,3)
          end subroutine xmmsa1
        end interface
