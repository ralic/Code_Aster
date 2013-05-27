        interface
          subroutine dichoc(nbt,neq,nno,nc,icodma,dul,utl,xg,pgl,klv,&
     &duly,dvl,dpe,dve,force,varmo,varpl,dimele)
            integer :: neq
            integer :: nbt
            integer :: nno
            integer :: nc
            integer :: icodma
            real(kind=8) :: dul(neq)
            real(kind=8) :: utl(neq)
            real(kind=8) :: xg(6)
            real(kind=8) :: pgl(3,3)
            real(kind=8) :: klv(nbt)
            real(kind=8) :: duly
            real(kind=8) :: dvl(neq)
            real(kind=8) :: dpe(neq)
            real(kind=8) :: dve(neq)
            real(kind=8) :: force(3)
            real(kind=8) :: varmo(8)
            real(kind=8) :: varpl(8)
            integer :: dimele
          end subroutine dichoc
        end interface
