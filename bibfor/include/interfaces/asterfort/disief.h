        interface
          subroutine disief(nbt,neq,nno,nc,pgl,klv,dul,sim,ilogic,duly&
     &,sip,fono,force,dimele)
            integer :: neq
            integer :: nbt
            integer :: nno
            integer :: nc
            real(kind=8) :: pgl(3,3)
            real(kind=8) :: klv(nbt)
            real(kind=8) :: dul(neq)
            real(kind=8) :: sim(neq)
            integer :: ilogic
            real(kind=8) :: duly
            real(kind=8) :: sip(neq)
            real(kind=8) :: fono(neq)
            real(kind=8) :: force(3)
            integer :: dimele
          end subroutine disief
        end interface
