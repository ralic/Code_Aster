        interface
          subroutine digouj(option,compor,nno,nbt,neq,nc,icodma,dul,&
     &sim,varim,pgl,klv,klc,varip,fono,sip,nomte)
            integer :: neq
            integer :: nbt
            character(len=16) :: option
            character(len=16) :: compor(*)
            integer :: nno
            integer :: nc
            integer :: icodma
            real(kind=8) :: dul(neq)
            real(kind=8) :: sim(neq)
            real(kind=8) :: varim(*)
            real(kind=8) :: pgl(3,3)
            real(kind=8) :: klv(nbt)
            real(kind=8) :: klc(neq,neq)
            real(kind=8) :: varip(*)
            real(kind=8) :: fono(neq)
            real(kind=8) :: sip(neq)
            character(len=16) :: nomte
          end subroutine digouj
        end interface
