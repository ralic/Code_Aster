        interface
          subroutine dicrgr(fami,option,neq,nc,icodma,ulm,dul,sim,&
     &varim,pgl,klv,varip,fono,sip)
            integer :: neq
            character(*) :: fami
            character(len=16) :: option
            integer :: nc
            integer :: icodma
            real(kind=8) :: ulm(neq)
            real(kind=8) :: dul(neq)
            real(kind=8) :: sim(neq)
            real(kind=8) :: varim(6)
            real(kind=8) :: pgl(3,3)
            real(kind=8) :: klv(78)
            real(kind=8) :: varip(6)
            real(kind=8) :: fono(neq)
            real(kind=8) :: sip(neq)
          end subroutine dicrgr
        end interface
