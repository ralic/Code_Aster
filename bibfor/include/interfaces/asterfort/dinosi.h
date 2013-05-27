        interface
          subroutine dinosi(nbt,neq,nno,nc,pgl,klv,dul,sim,sip,fono,&
     &numloi,varint)
            integer :: neq
            integer :: nbt
            integer :: nno
            integer :: nc
            real(kind=8) :: pgl(3,3)
            real(kind=8) :: klv(nbt)
            real(kind=8) :: dul(neq)
            real(kind=8) :: sim(neq)
            real(kind=8) :: sip(neq)
            real(kind=8) :: fono(neq)
            integer :: numloi
            real(kind=8) :: varint(*)
          end subroutine dinosi
        end interface
