        interface
          subroutine dicorn(irmetg,nbt,neq,iterat,icodma,ul,dul,utl,&
     &sim,varim,klv,klv2,varip)
            integer :: neq
            integer :: nbt
            integer :: irmetg
            integer :: iterat
            integer :: icodma
            real(kind=8) :: ul(neq)
            real(kind=8) :: dul(neq)
            real(kind=8) :: utl(neq)
            real(kind=8) :: sim(neq)
            real(kind=8) :: varim(7)
            real(kind=8) :: klv(nbt)
            real(kind=8) :: klv2(nbt)
            real(kind=8) :: varip(7)
          end subroutine dicorn
        end interface
