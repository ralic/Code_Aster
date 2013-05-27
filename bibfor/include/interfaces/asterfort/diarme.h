        interface
          subroutine diarme(nbt,neq,icodma,ul,dul,utl,sim,varim,klv,&
     &varip,kty2,duly)
            integer :: neq
            integer :: nbt
            integer :: icodma
            real(kind=8) :: ul(neq)
            real(kind=8) :: dul(neq)
            real(kind=8) :: utl(neq)
            real(kind=8) :: sim(neq)
            real(kind=8) :: varim
            real(kind=8) :: klv(nbt)
            real(kind=8) :: varip
            real(kind=8) :: kty2
            real(kind=8) :: duly
          end subroutine diarme
        end interface
