        interface
          subroutine irdepl(chamno,partie,ifi,form,titre,nomsd,nomsym,&
     &numord,lcor,nbnot,numnoe,nbcmp,nomcmp,lsup,borsup,linf,borinf,lmax&
     &,lmin,lresu,formr,nive)
            character(*) :: chamno
            character(*) :: partie
            integer :: ifi
            character(*) :: form
            character(*) :: titre
            character(*) :: nomsd
            character(*) :: nomsym
            integer :: numord
            logical :: lcor
            integer :: nbnot
            integer :: numnoe(*)
            integer :: nbcmp
            character(*) :: nomcmp(*)
            logical :: lsup
            real(kind=8) :: borsup
            logical :: linf
            real(kind=8) :: borinf
            logical :: lmax
            logical :: lmin
            logical :: lresu
            character(*) :: formr
            integer :: nive
          end subroutine irdepl
        end interface
