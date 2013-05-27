        interface
          subroutine irchml(chamel,partie,ifi,form,titre,loc,nomsd,&
     &nomsym,numord,lcor,nbnot,numnoe,nbmat,nummai,nbcmp,nomcmp,lsup,&
     &borsup,linf,borinf,lmax,lmin,lresu,formr,ncmp,nucmp,nive)
            character(*) :: chamel
            character(*) :: partie
            integer :: ifi
            character(*) :: form
            character(*) :: titre
            character(*) :: loc
            character(*) :: nomsd
            character(*) :: nomsym
            integer :: numord
            logical :: lcor
            integer :: nbnot
            integer :: numnoe(*)
            integer :: nbmat
            integer :: nummai(*)
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
            integer :: ncmp
            integer :: nucmp(*)
            integer :: nive
          end subroutine irchml
        end interface
