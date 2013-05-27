        interface
          subroutine irch19(cham19,partie,form,ifi,titre,nomsd,nomsym,&
     &numord,lcor,nbnot,numnoe,nbmat,nummai,nbcmp,nomcmp,lsup,borsup,&
     &linf,borinf,lmax,lmin,lresu,formr,nive)
            integer :: nbcmp
            character(*) :: cham19
            character(*) :: partie
            character(*) :: form
            integer :: ifi
            character(*) :: titre
            character(*) :: nomsd
            character(*) :: nomsym
            integer :: numord
            logical :: lcor
            integer :: nbnot
            integer :: numnoe(*)
            integer :: nbmat
            integer :: nummai(*)
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
          end subroutine irch19
        end interface
