        interface
          subroutine irecri(nomcon,form,ifi,titre,lgmsh,nbcham,cham,&
     &partie,nbpara,para,nbordr,ordr,lresu,motfac,iocc,cecr,tycha,lcor,&
     &nbnot,numnoe,nbmat,nummai,nbcmp,nomcmp,lsup,borsup,linf,borinf,&
     &lmax,lmin,formr,nive,versio)
            character(*) :: nomcon
            character(*) :: form
            integer :: ifi
            character(*) :: titre
            logical :: lgmsh
            integer :: nbcham
            character(*) :: cham(*)
            character(*) :: partie
            integer :: nbpara
            character(*) :: para(*)
            integer :: nbordr
            integer :: ordr(*)
            logical :: lresu
            character(*) :: motfac
            integer :: iocc
            character(*) :: cecr
            character(len=8) :: tycha
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
            character(*) :: formr
            integer :: nive
            integer :: versio
          end subroutine irecri
        end interface
