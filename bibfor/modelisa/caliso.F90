subroutine caliso(chargz)
    implicit none
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! person_in_charge: jacques.pellet at edf.fr
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    include 'jeveux.h'
!
    include 'asterc/getfac.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterc/indik8.h'
    include 'asterc/r8gaem.h'
    include 'asterfort/aflrch.h'
    include 'asterfort/assert.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/drz02d.h'
    include 'asterfort/drz03d.h'
    include 'asterfort/drz12d.h'
    include 'asterfort/drz13d.h'
    include 'asterfort/drzrot.h'
    include 'asterfort/exisdg.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/ltnotb.h'
    include 'asterfort/malino.h'
    include 'asterfort/tbliva.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mess.h'
    character(len=8) :: charge
    character(len=*) :: chargz
! -------------------------------------------------------
!     TRAITEMENT DU MOT CLE LIAISON_SOLIDE DE AFFE_CHAR_MECA
! -------------------------------------------------------
!  CHARGE        - IN    - K8   - : NOM DE LA SD CHARGE
!                - JXVAR -      -   LA  CHARGE EST ENRICHIE
!                                   DES RELATIONS LINEAIRES NECESSAIRES
! -------------------------------------------------------
!
    complex(kind=8) :: cbid
!
! --------- VARIABLES LOCALES ---------------------------
    integer :: i, ibid, icmp, icmp4, icmp5, icmp6, idrxyz, idrz, ier, ierd
    integer :: ilisno, in, inom, iocc, jnoma, jprnm, lonlis, n1, n2
    integer :: narl, nbcmp, nbec, nddla, ndimmo, nliai, nmocl, nrl
    parameter    (nmocl = 300)
    character(len=1) :: k1bid
    character(len=2) :: typlag
    character(len=8) :: mod, nomg, k8bid, poslag
    real(kind=8) :: dmin, armin, x
    character(len=8) :: noma, cmp, nomcmp(nmocl)
    character(len=8) :: cmp4, cmp5, cmp6
    character(len=9) :: nomte
    character(len=19) :: ligrmo, lisrel, nomtab
    character(len=24) :: lisnoe
    integer :: ntypel(nmocl)
    integer :: vali(2)
    integer :: iarg
! --------- FIN  DECLARATIONS  VARIABLES LOCALES --------
!
    call jemarq()
    lisnoe = '&&CALISO.LISTNOE'
    charge = chargz
    typlag = '12'
!
! --- NOM DE LA LISTE DE RELATIONS
    lisrel = '&&CALISO.RLLISTE'
!
    call getfac('LIAISON_SOLIDE', nliai)
    if (nliai .eq. 0) goto 99999
!
! --- MODELE ASSOCIE AU LIGREL DE CHARGE
    call dismoi('F', 'NOM_MODELE', charge(1:8), 'CHARGE', ibid,&
                mod, ier)
!
! ---  LIGREL DU MODELE
    ligrmo = mod(1:8)//'.MODELE'
!
! --- MAILLAGE ASSOCIE AU MODELE
    call jeveuo(ligrmo//'.LGRF', 'L', jnoma)
    noma = zk8(jnoma)
!
!     RECUPERATION DE L'ARETE MIN : ARMIN
    call ltnotb(noma, 'CARA_GEOM', nomtab)
    call tbliva(nomtab, 1, 'APPLAT_Z', ibid, 0.d0,&
                cbid, k1bid, 'ABSO', r8gaem(), 'AR_MIN',&
                k1bid, ibid, armin, cbid, k1bid,&
                ier)
    call assert(armin.gt.0.d0)
!
!
! --- DIMENSION ASSOCIEE AU MODELE
    call dismoi('F', 'DIM_GEOM', mod, 'MODELE', ndimmo,&
                k8bid, ier)
    if (.not.(ndimmo.eq.2.or.ndimmo.eq.3)) call u2mess('F', 'MODELISA2_6')
!
! --- RECUPERATION DES NOMS DES DDLS ET DES NUMEROS
! --- D'ELEMENTS DE LAGRANGE ASSOCIES
!
    nomg = 'DEPL_R'
    nomte = 'D_DEPL_R_'
!
    call jeveuo(jexnom('&CATA.GD.NOMCMP', nomg), 'L', inom)
    call jelira(jexnom('&CATA.GD.NOMCMP', nomg), 'LONMAX', nbcmp, k1bid)
    nddla = nbcmp-1
    if (nddla .gt. nmocl) then
        vali (1) = nmocl
        vali (2) = nddla
        call u2mesg('F', 'MODELISA8_29', 0, ' ', 2,&
                    vali, 0, 0.d0)
    endif
    do 10 i = 1, nddla
        nomcmp(i)=zk8(inom-1+i)
        call jenonu(jexnom('&CATA.TE.NOMTE', nomte//nomcmp(i)(1:7)), ntypel(i))
10  end do
    call dismoi('F', 'NB_EC', nomg, 'GRANDEUR', nbec,&
                k8bid, ierd)
!
! --- ACCES A L'OBJET .PRNM
!
    if (nbec .gt. 10) then
        call u2mess('F', 'MODELISA_94')
    else
        call jeveuo(ligrmo//'.PRNM', 'L', jprnm)
    endif
!
! --- BOUCLE SUR LES OCCURENCES DU MOT-FACTEUR LIAISON_SOLIDE
!
    do 20 iocc = 1, nliai
!
! --- ON REGARDE SI LES MULTIPLICATEURS DE LAGRANGE SONT A METTRE
! --- APRES LES NOEUDS PHYSIQUES LIES PAR LA RELATION DANS LA MATRICE
! --- ASSEMBLEE :
! --- SI OUI TYPLAG = '22'
! --- SI NON TYPLAG = '12'
!
        call getvtx('LIAISON_SOLIDE', 'NUME_LAGR', iocc, iarg, 0,&
                    k8bid, narl)
        call getvr8('LIAISON_SOLIDE', 'DIST_MIN', iocc, iarg, 0,&
                    dmin, n1)
        if (n1 .eq. 0) dmin=armin*1.d-3
!
        if (narl .ne. 0) then
            call getvtx('LIAISON_SOLIDE', 'NUME_LAGR', iocc, iarg, 1,&
                        poslag, nrl)
            if (poslag(1:5) .eq. 'APRES') then
                typlag = '22'
            else
                typlag = '12'
            endif
        else
            typlag = '12'
        endif
!
! --- ACQUISITION DE LA LISTE DES NOEUDS A LIER
! --- (CETTE LISTE EST NON REDONDANTE)
        call malino('LIAISON_SOLIDE', charge, iocc, lisnoe, lonlis)
        call jeveuo(lisnoe, 'L', ilisno)
!
!
! --- SI LES MOTS CLES TRAN OU ANGL_NAUT SONT UTILISES :
        call getvr8('LIAISON_SOLIDE', 'TRAN', iocc, iarg, 0,&
                    x, n1)
        call getvr8('LIAISON_SOLIDE', 'ANGL_NAUT', iocc, iarg, 0,&
                    x, n2)
        if (n1+n2 .lt. 0) then
            call drzrot(lisnoe, lonlis, charge, typlag, lisrel,&
                        iocc, ndimmo)
            goto 9999
        endif
!
!
! --- CAS OU LA LISTE DES NOEUDS A LIER EST UN SINGLETON
        if (lonlis .eq. 1) then
            call u2mess('I', 'MODELISA3_17')
            goto 9999
        endif
!
!
! --- CAS OU LA DIMENSION DU MODELE EST EGALE A 2
        if (ndimmo .eq. 2) then
!
! ---      ON REGARDE S'IL Y A UN NOEUD DE LA LISTE PORTANT LE DDL DRZ
!
            cmp = 'DRZ'
            icmp = indik8(nomcmp,cmp,1,nddla)
            idrz = 0
            do 30 i = 1, lonlis
! ---        NUMERO DU NOEUD COURANT DE LA LISTE
                call jenonu(jexnom(noma//'.NOMNOE', zk8(ilisno+i-1)), in)
                if (exisdg(zi(jprnm-1+(in-1)*nbec+1),icmp)) then
                    idrz = 1
                    goto 40
                endif
30          continue
40          continue
!
! ---      CAS OU L'ON A UN NOEUD DE LA LISTE PORTANT LE DDL DRZ
!
            if (idrz .eq. 1) then
                call drz12d(lisnoe, lonlis, charge, typlag, lisrel)
!
! ---      CAS OU AUCUN NOEUD DE LA LISTE NE PORTE LE DDL DRZ
!
            else if (idrz.eq.0) then
                call drz02d(lisnoe, lonlis, charge, typlag, lisrel,&
                            dmin)
!
! ---      FIN DU CAS 2D SANS DDL DE ROTATION
            endif
!
! --- CAS OU LA DIMENSION DU MODELE EST EGALE A 3
!
        else if (ndimmo.eq.3) then
!
! ---      ON REGARDE S'IL Y A UN NOEUD DE LA LISTE PORTANT LES 3 DDLS
! ---      DE ROTATION
!
            cmp4 = 'DRX'
            cmp5 = 'DRY'
            cmp6 = 'DRZ'
            icmp4 = indik8(nomcmp,cmp4,1,nddla)
            icmp5 = indik8(nomcmp,cmp5,1,nddla)
            icmp6 = indik8(nomcmp,cmp6,1,nddla)
            idrxyz = 0
            do 50 i = 1, lonlis
! ---        NUMERO DU NOEUD COURANT DE LA LISTE
                call jenonu(jexnom(noma//'.NOMNOE', zk8(ilisno+i-1)), in)
!
                if ((exisdg(zi(jprnm-1+(in-1)*nbec+1),icmp4)) .and.&
                    ( exisdg(zi(jprnm-1+(in-1)*nbec+1),icmp5)) .and.&
                    (exisdg( zi(jprnm-1+(in-1)*nbec+1),icmp6))) then
                    idrxyz = 1
                    goto 60
                endif
50          continue
60          continue
!
! ---      CAS OU L'ON A UN NOEUD DE LA LISTE PORTANT LES 3 DDLS
! ---      DE ROTATION
            if (idrxyz .eq. 1) then
                call drz13d(lisnoe, lonlis, charge, typlag, lisrel)
!
! ---      CAS MASSIF (PAS DE COMPOSANTES DE ROTATION)
            else if (idrxyz.eq.0) then
                call drz03d(lisnoe, lonlis, charge, typlag, lisrel,&
                            dmin)
!
! ---      FIN DU CAS 3D MASSIF (IDRXYZ=0)
            endif
! ---    FIN DU CAS 3D
        endif
9999      continue
! ---    DESTRUCTION DE LA LISTE DES NOEUDS A LIER
        call jedetr(lisnoe)
!
! ---       FIN DE LA BOUCLE SUR LES OCCURENCES DU MOT-FACTEUR
! ---       LIAISON_SOLIDE
20  end do
!
!     -- AFFECTATION DE LA LISTE_RELA A LA CHARGE :
!     ---------------------------------------------
    call aflrch(lisrel, charge)
!
99999  continue
    call jedema()
end subroutine
