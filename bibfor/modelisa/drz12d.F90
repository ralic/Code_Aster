subroutine drz12d(lisnoz, lonlis, chargz, typlaz, lisrez)
    implicit none
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
    include 'asterc/getres.h'
    include 'asterc/indik8.h'
    include 'asterfort/afrela.h'
    include 'asterfort/assert.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/exisdg.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    character(len=8) :: charge
    character(len=19) :: lisrel
    character(len=24) :: lisnoe
    character(len=*) :: chargz, lisnoz, typlaz, lisrez
! -------------------------------------------------------
!     BLOCAGE DES DEPLACEMENTS RELATIFS D'UNE LISTE DE NOEUDS
!     SPECIFIEE PAR L'UTILISATEUR DANS LE CAS OU L' ON EST
!     EN 2D ET AU-MOINS L'UN DES NOEUDS PORTE LE DDL DRZ
! -------------------------------------------------------
!  LISNOE        - IN    - K24 - : NOM DE LA LISTE DES
!                -       -     -   NOEUDS A LIER
! -------------------------------------------------------
!  LONLIS        - IN    - I   - : LONGUEUR DE LA LISTE DES
!                -       -     -   NOEUDS A LIER
! -------------------------------------------------------
!  CHARGE        - IN    - K8  - : NOM DE LA SD CHARGE
!                - JXIN  -     -
! -------------------------------------------------------
! TYPLAG         - IN    - K2  - : TYPE DES MULTIPLICATEURS DE LAGRANGE
!                                  ASSOCIES A LA RELATION :
!                              SI = '12'  LE PREMIER LAGRANGE EST AVANT
!                                         LE NOEUD PHYSIQUE
!                                         LE SECOND LAGRANGE EST APRES
!                              SI = '22'  LE PREMIER LAGRANGE EST APRES
!                                         LE NOEUD PHYSIQUE
!                                         LE SECOND LAGRANGE EST APRES
! -------------------------------------------------------
!  LISREL        - IN    - K19 - : NOM DE LA SD
!                - JXVAR -     -   LISTE DE RELATIONS
! -------------------------------------------------------
!
!
! --------- VARIABLES LOCALES ---------------------------
!-----------------------------------------------------------------------
    integer :: i, ibid, icmp, ier, ierd, ilisno, in
    integer :: ino, inom, jcoor, jliscc, jliscr, jlisdi, jlisdl
    integer :: jlisdm, jlisno, jnoma, jprnm, lonlis, nbcmp, nbec
    integer :: nbterm, nddla, nmocl
    real(kind=8) :: beta, un, x, y
!-----------------------------------------------------------------------
    parameter (nmocl=300)
    complex(kind=8) :: betac
    character(len=2) :: typlag
    character(len=4) :: typval, typcoe
    character(len=8) :: betaf, resu
    character(len=8) :: mod, nomg, nomnoe, k8bid
    character(len=8) :: noma, cmp, nomcmp(nmocl)
    character(len=9) :: nomte
    character(len=16) :: type, oper
    character(len=19) :: ligrmo
    integer :: ntypel(nmocl)
    integer :: vali(2)
    character(len=1) :: k1bid
! --------- FIN  DECLARATIONS  VARIABLES LOCALES --------
    call jemarq()
    call getres(resu, type, oper)
    lisrel = lisrez
    charge = chargz
    typlag = typlaz
    lisnoe = lisnoz
!
! --- INITIALISATIONS
!
    betaf = '&FOZERO'
    beta = 0.0d0
    betac = (0.0d0,0.0d0)
    un = 1.0d0
!
! --- MODELE ASSOCIE AU LIGREL DE CHARGE
!
    call dismoi('F', 'NOM_MODELE', charge(1:8), 'CHARGE', ibid,&
                mod, ier)
!
! ---  LIGREL DU MODELE
!
    ligrmo = mod(1:8)//'.MODELE'
!
! --- MAILLAGE ASSOCIE AU MODELE
!
    call jeveuo(ligrmo//'.LGRF', 'L', jnoma)
    noma = zk8(jnoma)
!
! --- TYPE DES VALEURS DES COEFFICIENTS DES RELATIONS
!
    typcoe = 'REEL'
!
! --- TYPE DES VALEURS AU SECOND MEMBRE DES RELATIONS
!
    if (oper(15:16) .eq. '_F') then
        typval = 'FONC'
    else if (oper(15:16).eq.'_C') then
        typval = 'COMP'
    else if (oper(15:16).eq.'  ') then
        typval = 'REEL'
    else
        call assert(.false.)
    endif
!
! --- RECUPERATION DES NOMS DES DDLS ET DES NUMEROS
! --- D'ELEMENTS DE LAGRANGE ASSOCIES
!
    nomg = 'DEPL_R'
    nomte = 'D_DEPL_R_'
!
    call jeveuo(jexnom('&CATA.GD.NOMCMP', nomg), 'L', inom)
    call jelira(jexnom('&CATA.GD.NOMCMP', nomg), 'LONMAX', nbcmp, k1bid)
    nddla = nbcmp - 1
    if (nddla .gt. nmocl) then
        vali (1) = nmocl
        vali (2) = nddla
        call u2mesg('F', 'MODELISA8_29', 0, ' ', 2,&
                    vali, 0, 0.d0)
    endif
    do 10 i = 1, nddla
        nomcmp(i) = zk8(inom-1+i)
        call jenonu(jexnom('&CATA.TE.NOMTE', nomte//nomcmp(i) (1:7)), ntypel(i))
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
! --- CREATION DES TABLEAUX DE TRAVAIL NECESSAIRES A L'AFFECTATION
! --- DE LISREL
!
! ---  MAJORANT DU NOMBRE DE TERMES DANS UNE RELATION
    nbterm = 12
! ---  VECTEUR DU NOM DES NOEUDS
    call wkvect('&&DRZ12D.LISNO', 'V V K8', nbterm, jlisno)
! ---  VECTEUR DU NOM DES DDLS
    call wkvect('&&DRZ12D.LISDDL', 'V V K8', nbterm, jlisdl)
! ---  VECTEUR DES COEFFICIENTS REELS
    call wkvect('&&DRZ12D.COER', 'V V R', nbterm, jliscr)
! ---  VECTEUR DES COEFFICIENTS COMPLEXES
    call wkvect('&&DRZ12D.COEC', 'V V C', nbterm, jliscc)
! ---  VECTEUR DES DIRECTIONS DES DDLS A CONTRAINDRE
    call wkvect('&&DRZ12D.DIRECT', 'V V R', 3*nbterm, jlisdi)
! ---  VECTEUR DES DIMENSIONS DE CES DIRECTIONS
    call wkvect('&&DRZ12D.DIME', 'V V I', nbterm, jlisdm)
!
! --- RECUPERATION DU TABLEAU DES COORDONNEES
!
    call jeveuo(noma//'.COORDO    .VALE', 'L', jcoor)
!
! --- ACQUISITION DE LA LISTE DES NOEUDS A LIER
! --- (CETTE LISTE EST NON REDONDANTE)
!
    call jeveuo(lisnoe, 'L', ilisno)
!
!
! ---      ON REGARDE S'IL Y A UN NOEUD DE LA LISTE PORTANT LE DDL DRZ
!
    cmp = 'DRZ'
    do 20 i = 1, lonlis
! ---        NUMERO DU NOEUD COURANT DE LA LISTE
        call jenonu(jexnom(noma//'.NOMNOE', zk8(ilisno+i-1)), in)
!
        icmp = indik8(nomcmp,cmp,1,nddla)
        if (exisdg(zi(jprnm-1+ (in-1)*nbec+1),icmp)) then
            nomnoe = zk8(ilisno+i-1)
            ino = in
            goto 30
        endif
20  end do
!
    call u2mess('F', 'MODELISA4_43')
!
30  continue
!
    zk8(jlisno+2-1) = nomnoe
    zk8(jlisno+3-1) = nomnoe
!
    do 40 i = 1, lonlis
        if (zk8(ilisno+i-1) .eq. nomnoe) goto 40
        call jenonu(jexnom(noma//'.NOMNOE', zk8(ilisno+i-1)), in)
        x = zr(jcoor-1+3* (in-1)+1) - zr(jcoor-1+3* (ino-1)+1)
        y = zr(jcoor-1+3* (in-1)+2) - zr(jcoor-1+3* (ino-1)+2)
!
! ---    PREMIERE RELATION
! ---    DX(M) - DX(A) + Y*DRZ(A) = 0
!
        nbterm = 3
!
        zk8(jlisno+1-1) = zk8(ilisno+i-1)
!
        zk8(jlisdl+1-1) = 'DX'
        zk8(jlisdl+2-1) = 'DX'
        zk8(jlisdl+3-1) = 'DRZ'
!
        zr(jliscr+1-1) = un
        zr(jliscr+2-1) = -un
        zr(jliscr+3-1) = y
!
        call afrela(zr(jliscr), zc(jliscc), zk8(jlisdl), zk8(jlisno), zi(jlisdm),&
                    zr(jlisdi), nbterm, beta, betac, betaf,&
                    typcoe, typval, typlag, 0.d0, lisrel)
!
! ---    DEUXIEME RELATION
! ---    DY(M) - DY(A) - X*DRZ(A) = 0
!
        nbterm = 3
!
        zk8(jlisno+1-1) = zk8(ilisno+i-1)
!
        zk8(jlisdl+1-1) = 'DY'
        zk8(jlisdl+2-1) = 'DY'
        zk8(jlisdl+3-1) = 'DRZ'
!
        zr(jliscr+1-1) = un
        zr(jliscr+2-1) = -un
        zr(jliscr+3-1) = -x
!
        call afrela(zr(jliscr), zc(jliscc), zk8(jlisdl), zk8(jlisno), zi(jlisdm),&
                    zr(jlisdi), nbterm, beta, betac, betaf,&
                    typcoe, typval, typlag, 0.d0, lisrel)
!
! ---    TROISIEME RELATION SI LE NOEUD COURANT PORTE LE DDL DRZ
! ---    DRZ(M) - DRZ(A)  = 0
!
        icmp = indik8(nomcmp,cmp,1,nddla)
        if (exisdg(zi(jprnm-1+ (in-1)*nbec+1),icmp)) then
!
            nbterm = 2
!
            zk8(jlisno+1-1) = zk8(ilisno+i-1)
            zk8(jlisno+2-1) = nomnoe
!
            zk8(jlisdl+1-1) = 'DRZ'
            zk8(jlisdl+2-1) = 'DRZ'
!
            zr(jliscr+1-1) = un
            zr(jliscr+2-1) = -un
!
            call afrela(zr(jliscr), zc(jliscc), zk8(jlisdl), zk8(jlisno), zi(jlisdm),&
                        zr(jlisdi), nbterm, beta, betac, betaf,&
                        typcoe, typval, typlag, 0.d0, lisrel)
        endif
40  end do
!
! --- DESTRUCTION DES OBJETS DE TRAVAIL
!
    call jedetr('&&DRZ12D.LISNO')
    call jedetr('&&DRZ12D.LISDDL')
    call jedetr('&&DRZ12D.COER')
    call jedetr('&&DRZ12D.COEC')
    call jedetr('&&DRZ12D.DIRECT')
    call jedetr('&&DRZ12D.DIME')
!
    call jedema()
end subroutine
